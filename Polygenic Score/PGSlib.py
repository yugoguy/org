#!/usr/bin/env python
# coding: utf-8

# In[ ]:

def cd(usb):
    if usb == 0:
        get_ipython().run_line_magic('cd', '')
    elif usb == 1:
        get_ipython().run_line_magic('cd', '/Volumes/GENDATAENV1/')
    elif usb == 2:
        get_ipython().run_line_magic('cd', '/Volumes/GENDATAENV2/')
    elif usb == 3:
        get_ipython().run_line_magic('cd', '/Volumes/GENDATAENV3/')
        
        
class GWAS:
    def __init__(self, raw_file_path: str):
        self.raw_file_path = raw_file_path
    
    def info(self, header="infer", nrows=10, compression="infer"):
        head = pd.read_table(self.raw_file_path, nrows=nrows, header=header, compression=compression)
        print(head.columns)
        print(head.isna().sum())
        return head
        
    def load(self, header="infer", use_cols=None, dropNA=True, dropDUP=True, dropDUPcol=None):
        self.data = pd.read_table(raw_file_path, header=header, use_cols=use_cols).dropna().drop_duplicates(subset=dropDUPcol, inplace=True)

    def setcols(self, hm=True, default=False, mapper_change=None):
        if default:
            if hm:
                mapper = {"hm_chrom":"chrom", 
                          "hm_pos":"pos", 
                          "hm_beta":"beta", 
                          "hm_effect_allele_frequency": "eaf",
                          "hm_effect_allele": "eff",
                          "p_value": "pval",
                          "hm_other_allele": "non-eff",
                          "standard_error": "std"}
        self.data.rename(mapper) 
    
    def chrom_to_string(self, chrom_col="chrom"):
        if "chr" not in str(self.data[chrom_col][0]):
            string_chrom = self.data[chrom_col].apply(lambda x: "chr"+str(x).split(".")[0].split("_")[0])
            self.data[chrom_col] = string_chrom

    def add_ref(self, fasta_path="/Volumes/GENDATAENV1/GRCh38/GCF_000001405.40_GRCh38.p14_genomic.fna", 
                GRCh38_dict = {"chr1": "NC_000001.11",
                              "chr2": "NC_000002.12",
                              "chr3": "NC_000003.12",
                              "chr4": "NC_000004.12",
                              "chr5": "NC_000005.10",
                              "chr6": "NC_000006.12",
                              "chr7": "NC_000007.14",
                              "chr8": "NC_000008.11",
                              "chr9": "NC_000009.12",
                              "chr10": "NC_000010.11",
                              "chr11": "NC_000011.10",
                              "chr12": "NC_000012.12",
                              "chr13": "NC_000013.11",
                              "chr14": "NC_000014.9",
                              "chr15": "NC_000015.10",
                              "chr16": "NC_000016.10",
                              "chr17": "NC_000017.11",
                              "chr18": "NC_000018.10",
                              "chr19": "NC_000019.10",
                              "chr20": "NC_000020.11",
                              "chr21": "NC_000021.9",
                              "chr22": "NC_000022.11",
                              "chrX": "NC_000023.11",
                              "chrY": "NC_000024.10",
                              "chrMT": "NC_012920.1"}):
        fna = pysam.FastaFile(fasta_path)
        refs = []
        for index, row in self.data.iterrows():
            chrom = row["chrom"]
            pos = row["pos"]
            ref = fna.fetch(GRCh38_dict[chrom], pos-1, pos)[0]
            refs.append(ref.upper())
        self.data["ref"] = refs
    
    def df(self):
        return self.data
    
    def add_maf(self, eafcol="eaf"):
        mafs = []
        for snp in self.data:
            maf = min([snp["eaf"], abs(1-snp["eaf"])])
            mafs.append(maf)
        self.data["maf"] = mafs
        
    def flip_effect(self):
        for index, row in self.data.iterrows():
            if row["eff"] == row["ref"]:
                self.data.iloc[index]["eff"] = self.data.iloc[index]["non-eff"]
                self.data.iloc[index]["beta"] = -1*self.data.iloc[index]["beta"]
    
    
class LDdata:
    def __init__(self, raw_file_path_dict=None, default=False):
        if default:
            raw_file_path_dict = {}
            for chrom in [f"chr{i}" for i in range(1, 12)]+["chrM"]:
                raw_file_path_dict[chrom] = f"/Volumes/GENDATAENV2/tommo_cooccurrence/tommo-54kjpn-20230828-GRCh38-autosome-{chrom}-plink-r2.tsv.gz"
            for chrom in [f"chr{i}" for i in range(12, 23)]+["chrXY"]:
                raw_file_path_dict[chrom] = f"/Volumes/GENDATAENV3/tommo_cooccurrence/tommo-54kjpn-20230828-GRCh38-autosome-{chrom}-plink-r2.tsv.gz"
        self.dict = raw_file_path_dict
    
    def set_fetcher(self, chrom: str):
        self.pysam_fetcher = pysam.TabixFile(self.dict[chrom])
    
    def fetcher(self):
        return self.pysam_fetcher
    
    
class PGS:
    def __init__(self, vcf_raw_paths: list, vcf_types: list, gwas: GWAS, ld=None, TOMMO_gt_paths=["/Volume/GENDATAENV1/tommo54k/gt/tommo-54kjpn-20230626r2-GRCh38-gf-autosome.vcf.gz", "/Volume/GENDATAENV1/tommo54k/gt/tommo-54kjpn-20230626r2-GRCh38-gf-chrX_PAR2.vcf.gz"], target_samples=[[f"NA{i}" for i in range(18939,19092)], "all", "all", "all"]):
        self.variant_fetchers = dict()
        for file in vcf_raw_paths:
            if type(file)!=list:
                self.variant_fetchers[file] = pysam.VariantFile(file)
            elif type(file)==list:
                self.variant_fetchers[file] = dict()
                for i in file:
                    self.variant_fetchers[file][i] = pysam.VariantFile(i)
                
        self.gwas = gwas.df()
        
        self.ld = ld
        self.tommo_fetchers = dict()
        
        for file in TOMMO_gt_paths:
            self.tommo_fetchers[file] = pysam.VariantFile(file)
            
        self.target_samples = target_samples
        
        self.scores = dict() 
        for file in vcf_raw_paths:
            j = vcf_raw_paths.index(file)
            if type(file)!=list:
                if vcf_types[j]=="single":
                    self.scores[self.variant_fetchers[file].header.samples[0]] = {"pgs":0, "variant count": 0}
                elif vcf_types[j]=="multiple":
                    if target_samples[j]=="all":
                        self.scores[file] = {sample: {"pgs":0, "variant count": 0} for sample in self.variant_fetchers[file].header.samples}
                    else:
                        self.scores[file] = {sample: {"pgs":0, "variant count": 0} for sample in target_samples[j]}
            elif type(file)==list:
                if vcf_types[j]=="single":
                    self.scores[self.variant_fetchers[file][file[0]].header.samples[0]] = {"pgs":0, "variant count": 0}
                elif vcf_types[j]=="multiple":
                    if target_samples[j]=="all":
                        self.scores[file] = {sample: {"pgs":0, "variant count": 0} for sample in self.variant_fetchers[file][file[0]].header.samples}
                    else:
                        self.scores[file] = {sample: {"pgs":0, "variant count": 0} for sample in target_samples[j]}
            else:
                print("vcf_types list must have either 'single' or 'multiple'")
        
    def  clump(self, r2=0.5, kilobase=1000):
        for index, snp in self.gwas.iterrows():
            chrom = snp["chrom"]
            if (chrom=="chrX") or (chrom=="chrX"):
                chrom = "chrXY"
            
            self.ld.set_fetcher(chrom)
            
            pos = snp["pos"]
            window_lower = pos-kilobase*1000
            window_upper = pos+kilobase*1000
            
            snp_r2 = 0
            num_clumped = 0
            # while looping through ld data, if fetched snp is in the snp list, then add r2 to the loop invariant snp 
            for i in self.ld.fetcher.fetch(snp["chrom"], snp["pos"]-1, snp["pos"]):
                retrieved = re.split(r'\t+', i.rstrip('\n'))
                pair_r2 = retrieved[12]
                pair_pos = retrieved[8]
                
                # if not self and is within window
                if (pos != pair_pos)&(window_lower<pos<window_upper):
                    if pair_pos in self.gwas["pos"]:
                        snp_r2 += pair_r2
                       
                # if snp_r2 exceeds threshold, delete the snp from list and break
                if snp_r2 > r2:
                    self.gwas.drop(label=index, axis=0, inplace=True)
                    num_clumped += 1
                    break
            print(f"{num_clumped} variants excluded")
            
    def threshold(self, pval_thr: float, mae_thr=None, info_thr=None, hwe_thr=0.05):
        original_length = len(self.gwas)
        self.gwas = self.gwas[self.gwas["pval"]<pval_thr]
        print(f"{original_length - len(self.gwas)} variants excluded")
        if mae_thr:
            original_length = len(self.gwas)
            self.gwas = self.gwas[self.gwas["mae"]>mae_thr]
            print(f"{original_length - len(self.gwas)} variants excluded")
        if info_thr:
            original_length = len(self.gwas)
            self.gwas = self.gwas[self.gwas["info"]>info_thr]
            print(f"{original_length - len(self.gwas)} variants excluded")
        if hwe_thr:
            # fetch snp rom vcf, look up hwe pvalue, and apply threshold
            num_excluded = 0
            for index, snp in self.gwas.iterrows():
                chrom = snp["chrom"]
                pos = snp["pos"]
                eff = snp["eff"]
                for fetcher in self.tommo_fetchers.values():
                    for record in fetcher.fetch(chrom, pos-1, pos):
                        retrieved = re.split(r'\t+', str(snp).rstrip('\n'))
                        hwe = retrieved[-1].split(";")[-2]
                        hwe_pval_list = list(map(float, hwe.split("=")[-1].split(",")))
                        
                        alt = retrieved[4].split(",")
                        try:
                            which_alt = alt.index(eff)
                        except ValueError:
                            break
                        
                        hwe_pval = hwe_pval_list[which_alt]
                        
                        if hwe_pval > hwe_thr:
                            self.gwas.drop(label=index, axis=0, inplace=True)
                            num_excluded += 1
            
            print(f"{num_excluded} variants excluded")

    def snplist(self, head=False):
        return self.gwas
        
    def compute(self):
        """
        Procedure:
        1. create score memory for each input vcf files and each samples
            a1. if multiple vcf (e.g. per chromosome), create a dictionary
            b1. if single vcf, create integer
            a2. if multiple sample vcf, create a dictionary
            b2. if single sample vcf, create integer
        2. go through gwas snp list and fetch vcf
        3. verify that the matched variants have same reference allele
        4. find which alt allele is the targetted snp
        5. extract genotype information
        6. find the appropriate PGS allele count weight
        7. compute PGS based on beta and PGS allele count weight
        8. add computed PGS to the memory of the sample
        9. do 3~9 for each samples if multiple sample VCF
        10. do 2~9 for each VCF
        """
        for snp in self.gwas:
            chrom = snp["chrom"]
            pos = snp["pos"]
            ref = snp["ref"]
            eff = snp["eff"]
            beta = snp["beta"]
            for index, path in enumerate(vcf_raw_paths):
                if type(path) != list:
                    if vcf_types[index] == "multiple":
                        for record in self.variant_fetchers[path].fetch(chrom, pos-1, pos):
                            if record.ref == ref: # check just in case
                                try: 
                                    which_alt = 0 if eff==record.ref else record.alts.index(eff) + 1
                                except ValueError:
                                    break
                                for sample in self.scores[path].keys():
                                    genotype = record.samples[sample]["GT"] # expecting a tuple from pysam
                                    if None in genotype:
                                        break
                                    else:
                                        eff_count = genotype.count(which_alt)
                                        snp_pgs = beta * eff_count
                                        self.scores[path][sample]["pgs"] += snp_pgs
                                        if eff_count > 0:
                                            self.scores[path][sample]["variant count"] += 1
                                        
                    elif vcf_types[index] == "single":
                        for record in self.variant_fetchers[path].fetch(chrom, pos-1, pos):
                            if record.ref == ref:
                                try: 
                                    which_alt = 0 if eff==record.ref else record.alts.index(eff) + 1
                                except ValueError:
                                    break
                                for sample in self.scores[self.variant_fetchers[path].header.samples[0]].keys():
                                    genotype = record.samples[sample]["GT"] # expecting a tuple from pysam
                                    if None in genotype:
                                        break
                                    else:
                                        eff_count = genotype.count(which_alt)
                                        snp_pgs = beta * eff_count
                                        self.scores[sample]["pgs"] += snp_pgs
                                        if eff_count > 0:
                                            self.scores[sample]["variant count"] += 1


                elif type(path) == list: 
                    for sub_path in path:
                        if vcf_types[index] == "multiple":
                            for record in self.variant_fetchers[path][sub_path].fetch(chrom, pos-1, pos):
                                if record.ref == ref: # check just in case
                                    try: 
                                        which_alt = 0 if eff==record.ref else record.alts.index(eff) + 1
                                    except ValueError:
                                        break
                                    for sample in self.scores[path].keys():
                                        genotype = record.samples[sample]["GT"] # expecting a tuple from pysam
                                        if None in genotype:
                                            break
                                        else:
                                            eff_count = genotype.count(which_alt)
                                            snp_pgs = beta * eff_count
                                            self.scores[path][sample]["pgs"] += snp_pgs
                                            if eff_count > 0:
                                                self.scores[path][sample]["variant count"] += 1

                        elif vcf_types[index] == "single":
                            for record in self.variant_fetchers[path][sub_path].fetch(chrom, pos-1, pos):
                                if record.ref == ref:
                                    try: 
                                        which_alt = 0 if eff==record.ref else record.alts.index(eff) + 1
                                    except ValueError:
                                        break
                                    for sample in self.scores[self.variant_fetchers[path][path[0]].header.samples[0]].keys():
                                        genotype = record.samples[sample]["GT"] # expecting a tuple from pysam
                                        if None in genotype:
                                            break
                                        else:
                                            eff_count = genotype.count(which_alt)
                                            snp_pgs = beta * eff_count
                                            self.scores[sample]["pgs"] += snp_pgs
                                            if eff_count > 0:
                                                self.scores[sample]["variant count"] += 1


