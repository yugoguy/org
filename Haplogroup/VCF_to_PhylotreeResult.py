#!/usr/bin/env python
# coding: utf-8

# In[31]:


def VCF_to_PhylotreeResult(sample_VCF_path, phylotree_df_path="phylotree_df.pickle", filter_=0):
    import pandas as pd
    import numpy as np
    import warnings
    import matplotlib.pyplot as plt
    warnings.filterwarnings("ignore")
    import pickle
    
    df = pd.read_pickle(phylotree_df_path)
    df = df[["branch", "level", "pos"]]
    
    sample_VCF = pd.read_table(sample_VCF_path, header = None)
    sample_VCF.columns = ["chr", "pos", "rs", "ref", "var", "info0", "info1", "info2","info3", "info4"]
    sample_VCF_list = list(sample_VCF["pos"])
    
    branch_order = []
    match_score = []
    levels = []
    for i in range(22):
        search = df[df["level"]==i]
        for branch in search.index:
            branch_order.append(search["branch"].loc[branch])
            var_position = search["pos"].loc[branch]
            matching = len(set(var_position).intersection(set(sample_VCF_list)))/len(sample_VCF_list)
            match_score.append(matching)
            levels.append(search["level"].loc[branch])
            
    results = pd.DataFrame(branch_order)
    results.columns = ["branch"]
    results["%intersection"] = match_score
    results["tree_level"] = levels
    filtered = results[results["%intersection"]>filter_]
    
    plt.bar(x=filtered["branch"], height=filtered["%intersection"])
    plt.xticks(rotation=90)
    plt.show()
    
    most_likely = results[results["%intersection"]==results["%intersection"].max()]
    for group in most_likely.index:
        confidence_score = results["%intersection"].loc[group]/results["%intersection"].sum()
        most_likely_group = most_likely["branch"].loc[group]
        print(f"You most likely belong to haplogroup {most_likely_group}")
        #print(f"Confidence score for you bolonging to this haplogroup is {confidence_score}")
        #print("Note: confidence score is relative measure computed by (match rate for most likely group / total match rate for all groups)")


    return results
    

