If in Colab
```
!rm -rf org && git clone --filter=blob:none --sparse https://github.com/yugoguy/org.git
!cd org && git sparse-checkout set dev/SSLVE && git checkout Latent-Variable-Evolution
```

```
import sys, os, glob
sys.path.insert(0, 'org/dev/SSLVE')
for f in sorted(glob.glob('org/dev/SSLVE/*.py')):
    print(f"Running: {f}")
    %run {f}
```

Could require several package installation.
