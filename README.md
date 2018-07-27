# 3D-MICE: integration of cross-sectional and longitudinal imputation
### Requirements
Code is written in R.


### Get Started
To train, run (better run as R markdown)
```
source('tempMICEGPEvalTr.R')
```

This is a wrapper code calling various subroutines that generate the training data, mask missing values, and performs 3D-MICE imputation, each step is wrapped in its own R source file and should be self-explanatory. 

Similarly, to train, run (better run as R markdown)
```
source('tempMICEGPEvalTe.R')
```

### Citation
```
@article{luo20173d,
  title={3D-MICE: integration of cross-sectional and longitudinal imputation for multi-analyte longitudinal clinical data},
  author={Luo, Yuan and Szolovits, Peter and Dighe, Anand S and Baron, Jason M},
  journal={Journal of the American Medical Informatics Association},
  volume={25},
  number={6},
  pages={645--653},
  year={2017},
  publisher={Oxford University Press}
}
```
