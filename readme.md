# abc_R

This repository contains code for running case studies 1-3 in the manuscript entitled "Approximate Bayesian Computation for infectious disease modelling" by Minter and Retkute.  


## Usage


### Case study 1: Deterministic compartmental model
Case study can be run in the following manner:

`Rscript ABC_rejection_1.R`

`Rscript ABC_rejection_2.R`

`Rscript ABC_rejection_3.R`


### Case study 2: Stochastic compartmental model
Case study can be run in the following manner:

`Rscript case_2_ABC.R`

`Rscript case_2_ABC_SMC.R`

`Rscript case_2_ABC_SMC_MNN.R`

### Case study 3: Stochastic individual-based model
Case study can be run in the following manner:

`Rscript case_3_ABC_SMC_MNN.R’
 

## Requirements


R code was run on [R version 3.5.3](https://cran.r-project.org).  Case study 1 requires the package [`deSolve`](https://cran.r-project.org/web/packages/deSolve/index.html), case study 2 requires packages [`tmvtnorm`](https://cran.r-project.org/web/packages/tmvtnorm/index.html), [`factoextra`](https://cran.r-project.org/web/packages/factoextra/index.html), [`ggpubr`](https://cran.r-project.org/web/packages/ggpubr/index.html), and [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/index.html),  case study 3 requires package [`tmvtnorm`](https://cran.r-project.org/web/packages/tmvtnorm/index.html).

