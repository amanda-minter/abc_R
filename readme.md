`abc_R`
==========

Code for running case studies 1-2 in the manuscript entitled *Efficient methods for Approximate Bayesian Computation for infectious disease modelling* by Minter and Retkute.  


Usage
-----

Case study 1 can be run in the following manner:

```bash
Rscript Case_1/ABC_rejection_1.R
Rscript Case_1/ABC_rejection_2.R
Rscript Case_1/ABC_rejection_3.R
```

Case study 2 can be run, for instance, in the following manner:

```bash
Rscript Case_2/case_2_ABC_SMC_MNN.R
```

Requirements
------------

R code was run on [R version 3.5.3](https://cran.r-project.org).  Case study 1 requires the package [`deSolve`](https://cran.r-project.org/web/packages/deSolve/index.html), case study 2 requires packages [`tmvtnorm`](https://cran.r-project.org/web/packages/tmvtnorm/index.html), [`factoextra`](https://cran.r-project.org/web/packages/factoextra/index.html), [`ggpubr`](https://cran.r-project.org/web/packages/ggpubr/index.html), and [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/index.html).  

