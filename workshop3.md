Workshop 3 - Portfolio insurance strategies
================
Pierre Clauss
September 2020

*The following R Markdown document has to be read with my course notes
(in particular for the details of the analysis framework).*

*Not all the R codes are displayed but only some of them to help you to
succeed the workshop.*

## Foreword

There is no data steps in this workshop. I give you the annualised
expected return and volatility of the asset to be insured:

  
![\\hat\\mu
= 5\\%](https://latex.codecogs.com/png.latex?%5Chat%5Cmu%20%3D%205%5C%25
"\\hat\\mu = 5\\%")  

  
![\\hat\\sigma
= 20\\%](https://latex.codecogs.com/png.latex?%5Chat%5Csigma%20%3D%2020%5C%25
"\\hat\\sigma = 20\\%")  

``` r
library(tidyverse)
library(scales)
mu <- 0.05
sigma <- 0.20
```

## 1 Analysis framework

I focus on 2 approaches to insure my capital:

1.  Option-Based Portfolio Insurance with a call
2.  Constant-Proportion Portfolio Insurance

*See my course notes for the details of all the formulas.*

## 2 Simulation methodologies

As I want to simplify the workshop using gaussian simulations, I
simulate monthly returns which could be considered more gaussian than
daily or weekly returns. As an alternative not studied here, we could
use more complex modelling with mixtures of gaussian distributions or
Student distributions family.

The maturity of insurance capital is 1 year.

The other assumptions are:   
![r\_f = 2\\%](https://latex.codecogs.com/png.latex?r_f%20%3D%202%5C%25
"r_f = 2\\%")  
  
![\\text{stress}
= 20\\%](https://latex.codecogs.com/png.latex?%5Ctext%7Bstress%7D%20%3D%2020%5C%25
"\\text{stress} = 20\\%")  

``` r
delta <- 1 / 12
mat <- 1
rf <- 0.02
strike <- 100
stress <- 0.20
tol <- 0
```

### 2.1 An example with two simulated tracks

![](workshop3_files/figure-gfm/track1-1.png)<!-- -->

![](workshop3_files/figure-gfm/track2-1.png)<!-- -->

### 2.2 Monte-Carlo simulations

The number of Monte-Carlo simulations is
![10000](https://latex.codecogs.com/png.latex?10000 "10000").

``` r
nsimul <- 10000
```

    ## # A tibble: 3 x 4
    ##   rowname           Equity   OBPI   CPPI
    ##   <chr>              <dbl>  <dbl>  <dbl>
    ## 1 Annualised return 0.0499 0.0200 0.0233
    ## 2 Volatility        0.195  0.0263 0.0218
    ## 3 Insurance rate    0.559  1      0.998

![](workshop3_files/figure-gfm/simMC-1.png)<!-- -->![](workshop3_files/figure-gfm/simMC-2.png)<!-- -->![](workshop3_files/figure-gfm/simMC-3.png)<!-- -->

## To conclude the third workshop

This workshop was the last of my course on Asset Management dedicated to
structured portfolios with an objective of capital insurance. I
presented some tools to insure portfolios and study their risks and
performances.