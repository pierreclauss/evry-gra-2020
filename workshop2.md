Workshop 2 - Multi-assets tactical allocation, Tangency Portfolio and
Black-Litterman approach
================
Pierre Clauss
September 2020

*The following R Markdown document has to be read with my course notes
(in particular for the details of the analysis framework).*

*Not all the R codes are displayed but only some of them to help you to
succeed the workshop.*

## 1 Data

### 1.1 Importation

I import data in the same way than in the workshop 1.

``` r
library(tidyverse)
library(readxl)
(workshop2 <- read_xlsx("data.xlsx", sheet = "workshop2", skip = 3))
```

    ## # A tibble: 203 x 4
    ##    `TRADE DATE`        `NASDAQOMX/NQFRT/I~ `NASDAQOMX/NQBRICT~ `ML/TRI/BAMLCC0A~
    ##    <dttm>                            <dbl>               <dbl>             <dbl>
    ##  1 2003-10-31 00:00:00            0.0730               0.0679           -0.0112 
    ##  2 2003-11-30 00:00:00            0.0481               0.0347            0.00488
    ##  3 2003-12-31 00:00:00            0.0864               0.137             0.0130 
    ##  4 2004-01-31 00:00:00           -0.000539            -0.00188           0.0108 
    ##  5 2004-02-29 00:00:00            0.0245               0.0644            0.0116 
    ##  6 2004-03-31 00:00:00           -0.0375               0.0294            0.0103 
    ##  7 2004-04-30 00:00:00           -0.0106              -0.108            -0.0311 
    ##  8 2004-05-31 00:00:00            0.0251              -0.0581           -0.00708
    ##  9 2004-06-30 00:00:00            0.0313               0.0179            0.00486
    ## 10 2004-07-31 00:00:00           -0.0319               0.0102            0.0121 
    ## # ... with 193 more rows

Data are a sample of monthly returns (i) for one part from Nasdaq
observed on 2 equity indexes (France and BRIC) and (ii) for a second
part from Merrill Lynch observed on 1 Bond index (US corporates). The
start date is 31th October 2003 until today.

### 1.2 Wrangling

I need for the workshop only returns to do calculation on: so I shrink
the data of workshop 2 to returns.

``` r
fin_return <- workshop2 %>% select(-"TRADE DATE")
```

Then, the wrangling is quite simple here as for the workshop 1 and is
essentially done via data importation in Excel: data are tidy - each
column is a variable (an index) and each line is an observation (a
month) - and data are transformed in financial returns, which are used
to model a portfolio.

I continue to work on monthly returns because they are nearer to
gaussian data than weekly and daily data.

We can see below, thanks to the package **DataExplorer**, a summary of
the tidy data observed for the 3 variables.

``` r
library(DataExplorer)
plot_intro(fin_return)
```

![](workshop2_files/figure-gfm/wrangling-1.png)<!-- -->

I can conclude that data are tidy without missing values.

### 1.3 Visualisation

Data viz has to be thought in relation with modelling. As for workshop
1, I am interested by visualising the distributions of the returns and
the structure of the correlations between them.

Some statistics to sum up the distribution can be shown below: I can
observe symmetric data with a median and a mean which are quite equal.

``` r
summary(fin_return)
```

    ##  NASDAQOMX/NQFRT/INDEX VALUE NASDAQOMX/NQBRICT/INDEX VALUE
    ##  Min.   :-0.219149           Min.   :-0.28792             
    ##  1st Qu.:-0.023993           1st Qu.:-0.02615             
    ##  Median : 0.010216           Median : 0.01548             
    ##  Mean   : 0.007537           Mean   : 0.01192             
    ##  3rd Qu.: 0.045985           3rd Qu.: 0.05130             
    ##  Max.   : 0.146377           Max.   : 0.24392             
    ##  ML/TRI/BAMLCC0A0CMTRIV
    ##  Min.   :-0.074740     
    ##  1st Qu.:-0.003067     
    ##  Median : 0.005360     
    ##  Mean   : 0.004518     
    ##  3rd Qu.: 0.013086     
    ##  Max.   : 0.055541

I can go deeper thanks to distribution graphics: the non-parametric
(kernel method) estimation of the distribution and QQ-plots.

``` r
plot_density(fin_return)
```

![](workshop2_files/figure-gfm/dataviz_2-1.png)<!-- -->

``` r
plot_qq(fin_return)
```

![](workshop2_files/figure-gfm/dataviz_2-2.png)<!-- -->

Finally, I can visualize the correlations between each of the indexes.
To obtain efficient diversification between assets, we need correlations
smaller than 1, which can be observed in the graph below.

``` r
library(corrplot)
corrplot(cor(fin_return), type='upper', tl.col = 'black', tl.cex = 0.5)
```

![](workshop2_files/figure-gfm/dataviz_3-1.png)<!-- -->

## 2 Modelling

Before all, I need to load the package **scales** to communicate with a
pretty way the results of our allocations.

``` r
library(scales)
```

### 2.1 Analysis framework

The analysis framework of our modelling is the Modern Portfolio Theory
initiated by H. Markowitz in the 1950s. An essential portfolio, with the
*Global Minimum Variance* (GMV) portfolio studied in the workshop 1, is
the *Tangency Portfolio* (TP), also named *Maximum Sharpe ratio
portfolio*, for which the weights
![\\omega](https://latex.codecogs.com/png.latex?%5Comega "\\omega") are
equal to :

  
![\\omega = \\frac{1}{\\tilde
A}\\Sigma^{-1}\\tilde\\mu](https://latex.codecogs.com/png.latex?%5Comega%20%3D%20%5Cfrac%7B1%7D%7B%5Ctilde%20A%7D%5CSigma%5E%7B-1%7D%5Ctilde%5Cmu
"\\omega = \\frac{1}{\\tilde A}\\Sigma^{-1}\\tilde\\mu")  

with ![\\Sigma](https://latex.codecogs.com/png.latex?%5CSigma "\\Sigma")
the covariance matrix between assets returns of length
![n\*n](https://latex.codecogs.com/png.latex?n%2An "n*n"), with
![n](https://latex.codecogs.com/png.latex?n "n") the number of assets in
the portfolio,
![\\tilde\\mu](https://latex.codecogs.com/png.latex?%5Ctilde%5Cmu
"\\tilde\\mu") the vector of the expected excess returns equal to
![\\tilde\\mu = \\mu -
r\_fe](https://latex.codecogs.com/png.latex?%5Ctilde%5Cmu%20%3D%20%5Cmu%20-%20r_fe
"\\tilde\\mu = \\mu - r_fe") with
![\\mu](https://latex.codecogs.com/png.latex?%5Cmu "\\mu") the expected
returns, ![r\_f](https://latex.codecogs.com/png.latex?r_f "r_f") the
risk-free rate and ![e](https://latex.codecogs.com/png.latex?e "e") a
vector of ![1](https://latex.codecogs.com/png.latex?1 "1") of length
![n](https://latex.codecogs.com/png.latex?n "n") and finally
![\\tilde{A} =
\\tilde\\mu'\\Sigma^{-1}e](https://latex.codecogs.com/png.latex?%5Ctilde%7BA%7D%20%3D%20%5Ctilde%5Cmu%27%5CSigma%5E%7B-1%7De
"\\tilde{A} = \\tilde\\mu'\\Sigma^{-1}e").

Contrary to GMV portfolio, we have to estimate also
![\\mu](https://latex.codecogs.com/png.latex?%5Cmu "\\mu") with
![\\Sigma](https://latex.codecogs.com/png.latex?%5CSigma "\\Sigma").
Here, I will present the estimation methodology proposed by F. Black and
R. Litterman (BL) in 1990 and 1992 to estimate
![\\mu](https://latex.codecogs.com/png.latex?%5Cmu "\\mu"): see [the
following
paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=334304) to
demystify this approach and also pages 315-316 of [this book
chapter](https://faculty.fuqua.duke.edu/~mbrandt/papers/published/portreview.pdf).

I then construct a multi-assets allocation with equity, bond and
monetary assets (null weight for this last asset in the TP).

### 2.2 Estimation methodology

Before modelling, I separate the initial sample between a learning
sample and a backtest sample to evaluate the performance of our
modelling. I choose July 2018 as a separation date to backtest the
strategy on the last 2 years of the sample.

``` r
end_date <- nrow(fin_return)
fin_return_learning <- fin_return %>% slice(1:178)
fin_return_backtest <- fin_return %>% slice(179:end_date)
```

There are 178 learning observations and 25 backtest observations. My
objective is to observe if I can obtain better performance with a
tactical allocation on the backtest sample. Before all, I present the
construction of the strategic allocation.

#### 2.2.1 Strategic allocation for the Tangency Portfolio

I begin with classical unbiased estimators for
![\\mu](https://latex.codecogs.com/png.latex?%5Cmu "\\mu") and
![\\Sigma](https://latex.codecogs.com/png.latex?%5CSigma "\\Sigma"):

  
![\\hat\\mu = \\frac{1}{T}\\sum\_{t=1}^Tr\_t -
r\_f](https://latex.codecogs.com/png.latex?%5Chat%5Cmu%20%3D%20%5Cfrac%7B1%7D%7BT%7D%5Csum_%7Bt%3D1%7D%5ETr_t%20-%20r_f
"\\hat\\mu = \\frac{1}{T}\\sum_{t=1}^Tr_t - r_f")  

  
![\\hat\\Sigma =
\\frac{1}{T-n-2}\\sum\_{t=1}^T\\left(r\_t-\\hat\\mu\\right)\\left(r\_t-\\hat\\mu\\right)'](https://latex.codecogs.com/png.latex?%5Chat%5CSigma%20%3D%20%5Cfrac%7B1%7D%7BT-n-2%7D%5Csum_%7Bt%3D1%7D%5ET%5Cleft%28r_t-%5Chat%5Cmu%5Cright%29%5Cleft%28r_t-%5Chat%5Cmu%5Cright%29%27
"\\hat\\Sigma = \\frac{1}{T-n-2}\\sum_{t=1}^T\\left(r_t-\\hat\\mu\\right)\\left(r_t-\\hat\\mu\\right)'")  

Then, I can plug-in these estimates on the formula of the TP to obtain
unbiased estimators of its weights. I assume that
![r\_f=0\\%](https://latex.codecogs.com/png.latex?r_f%3D0%5C%25
"r_f=0\\%").

![](workshop2_files/figure-gfm/TP-1.png)<!-- -->

The realised return observed on the backtest sample of the portfolio
constructed on the learning sample is equal to 9.56%.

I am going to improve this result thanks to a more robust statistical
approach integrating economic predictions in the allocation.

#### 2.2.2 Tactical allocation for the Tangency Portfolio with Black-Litterman approach

Tactical allocation is a process that deviates the strategic allocation
thanks to new information: for instance, if I think that one index will
perform better in the near future, then I will increase its weight
relative to the others.

Black-Litterman approach is one of a quantitative methodology to
integrate these predictions/views in a relevant way. This approach adds
to economic predictions statistical uncertainty: the econometric
approach is no more a plug-in approach but a decision-bayesian approach.

The Black-Litterman returns are the following mixed estimates:

  
![\\hat\\mu\_\\text{mixed} =
\\left\[\\left(\\tau\\hat\\Sigma\\right)^{-1} +
\\Omega^{-1}\\right\]^{-1}
\\left\[\\left(\\tau\\hat\\Sigma\\right)^{-1}\\hat\\mu +
\\Omega^{-1}Q\\right\]](https://latex.codecogs.com/png.latex?%5Chat%5Cmu_%5Ctext%7Bmixed%7D%20%3D%20%5Cleft%5B%5Cleft%28%5Ctau%5Chat%5CSigma%5Cright%29%5E%7B-1%7D%20%2B%20%5COmega%5E%7B-1%7D%5Cright%5D%5E%7B-1%7D%20%5Cleft%5B%5Cleft%28%5Ctau%5Chat%5CSigma%5Cright%29%5E%7B-1%7D%5Chat%5Cmu%20%2B%20%5COmega%5E%7B-1%7DQ%5Cright%5D
"\\hat\\mu_\\text{mixed} = \\left[\\left(\\tau\\hat\\Sigma\\right)^{-1} + \\Omega^{-1}\\right]^{-1} \\left[\\left(\\tau\\hat\\Sigma\\right)^{-1}\\hat\\mu + \\Omega^{-1}Q\\right]")  

with ![Q](https://latex.codecogs.com/png.latex?Q "Q") the economic views
quantified by average returns,
![\\tau](https://latex.codecogs.com/png.latex?%5Ctau "\\tau") the
confidence parameter in the views and
![\\Omega](https://latex.codecogs.com/png.latex?%5COmega "\\Omega") the
matrix of uncertainty associated to the economic views; we assume that
![\\Omega](https://latex.codecogs.com/png.latex?%5COmega "\\Omega") is a
diagonal matrix with diagonal elements equal to variances of assets
returns.

``` r
# Example of parameters for BL approach
mu
```

    ##   NASDAQOMX/NQFRT/INDEX VALUE NASDAQOMX/NQBRICT/INDEX VALUE 
    ##                    0.10077507                    0.15455283 
    ##        ML/TRI/BAMLCC0A0CMTRIV 
    ##                    0.04776933

``` r
Q <- numeric(3)
Q[1] <- 0.05 # negative view for France equity index
Q[2] <- 0.05 # negative view for BRIC equity index
Q[3] <- 0.10 # positive view for US Corporate bonds index
tau <- 0.9
```

![](workshop2_files/figure-gfm/BL-1.png)<!-- -->

The realised return observed on the backtest sample of the BL portfolio
constructed on the learning sample is equal to 10.08%.

I can compare it to the portfolio constructed directly with views and
without uncertainty on the predictions. The realised return observed on
the backtest sample of this portfolio constructed on the learning sample
is equal to 10%. BL approach mitigates the return of this portfolio by
integrating uncertainty and then less confidence in the views.

## To conclude the second workshop

This workshop was the second of my course on Asset Management dedicated
to multi-assets allocation and Tangency Portfolio. I presented some
improvements of the classical plug-in estimators thanks to the
Black-Litterman approach.
