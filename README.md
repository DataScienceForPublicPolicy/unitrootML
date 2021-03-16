# Machine Learning-Based Hypothesis Testing for Time Series  (hypML)

Recent data science for economics research has found that machine learning can vastly improve upon the current state of hypothesis testing, especially for identifying qualities of time series. This `R` package implements a number of ML-based tests that offer dramatic gains in accuracy, namely for detecting unit roots (subsequent versions for Stata and Python will be developed in due course).  This package is built to test three core unit root DGPs using the ML algorithm from [Cornwall, Chen, and Sauley (2021)](https://arxiv.org/abs/2103.01368):

![equation](https://latex.codecogs.com/gif.latex?y_t&space;=&space;\lambda&space;&plus;&space;\phi&space;y_{t-1}&space;&plus;&space;\delta&space;t&space;&plus;&space;\epsilon_t)

![equation](https://latex.codecogs.com/gif.latex?y_t&space;=&space;\lambda&space;&plus;&space;\phi&space;y_{t-1}&space;&plus;&space;\epsilon_t)

![equation](https://latex.codecogs.com/gif.latex?y_t&space;=&space;\phi&space;y_{t-1}&space;&plus;&space;\epsilon_t)

In addition, the `R` package also has the capability of training custom ML-based unit root tests.


## Set Up

Install this package from this Github repository, then load it up!
```
devtools::install_github("DataScienceforPublicPolicy/hypML") 
library(hypML)
```

## Applying an ML-based unit root test 

To illustrate the test in practice, we will need some test data. We simulate five time series: two with a unit root (`gamma = 1`) and three without a unit root (`gamma < 1`). These time series are stored in a list object. Let's take a look at a few of these time series using `autoplot` from the `forecast` package and arranged into a grid using `grid.arrange`.

```
 #Example time series
    set.seed(123)
    out <- list(series1 = ts(dgp_enders1(gamma = 1), freq = 12, start = c(2010,1)),
                series2 = ts(dgp_enders1(gamma = 1), freq = 12, start = c(2010,1)),
                series3 = ts(dgp_enders1(gamma = 0.98), freq = 12, start = c(2010,1)),
                series4 = ts(dgp_enders1(gamma = 0.8), freq = 12, start = c(2010,1)),
                series5 = ts(dgp_enders1(gamma = 0.5), freq = 12, start = c(2010,1)))

  #Plot
    gridExtra::grid.arrange(autoplot(out[[1]]), autoplot(out[[2]]), 
                            autoplot(out[[3]]), autoplot(out[[4]]), 
                            autoplot(out[[5]]), ncol = 2)
```

To screen these time series, apply the `ml_test_all` function using an efficient pre-trained gradient boosting algorithm. The function expects a list object containing time series objects (`bank =`) and a `costratio` to identify the optimal decision threshold given the preferred weight between Type I and II errors.   In this case, we assume a `costratio = 0.2`.


```
  res <- ml_test_all(bank = out,  
  			      costratio = 0.2)
```

The `res` object captures the outputs of a battery of tests, including pre-trained ML-based unit root tests. The first element summarizes the verdict that each unit root test reached when evaluating each time series (e.g. unit root versus stationary). While each of the traditional tests may disagree, it is worth noting that the tests built on Random Forest (`verdict.ranger`) and Gradient Boosting (`verdict.xgbTree`) offer markedly improved diagnostic accuracy.

```
  verdicts <- res$verdicts
  print(verdicts[,1:4])
```

The test statistics and their critical thresholds are recorded in the `results` element.

```
  results <- res$results
  print(results[,1:4])
```

Lastly, all the input features for the ML-based tests that were calculated from the time series list are available in the `features` element. 

```
  feats <- res$features
  feats[,1:4]
```

## Custom tests

It is possible to also create and use your own custom ML tests (see included vignettes). We have made available a [ testing package (1gb)](https://www.dropbox.com/s/eu3tjmasdd6zln6/sysdata_full.Rda?dl=0) that include both a gradient boost-based test and random forest-based test. Note that in Cornwall, Chen, and Sauley (2021), these two different ensemble-based tests yield nearly identical performance.

```
  res <- ml_test_all(bank = out,  
  			     original = FALSE,	
			     custom_model = base_models, 
			     costratio = 0.2)
```



## Got questions?

Reach out to Gary Cornwall (gary.cornwall@bea.gov) and Jeff Chen (contact@jeffchen.org).


