# nnls2d : two examples

### To install the package from github
```r
devtools::install_github("Jawad-Boulahfa/nnls2d")
```

### Example 1: one explanatory variable
## Choose the parameters
```r
n <- 3
beta <- 2
sigma <- 4
```

## Generate the dataset
```r
dataset <- dataGenerator(n = n, beta = beta, sigma = sigma)
```

## Solve the positively constrained least squares problem
```r
results <- pcls1(X = dataset$X, Y = dataset$Y)
```

### Example 2: two explanatory variables
## Choose the parameters
```r
n <- 3
beta <- 1:2
sigma <- 4
```

## Generate the dataset
```r
dataset <- dataGenerator(n = n, beta = beta, sigma = sigma)
```

## Solve the positively constrained least squares problem
```r
results <- pcls2(X = dataset$X, Y = dataset$Y)
```
