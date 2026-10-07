# Function to create a multivariate cph model with a vector of variables

Function to create a multivariate cph model with a vector of variables

## Usage

``` r
multivariate_LL(db, vars, ptime, pevent, dec_HR = 4)
```

## Arguments

- db:

  A dataframe

- vars:

  Vector of variables to be included in the multivariate model

- ptime:

  Survival Time variable

- pevent:

  Event variable

- dec_HR:

  digits of HR (Default = 4)

## Value

the multivariate model
