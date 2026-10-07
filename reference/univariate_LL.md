# This function allows you to create the univariate regression model for a vector of variables

This function allows you to create the univariate regression model for a
vector of variables

## Usage

``` r
univariate_LL(db, vars, ptime, pevent, dec_HR = 4)
```

## Arguments

- db:

  dataframe

- vars:

  vector with variables name

- ptime:

  Survival Time variable

- pevent:

  Event variable

- dec_HR:

  digits of HR (Default = 4)

## Value

a dataframe with all univariate models
