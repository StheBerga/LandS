# Function to compare similarity between two kinetics

Function to compare similarity between two kinetics

## Usage

``` r
Similarity_Kinetic_LB(
  df_long,
  variables,
  grouping_var = NULL,
  time_var = "Time",
  trend = "similar",
  n_perm = 10000,
  seed = 123,
  excel = "similarity_output.xlsx"
)
```

## Arguments

- df_long:

  Dataframe in full long format

- variables:

  Vector of variables to compare

- grouping_var:

  If NULL consider the global trend and compare the variables, if a
  two-level variable is indicated compare all the stratified variables
  for this

- time_var:

  Time variable name in the dataset

- trend:

  If “similar” evaluates the similar kinetics of the curves, if
  “correlated” evaluates the correlation of the kinetics

- n_perm:

  Number of permutations

- seed:

  Seed to replicate permutations

- excel:

  Path to export excel

## Value

Results and file .xlsx
