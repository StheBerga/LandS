# This function computes the correlation coefficients and prints the pairs from the heightest coefficient

This function computes the correlation coefficients and prints the pairs
from the heightest coefficient

## Usage

``` r
correlazioni_LB(
  data,
  variables,
  method = "spearman",
  rho_dec = 3,
  pval_dec = 4,
  excel = FALSE,
  excel_path = paste0(path_output, "/Results.xlsx")
)
```

## Arguments

- data:

  dataframe

- variables:

  vector of numeric variables to be computed the correlation

- method:

  method to compute the correlation coefficient (Default = "spearman")

- rho_dec:

  number of decimal for rho (Default = 3)

- pval_dec:

  number of decimal for the pvalue (Default = 4)

- excel:

  export fuction results as multiple Excel sheets

- excel_path:

  path where you want your Excel

## Value

Una lista con dataset
