# Test for continuous variables splitted by categories

The most powerful function ever created. You can perform the 4 major
tests and the posthoc tests for Friedman and Kruskal-Wallis. If you are
dumb (option dumb = T) you can also perform posthoc tests without
correcting for test multiplicity. Please do not try this at home/work
and consider asking a statistician before performing any test. Stored
functions for statistic option are (median), (mean), (sd), (min), (max),
(q1), (q3), (n), and (range).

## Usage

``` r
cont_var_test_LB(
  data,
  variables,
  paired = FALSE,
  group,
  dumb = FALSE,
  statistic = "{mean} ({sd})",
  ID = "ID",
  num_dec = 2,
  p.adjust.method = NULL,
  excel = F,
  excel_path = paste0(path_out, "/Results.xlsx"),
  telegram = "none",
  verbose = TRUE
)
```

## Arguments

- data:

  dataframe

- variables:

  vector containing all variables of interest

- paired:

  FALSE/TRUE

- group:

  factor variable splitting the data

- dumb:

  FALSE are you dumb? Hope not

- statistic:

  Specifies summary statistics to display for each variable. Default =
  "(mean) (sd)".

- ID:

  ID variabl (Default = "ID")

- num_dec:

  Decimal number for mean and SD (Default = 2)

- p.adjust.method:

  correction method, a character string. Can be abbreviated.

- excel:

  export fuction results as multiple Excel sheets

- excel_path:

  path where you want your Excel

- telegram:

  send a telegram message

- verbose:

  print progress bar and messages

## Value

Una lista con dataset

## Examples

``` r
cont_var_test_LB(data = iris, variables = c("Sepal.Length", "Sepal.Width"), group = "Species", paired = FALSE)
#> Loading required package: progress
#> Loading required package: PMCMRplus
#> Loading required package: rlang
#> Loading required package: gtsummary
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> ████████████████████████████████████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  50%    1/2 | 00:00:00 | ETA: 00:00:00 | RAM: 0.13Gb████████████████████████████████████████████████████████████████████████████████ 100%    2/2 | 00:00:00 | ETA: 00:00:00 | RAM: 0.13Gb
#> Kruskal-Wallis rank sum test used
#> $Raw_tests
#>            Var Mean (sd): setosa Mean (sd): versicolor Mean (sd): virginica
#> 1 Sepal.Length       5.01 (0.35)           5.94 (0.52)          6.59 (0.64)
#> 2  Sepal.Width       3.43 (0.38)           2.77 (0.31)          2.97 (0.32)
#>   Kruskal_Wallis setosa vs versicolor setosa vs virginica
#> 1   8.918734e-22         3.058513e-09        6.000296e-22
#> 2   1.569282e-14         2.047087e-14        2.304897e-07
#>   versicolor vs virginica
#> 1            0.0008324597
#> 2            0.0474280138
#> 
#> $Form_tests
#>            Var Mean (sd): setosa Mean (sd): versicolor Mean (sd): virginica
#> 1 Sepal.Length       5.01 (0.35)           5.94 (0.52)          6.59 (0.64)
#> 2  Sepal.Width       3.43 (0.38)           2.77 (0.31)          2.97 (0.32)
#>   Kruskal_Wallis setosa vs versicolor setosa vs virginica
#> 1        <0.0001              <0.0001             <0.0001
#> 2        <0.0001              <0.0001             <0.0001
#>   versicolor vs virginica
#> 1                  0.0008
#> 2                  0.0474
#> 
#> $KW_ph_pval
#>            Var Kruskal_Wallis setosa vs versicolor setosa vs virginica
#> 1 Sepal.Length   8.918734e-22         3.058513e-09        6.000296e-22
#> 2  Sepal.Width   1.569282e-14         2.047087e-14        2.304897e-07
#>   versicolor vs virginica
#> 1            0.0008324597
#> 2            0.0474280138
#> 
```
