# Distribution function

Distribution function

## Usage

``` r
Distribution_LB(data, var, split = FALSE, split_rule = NULL)
```

## Arguments

- data:

  a dataframe

- var:

  variable

- split:

  does the variable need to be splitted

- split_rule:

  the splitting rule

## Value

a plot with the variable

## Examples

``` r
Distribution_LB(data = mtcars, var = "mpg", split = TRUE, split_rule = 23)
#> Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
#> ℹ Please use the `linewidth` argument instead.
#> ℹ The deprecated feature was likely used in the LandS package.
#>   Please report the issue to the authors.
```
