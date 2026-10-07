# Function to get a formatted p-value for a number o a vector of numbers

Function to get a formatted p-value for a number o a vector of numbers

## Usage

``` r
formatz_p(value, digits = 4)
```

## Arguments

- value:

  a number or a vector of numbers to be formatted

- digits:

  the number of decimal places (Default = 4)

## Value

a number or a vector of numbers formatted with 4 digits

## Examples

``` r
formatz_p(c(1.000, 0.75643242, 0.000032431, 0.00214))
#> [1] "1.0000"  "0.7564"  "<0.0001" "0.0021" 
```
