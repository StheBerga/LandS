# Function to print the PDF with the grid.arrange function

Function to print the PDF with the grid.arrange function

## Usage

``` r
Print_LB(
  plot_list,
  path_print = path_print,
  nrow = 8,
  ncol = 6,
  ext = NULL,
  width_pg = 21,
  height_pg = 29.7,
  return = FALSE,
  warning = TRUE
)
```

## Arguments

- plot_list:

  The list you want to be plotted

- path_print:

  The path where you want your PDF to be printed

- nrow:

  Rows of your grid

- ncol:

  Columns of your grid

- ext:

  File extention

- width_pg:

  page width in cm

- height_pg:

  page height in cm

- return:

  if you want to assign your grid

- warning:

  if you want to suppress warnings (Default = TRUE)

## Value

A pdf in the path_output
