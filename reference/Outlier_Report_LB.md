# Function for outliers report in excel

Function for outliers report in excel

## Usage

``` r
Outlier_Report_LB(
  data,
  variables,
  ID = "ID",
  Group = "Time",
  k = 1.5,
  excel = FALSE,
  excel_path = "Output/Outlier Report.xlsx"
)
```

## Arguments

- data:

  a dataframe

- variables:

  vector of variables to check

- ID:

  ID variable. Default = "ID"

- Group:

  Grouping variable. Default = "Time"

- k:

  Parameter between Q1-Q3 and IQR. Default = 1.5

- excel:

  Whether to create an excel file of the report

- excel_path:

  Path for the excel file

## Value

a list of dataframe one of the global variable, the other for the
variable stratified for the grouping variable
