# This function allows to create a KM survival curve overall or splitted by a categorical variable

This function allows to create a KM survival curve overall or splitted
by a categorical variable

## Usage

``` r
KM_LB(
  Event = "OS_EVENT",
  tEvent = "OS",
  strata = 1,
  data = data,
  title = "Prova",
  xlab = "Time in months",
  ylab = "Probability of Survival",
  atrisklab = "At risk",
  xlim = c(0, max(data[, tEvent], na.rm = T)),
  lwd_lines = 1,
  alpha_CI = 0,
  size_pval = 4,
  x_pval = 5,
  y_pval = 0.5,
  size_title = 14,
  size_title_x = 12,
  size_title_y = 12,
  size_text_x = 10,
  size_text_y = 10,
  size_legend_title = 5,
  size_legend_text = 12,
  at_risk_title_size = 10.75,
  at_risk_size = 3,
  breaks_by = 3
)
```

## Arguments

- Event:

  Event variable

- tEvent:

  Survival Time Variable

- strata:

  Variable to stratify (Default = 1)

- data:

  dataframe

- title:

  Graph title (Default = "Prova")

- xlab:

  x-axis title (Default = "Time in months")

- ylab:

  y-axis title (Default = "Probaility of Surv")

- xlim:

  limits of x-axis (Default is from 0 to maximum observed time)

- lwd_lines:

  linewidth of survival curves

- alpha_CI:

  transparency of CI of survival curves. To not display CI set it to 0
  (default)

- size_pval:

  size of log-rank-test p-value

- x_pval:

  x-axis coordinates of pvalue, same scale of tEvent

- y_pval:

  y-axis coordinates of pvalue, between 0 and 1

- size_title:

  size of title

- size_title_x:

  size of x-axis title

- size_title_y:

  size of y-axis title

- size_text_x:

  size of x-axis text

- size_text_y:

  size of y-axis text

- size_legend_title:

  size of legend title

- size_legend_text:

  size of legend text

- at_risk_title_size:

  size of at risk table title

- at_risk_size:

  size of at risk table text

- breaks_by:

  breaks of risk table (Default = 3)

- legendlab:

  title of at risk table (Default = "At risk")

## Value

a KM graph
