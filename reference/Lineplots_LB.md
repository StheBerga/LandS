# Function to build the lineplots

Function to build the lineplots

## Usage

``` r
Lineplots_LB(
  data,
  variables,
  time,
  breaks = unique(data[, time]),
  label = unique(data[, time]),
  group = 1,
  col_lines = c("salmon", "royalblue"),
  stat_line = "median",
  smooth_line = FALSE,
  span_line = 0.3,
  lw_reg = 1,
  alpha_line = 1,
  ylim = c(0.2, 0.8),
  ribbon = TRUE,
  alpha_ribbon = 0.05,
  ID_lines = FALSE,
  ID = "ID",
  alpha_ID_line = 0.3,
  lw_ID_line = 0.2,
  Point = FALSE,
  alpha_point = 0.3,
  size_point = 0.3,
  col_title = FALSE,
  fill_title = NULL,
  size_title = 7,
  alpha_fill_title = 0.2,
  label_title = paste0("Lineplots by ", group, "\n", format(Sys.Date(), "%d/%m/%Y")),
  size_label_title = 2.5,
  size_axis_x = 5,
  size_axis_y = 6,
  Overall = F,
  Test_results = Test_results,
  Posthoc = F,
  threshold_posthoc = 0.1,
  posthoc_test_size = 2,
  grid = T,
  ratio = 1,
  PPTX = F,
  pptx_width = 8.5,
  pptx_height = 5.5,
  target = "Output/Lineplots.pptx",
  extra = F,
  extra_text = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A long-formatted dataframe

- variables:

  Vector of variables to plot

- time:

  Numeric variable to plot on the x-axis

- breaks:

  Numeric vector with x-axis breaks. Default: unique(data\[, time\])

- label:

  Vector with x-axis labels. Must be the same length of breaks. Default:
  unique(data\[, time\])

- group:

  Factor variable to group the lines

- col_lines:

  Colours for the lines. Default: "salmon" & "royalblue"

- stat_line:

  Statistic method for the line. "median" or "mean".

- smooth_line:

  Whether to show smooth or spline regression line. Defaul = FALSE

- span_line:

  The span of smooth line. Default = 0.3

- lw_reg:

  Linewidth for regression line

- alpha_line:

  Alpha for regression line

- ylim:

  Limits for the y-axis to plot. Default: c(0.20, 0.80)

- ribbon:

  Whether to show or not ribbons

- alpha_ribbon:

  Alpha for ribbons. Default: 0.05

- ID_lines:

  Whether to show ID lines for every patient

- ID:

  ID variable. Default: "ID"

- alpha_ID_line:

  Alpha for ID lines. Default: 0.3

- lw_ID_line:

  Linewidth for ID lines. Default: 0.2

- Point:

  Whether to show points. Default = FALSE

- alpha_point:

  Alpha points. Default = 0.3

- size_point:

  Size points. Default = 0.3

- col_title:

  Whether to personalize the colour of title. Default = FALSE

- fill_title:

  A function to personalize the fill of title background. See vignette
  for more

- size_title:

  Size of title. If grid recommended 7, if PPTX recommended 20

- alpha_fill_title:

  Alpha for title background. Default = 0.2

- label_title:

  A title for your list. Default sets "Lineplots by grouping variable"
  and the current date

- size_label_title:

  Size for your list's title. Default = 2.5

- size_axis_x:

  x-axis text size. If grid recommended 5, if PPTX recommended 14

- size_axis_y:

  y-axis text size. If grid recommended 6, if PPTX recommended 14

- Overall:

  Whether to add overall test. Default = FALSE. See vignette for more.

- Test_results:

  Dataframe for overall and posthoc tests. See vignette for more.

- Posthoc:

  Whether to add posthoc tests with brackets. Default = FALSE. See
  vignette for more.

- threshold_posthoc:

  Threshold to display posthoc brackets

- posthoc_test_size:

  Size for annotations of posthoc p-values. Default = 2

- grid:

  Whether to build a grid pdf or a PPTX file. Default = TRUE

- ratio:

  Graph ratio when grid = TRUE

- PPTX:

  Whether to build PPTX or a grid pdf file. Default = FALSE. Must change
  grid = FALSE

- pptx_width:

  Graph dimensions for PPTX in inches

- pptx_height:

  Graph dimensions for PPTX in inches

- target:

  Path where to save the PPTX file

- extra:

  Whether to add an extra text function. Default = FALSE

- extra_text:

  A function to add extra functions to the graphs. See vignette for
  more.

- verbose:

  print progress bar and messages

## Value

When grid = TRUE returns a list of ggplots. When PPTX = TRUE and grid =
FALSE returns a PPTX file in the target folder
