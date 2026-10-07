# This function creates a list of boxplot

This function creates a list of boxplot

## Usage

``` r
Boxplot_LB(
  data,
  variables,
  group,
  ID = "ID",
  rm.outliers = F,
  th.outliers = 1.5,
  Point = F,
  size_point = 0.3,
  alpha_point = 0.3,
  alpha_box = 0.1,
  width_box = 0.2,
  lwd_box = 0.1,
  notch = F,
  notchwidth = 0.5,
  Median_line = F,
  lwd_median_line = 0.8,
  col_median_line = "red",
  ID_lines = FALSE,
  lwd_ID_line = 0.2,
  alpha_ID_line = 0.3,
  Overall = F,
  Posthoc = FALSE,
  Test_results = NULL,
  threshold_posthoc = 0.05,
  posthoc_test_size = 3.88,
  bracket_shorten = 0,
  bracket.nudge.y = 0,
  axis_x_title = NULL,
  size_axis_x = 6,
  axis_y_title = NULL,
  size_axis_y = 6,
  col_title = FALSE,
  alpha_fill_title = 0.2,
  fill_title = NULL,
  title_leg = FALSE,
  title_legend = NULL,
  size_title = 8,
  breaks_axis_x = levels(data[, group]),
  labels_axis_x = levels(data[, group]),
  grid = TRUE,
  PPTX = FALSE,
  pptx_width = 7.5,
  pptx_height = 5.5,
  extra = FALSE,
  extra_text = NULL,
  palette = rep("transparent", nlevels(data[, group])),
  label_legend_title = paste0("Boxplots by ", group, "\n", format(Sys.Date(),
    "%d/%m/%Y")),
  size_legend_title = 3,
  size_legend_text = 3,
  size_legend_circle = 4,
  target = "Output/Boxplot.pptx",
  ratio = 1,
  telegram = "none",
  verbose = TRUE
)
```

## Arguments

- data:

  Dataframe

- variables:

  Vector containing all variables of interest

- group:

  Factor variable splitting the data. Default = 1, overall distribution

- ID:

  ID variable for paired observations. Default = "ID"

- rm.outliers:

  Whether to remove outliers from display. Default = FALSE

- th.outliers:

  Times between Q1, Q3 and IQR to remove outliers. Default = 1.5

- Point:

  Whether to display observation points. Default = FALSE

- size_point:

  Size for points. Default = 0.3

- alpha_point:

  Alpha for points. Default = 0.3

- alpha_box:

  Alpha parameter for the boxes. Default = 0.1

- width_box:

  Width parameter for the boxes. Default = 0.2

- lwd_box:

  Linewidth for the boxes. Default = 0.1

- notch:

  Whether to display notches for the boxes. Default = FALSE

- notchwidth:

  Width parameter of the notch. Default = 0.5

- Median_line:

  Whether to display the line connecting median values. Default = FALSE

- lwd_median_line:

  Median line linewidth. Default = 0.8

- col_median_line:

  Colour for the median line. Default = "red"

- ID_lines:

  Whether to display the lines for paired observations. Default = FALSE

- lwd_ID_line:

  Linewidth parameter for paired observations lines. Default = 0.2

- alpha_ID_line:

  Alpha parameter for paired observations lines. Default = 0.3

- Overall:

  Whether to display overall test in the upper-left corner. Default =
  FALSE

- Posthoc:

  Whether to display posthoc tests brackets. Default = FALSE

- Test_results:

  Dataframe for global and posthoc tests for the LandS::cont_var_test_LB
  function

- threshold_posthoc:

  Threshold for displaying posthoc tests brackets. Default = 0.1

- posthoc_test_size:

  Size for annotations of posthoc p-values. Default = 3.88

- bracket_shorten:

  Width of the bracket \[0,1\]. Default = 0

- bracket.nudge.y:

  Vertical adjustment to nudge brackets by. Useful to move up or move
  down the bracket. If positive value, brackets will be moved up; if
  negative value, brackets are moved down. Default = 0

- axis_x_title:

  Title for the x axis

- size_axis_x:

  Dimensions for the x axis title. Default = 6

- axis_y_title:

  Title for the y axis

- size_axis_y:

  Dimensions for the y axis title. Default = 6

- col_title:

  Whether to personalize the colour of title. Default = FALSE

- alpha_fill_title:

  Alpha for title background. Default = 0.2

- fill_title:

  A function to personalize the fill of title background. See vignette
  for more

- title_leg:

  Whether to personalize the title of the variables. Default = FALSE

- title_legend:

  A function to personalize the title of the variables. See vignette for
  more

- size_title:

  Dimensions for the title. Default = 8

- breaks_axis_x:

  Levels to be displayed in the graph

- labels_axis_x:

  Labels of the levels

- grid:

  Whether to build a grid pdf or a PPTX file. Default = TRUE

- PPTX:

  Whether to build PPTX or a grid pdf file. Must change grid = FALSE.
  Default = FALSE

- pptx_width:

  Graph dimensions for PPTX in inches. Default = 7.5

- pptx_height:

  Graph dimensions for PPTX in inches. Default = 5.5

- extra:

  Whether to add an extra text function. Default = FALSE

- extra_text:

  A function to add extra functions to the graphs. See vignette for more

- palette:

  Colours for the boxes. Must be a vector of the same length as the
  nlevels. Default = "transparent"

- label_legend_title:

  A title for your list. Default sets "Boxplot by grouping variable" and
  the current date

- size_legend_title:

  Size for the list's title. Default = 3

- size_legend_text:

  Size for the list's text. Default = 3

- size_legend_circle:

  Size for the list's circles Default = 4

- target:

  Path where to save the PPTX file

- ratio:

  Aspect ratio when grid = TRUE. Default = 1

- telegram:

  Whether to send a telegram message when finished. Default = "none" to
  not send any message

- verbose:

  print progress bar and messages

## Value

If length(variables) \> 1 returns a list of boxplot where the first one
is a coverpage. Otherwise returns a ggplot. When grid = TRUE returns a
list of ggplots. When PPTX = TRUE and grid = FALSE returns a PPTX file
in the target path

## Examples

``` r
Boxplot_LB(mtcars, c('mpg', 'disp'), 'vs')
#> Loading required package: ggplot2
#> Loading required package: officer
#> Creazione 2 boxplots con: 
#> -Split by vs
#> -Points: FALSE
#> -Outliers: FALSE
#> -Posthoc: FALSE
#> -ID_lines: FALSE
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the LandS package.
#>   Please report the issue to the authors.
#> Installing package into ‘/home/runner/work/_temp/Library’
#> (as ‘lib’ is unspecified)
#> also installing the dependency ‘cpp11’
#> ████████████████████████████████████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  50%    1/2 | 00:00:02 | ETA: 00:00:02 | RAM: 0.11Gb████████████████████████████████████████████████████████████████████████████████ 100%    2/2 | 00:00:02 | ETA: 00:00:00 | RAM: 0.11Gb
#> [[1]]
#> Error in scale_color_manual(values = palette): Continuous value supplied to a discrete scale.
#> ℹ Example values: 0 and 1.
```
