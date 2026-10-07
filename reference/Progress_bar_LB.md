# Nice progress bar for cycles with different visual outputs

Nice progress bar for cycles with different visual outputs

## Usage

``` r
Progress_bar_LB(
  current,
  total,
  start_time,
  bar_length = 80,
  bar_fill = "=",
  bar_void = " ",
  logfile = NULL
)
```

## Arguments

- current:

  The current iteractive value in the cycle

- total:

  The total number of variables in the cycle

- start_time:

  Start time of the cycle

- bar_length:

  Bar width in the console (default 80)

- bar_fill:

  The bar fill character

- bar_void:

  The empty bar character

- logfile:

  If not null e.g. 'Log.txt' prints the loop log

## Value

Only visual output in the console
