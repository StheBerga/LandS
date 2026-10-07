# Function to create a new project in the default folder

Function to create a new project in the default folder

## Usage

``` r
New_Project_LB(
  project_name,
  rstudio = rstudioapi::isAvailable(),
  open = rlang::is_interactive()
)
```

## Arguments

- project_name:

  The name of the Project

- rstudio:

  If \`TRUE\`, calls \[use_rstudio()\] to make the new package or
  project into an \[RStudio
  Project\](https://r-pkgs.org/workflow101.html#sec-workflow101-rstudio-projects).
  If \`FALSE\` and a non-package project, a sentinel \`.here\` file is
  placed so that the directory can be recognized as a project by the
  \[here\](https://here.r-lib.org) or
  \[rprojroot\](https://rprojroot.r-lib.org) packages.

- open:

  If \`TRUE\`, \[activates\]\[proj_activate()\] the new project:

  \* If using RStudio desktop, the package is opened in a new session.
  \* If on RStudio server, the current RStudio project is activated. \*
  Otherwise, the working directory and active project is changed.

## Value

Returns a folder in Projects with Analisi, Dati and Output subfolders
