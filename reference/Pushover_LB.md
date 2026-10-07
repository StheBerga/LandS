# Sending a Pushover notification on your device

Sending a Pushover notification on your device

## Usage

``` r
Pushover_LB(
  dest = "both",
  script = 0,
  timestamp = TRUE,
  priority = 0,
  app = "ayje1n4x8fi64bupdn5shjnwd8ut95",
  title = "RStudio",
  attachment = NULL,
  start_time = NULL
)
```

## Arguments

- dest:

  Who is going to receive the notification (Default = both)

- script:

  Text of your message

- timestamp:

  If you want the time printed in your notification, if TRUE it requires
  a start_time in the .GlobalEnv

- priority:

  Priority of your notification (-2; 2)

- app:

  Your app API key

- title:

  Your title notification

- attachment:

  Path to an image, if you want to attach it

- start_time:

  Start time (Default = NULL)
