# Function to send a Telegram message with BiostatUO9 bot. NB: must create a start_time before running it

Function to send a Telegram message with BiostatUO9 bot. NB: must create
a start_time before running it

## Usage

``` r
telegram_mess_LB(
  dest = "both",
  script = 0,
  rm_start_time = TRUE,
  timestamp = TRUE
)
```

## Arguments

- dest:

  Who is going to receive the message

- script:

  The title of the message

- rm_start_time:

  If you want the start_time item to be removed after the message is
  sent

- timestamp:

  Do you want the time in your message

## Value

Nothing
