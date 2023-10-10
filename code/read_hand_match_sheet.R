library(googlesheets4)

gs4_auth(email = "devin.jl@gmail.com")

hand_match <- read_sheet("15LMOzyyVeKEc31U8XrKLf1WbM5z9b9GFxFtGhhNBkXI") |>
  drop_na(hand_match)

write_csv(hand_match,
          file = here::here("data", "hand_match.csv"))

