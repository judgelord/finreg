
# get appendix from google drive
googledrive::drive_auth(email = "devin.jl@gmail.com")

googledrive::drive_download(
  "https://docs.google.com/document/d/1CR5b3gDvfipxWcqheFHOOeAX2GMLg_9SJT54ByqqBJo",
  path = here::here("efficacy_appendix.txt"),
  overwrite = TRUE
  )

googledrive::drive_download(
  "https://docs.google.com/spreadsheets/d/1GeO5ndnTqqLoT6DwLkf5l5Z2sRQHlMm5-93aMr3GoZk",
  path = here::here("efficacy_appendix_table.csv"),
  overwrite = TRUE
)

