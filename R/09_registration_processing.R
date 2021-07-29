#### 09 REGISTRATION PROCESSING ################################################

source("R/01_startup.R")


# Load previous data ------------------------------------------------------

qload("output/str_processed.qsm", nthreads = availableCores())


# Get existing registration scrapes from server ---------------------------

upgo_connect(registration = TRUE)

registration_old <-
  registration_remote %>%
  filter(property_ID %in% !!property$property_ID) %>%
  collect() %>%
  group_by(property_ID) %>%
  filter(date == max(date)) %>%
  ungroup()

registration <- registration_old
if (!is.null(property$registration)) property$registration <- NULL

# registration <- qread("output/registration.qs", nthreads = availableCores())


# Scrape new properties ---------------------------------------------------

upgo_scrape_connect(workers = 5, proxies = .proxy_list[21:25])
n <- 1

while (nrow(filter(property, !property_ID %in% registration$property_ID)) > 0 &&
       n < 10) {

  n <- n + 1

  new_scrape <-
    property %>%
    st_drop_geometry() %>%
    filter(scraped >= "2021-06-01") |> 
    filter(!property_ID %in% registration$property_ID) %>%
    dplyr::slice(1:2000) %>%
    upgo_scrape_ab_registration(timeout = 3)

  registration <-
    registration %>%
    bind_rows(new_scrape)

  qsave(registration, file = "output/registration_new.qs",
        nthreads = availableCores())

}


# Recheck NA scrapes ------------------------------------------------------

NA_scrapes <-
  registration %>%
  filter(is.na(registration))

NA_scrapes_checked <- NA_scrapes[0,]

while (nrow(filter(NA_scrapes,
                   !property_ID %in% NA_scrapes_checked$property_ID)) > 0 &&
       n < 20) {

  n <- n + 1

  new_scrape <-
    NA_scrapes %>%
    filter(!property_ID %in% NA_scrapes_checked$property_ID) %>%
    dplyr::slice(1:2000) %>%
    upgo_scrape_ab_registration()

  NA_scrapes_checked <-
    NA_scrapes_checked %>%
    bind_rows(new_scrape)

}

upgo_scrape_disconnect()


# Add new scrapes to server -----------------------------------------------

registration_new <-
  registration %>%
  anti_join(registration_old)

NA_scrapes_new <-
  NA_scrapes_checked %>%
  anti_join(registration, by = c("property_ID", "date"))

RPostgres::dbWriteTable(.con, "registration", registration_new, append = TRUE)
RPostgres::dbWriteTable(.con, "registration", NA_scrapes_new, append = TRUE)

upgo_scrape_disconnect()


# Consolidate output ------------------------------------------------------

registration_table <-
  registration_old %>%
  bind_rows(registration_new) %>%
  filter(property_ID != "ab-NA") |> 
  arrange(property_ID, date) %>%
  group_by(property_ID) %>%
  filter(date == max(date)) %>%
  ungroup()

rm(registration)


# Clean output ------------------------------------------------------------

registration_table <-
  registration_table |> 
  mutate(registration = toupper(registration)) |> 
  mutate(registration = case_when(
    is.na(registration) ~ NA_character_,
    registration == "NO LISTING" ~ "NO LISTING",
    registration == "HOMEAWAY" ~ "HOMEAWAY",
    str_detect(registration, "TRANSIENT") ~ "EXEMPT TRANSIENT",
    str_detect(registration, "HOTEL") ~ "EXEMPT HOTEL",
    str_detect(registration, "BREAKFAST") ~ "EXEMPT BB",
    str_detect(registration, "HSR") ~ registration,
    TRUE ~ "INVALID"
  ))


# Save output -------------------------------------------------------------

qsave(registration_table, file = "output/reg_scrape.qs",
      nthreads = availableCores())
# registration_table <- qread("output/reg_scrape.qs", nthreads = availableCores())


# Import City registration data -------------------------------------------

reg_city <- 
  readxl::read_xlsx("data/registration/registration_2021-02-17.xlsx") |> 
  set_names(c("reg_number", "address", "unit_number", "reg_name", "platform"))


# Merge City data with scrape results -------------------------------------

property <- 
  registration_table |> 
  mutate(valid = registration %in% reg_city$reg_number) |> 
  select(property_ID, reg_number = registration, reg_status = valid) |> 
  mutate(reg_status = case_when(
    reg_status == TRUE ~ "Valid",
    reg_number == "INVALID" ~ "Fake",
    reg_status == FALSE & str_detect(reg_number, "HSR") ~ "Invalid",
    str_detect(reg_number, "EXEMPT") ~ "Exempt",
    is.na(reg_number) ~ "Missing",
    reg_number == "NO LISTING" ~ "No listing",
    reg_number == "HOMEAWAY" ~ "Vrbo")) |> 
  right_join(property) |> 
  relocate(reg_number, reg_status, .before = geometry)


# Save output -------------------------------------------------------------

qsavem(property, daily, GH, file = "output/str_processed.qsm", 
       nthreads = availableCores())

rm(reg_city, registration_table)
