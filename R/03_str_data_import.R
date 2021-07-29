#### 03 STR DATA IMPORT ########################################################

source("R/01_startup.R")
qload("output/geometry.qsm", nthreads = availableCores())
plan(multisession, workers = 10)


# Get data ----------------------------------------------------------------

upgo_connect()

property <-
  property_remote |> 
  filter(country == "United States", 
         metro_area == "Los Angeles-Long Beach-Anaheim, CA Metro Area") |> 
  collect() |>  
  filter(!is.na(created)) |> 
  strr_as_sf(32611) |> 
  st_filter(city)

daily <- 
  daily_remote |> 
  filter(property_ID %in% !!property$property_ID) |> 
  collect() |> 
  strr_expand()

host <-
  host_remote |> 
  filter(host_ID %in% !!property$host_ID) |> 
  collect() |> 
  strr_expand()

upgo_disconnect()


# Save temporary output ---------------------------------------------------

qs::qsavem(property, daily, host, file = "temp_STR.qsm", nthreads = 32)


# Process the property and daily files ------------------------------------

# Run raffle to assign a CT to each listing
property <-
  property |> 
  strr_raffle(CT, GEOID, dwellings, seed = 1) |> 
  select(-.grid_ID)


# Trim daily file
daily <- 
  daily |> 
  filter(property_ID %in% property$property_ID)

# Trim host file
host <- 
  host |> 
  filter(host_ID %in% property$host_ID)

# Add nbhd to property file
property <- 
  property |> 
  st_join(nbhd) |>
  select(-dwellings)

# Add nbhd to daily file
daily <- 
  property |> 
  st_drop_geometry() |> 
  select(property_ID, nbhd) |> 
  right_join(daily, by = "property_ID")


# Save output -------------------------------------------------------------

qsavem(property, daily, host, file = "output/str_raw.qsm",
       nthreads = availableCores())
qsave(host, file = "output/host.qs", nthreads = availableCores())
