#### 12 RENT INCREASES #########################################################

source("R/01_startup.R")


# Load data ---------------------------------------------------------------

qload("output/geometry.qsm", nthreads = availableCores())
qload("output/str_processed.qsm", nthreads = availableCores())

# The magic value derived from Barron et al.
magic_value <- 0.00651547619


# Get zipcode data --------------------------------------------------------

zillow <- 
  read_csv("data/zillow.csv") |> 
  filter(MsaName == "Los Angeles-Long Beach-Anaheim, CA")

ZCTA <- 
  tigris::zctas() |> 
  as_tibble() |> 
  st_as_sf() |> 
  st_transform(32611)

zips <- 
  ZCTA |> 
  st_centroid() |> 
  st_filter(city) |> 
  pull(GEOID10)

ZCTA <- 
  ZCTA |> 
  filter(GEOID10 %in% zips) |> 
  select(zip = GEOID10, geometry)

zillow <- 
  zillow |> 
  filter(RegionName %in% ZCTA$zip) |> 
  select(zip = RegionName, everything(), -RegionID, -SizeRank, -MsaName) |> 
  pivot_longer(c(-zip), names_to = "date", values_to = "rent")

rm(zips)


# Table for neighbourhoods ------------------------------------------------

# Calculate rent increase as the magic number * the proportion of total listings
# created in a given year
rent_increase_zip <-
  property %>% 
  st_intersection(ZCTA) %>% 
  st_drop_geometry() %>% 
  group_by(zip) %>% 
  count(year = year(created)) %>% 
  filter(!is.na(year)) %>% 
  mutate(year = pmax(year, 2014)) %>%
  filter(year <= 2019) |> 
  group_by(zip, year) %>% 
  summarize(n = sum(n)) %>% 
  mutate(
    rent_increase = slide_dbl(n, ~{
      magic_value * .x[length(.x)] / sum(.x[-length(.x)])}, .before = n() - 1),
    rent_increase = if_else(is.infinite(rent_increase), NA_real_, 
                            rent_increase)) %>% 
  ungroup()

rent_increase_zip <- 
  zillow |> 
  mutate(year = as.numeric(substr(date, 1, 4))) |> 
  group_by(zip, year) |> 
  summarize(rent = mean(rent, na.rm = TRUE), .groups = "drop") |> 
  right_join(rent_increase_zip, by = c("zip", "year")) |> 
  filter(year >= 2015)

# Cumulative rent increase, 2015-2019
cum_rent <- 
  rent_increase_zip %>% 
  filter(!is.na(rent_increase)) %>% 
  group_by(zip) |> 
  summarize(total = prod(1 + rent_increase))


# Save output -------------------------------------------------------------

qsavem(rent_increase_zip, cum_rent, file = "output/rent_increases.qsm",
       nthreads = availableCores())

rm(magic_value)
