#### 11 FREH MODEL #############################################################

source("R/01_startup.R")
library(caret)
plan(multisession, workers = 10)


# Load and consolidate data -----------------------------------------------

qload("output/str_processed.qsm", nthreads = availableCores())


# Prepare daily file ------------------------------------------------------

daily <-
  daily %>%
  mutate(year = year(date),
         month = month(date))


# Get traditional FREH status ---------------------------------------------

FREH <-
  daily %>%
  strr_FREH() %>%
  filter(FREH)


# Produce monthly activity table for all EH listings ----------------------

monthly <-
  daily %>%
  filter(listing_type == "Entire home/apt") %>%
  left_join(FREH, by = c("property_ID", "date")) %>%
  mutate(FREH = coalesce(FREH, FALSE)) %>%
  left_join(select(property, property_ID, created),
            by = "property_ID") %>%
  # Trim listings to the start of the month
  mutate(created = if_else(day(created) == 1, created,
                           floor_date(created, "month") %m+% months(1))) %>%
  filter(date >= created) %>%
  mutate(created_year = year(created),
         created_month = month(created),
         month_since_created = (year - created_year) * 12 +
           (month - created_month)) %>%
  group_by(property_ID, year, month) %>%
  summarize(month_since_created = first(month_since_created),
            R = sum(status == "R"),
            A = sum(status == "A"),
            B = sum(status == "B"),
            FREH = as.logical(ceiling(mean(FREH))),
            .groups = "drop")


# Summarize by month ------------------------------------------------------

after_one_year <-
  monthly %>%
  filter(year <= 2019) %>%
  mutate(month = month.name[.data$month],
         AR = A + R) %>%
  group_by(property_ID) %>%
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(R_3), !is.na(AR_3), month_since_created >= 12)


# Fit models and apply to listings > 2 months -----------------------------

model_3 <- glm(FREH ~ R_3 + AR_3 + month, data = after_one_year,
               family = binomial)

model_3_results <-
  monthly %>%
  mutate(month = month.name[.data$month],
         AR = A + R) %>%
  group_by(property_ID) %>%
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(R_3), !is.na(AR_3)) %>%
  modelr::add_predictions(model_3, type = "response") %>%
  mutate(FREH_3 = pred) %>%
  select(-pred) %>%
  rowwise() %>%
  mutate(month = which(month.name == month)) %>%
  ungroup()

daily <-
  daily %>%
  left_join(select(model_3_results, property_ID, year, month, FREH_3),
            by = c("property_ID", "year", "month")) %>%
  mutate(FREH_3 = coalesce(FREH_3, 0)) %>% 
  select(-year, -month)


# # Model testing -----------------------------------------------------------
#
# # Split the data into training and test set
# training_samples_3 <-
#   after_one_year$FREH %>% createDataPartition(p = 0.80, list = FALSE)
#
# train_data_3 <- after_one_year[training_samples_3, ]
# test_data_3 <- after_one_year[-training_samples_3, ]
#
# # Fit the model
# model_3_test <- glm(FREH ~ R_3 + AR_3 + month, data = train_data_3,
#                           family = binomial)
#
# # Test models
# probabilities_3 <- model_3_test %>% predict(test_data_3, type = "response")
# predicted_classes_3 <- ifelse(probabilities_3 > 0.5, "TRUE", "FALSE")
# mean(predicted_classes_3 == test_data_3$FREH)
# # Outcome: 0.858


# Save output -------------------------------------------------------------

qsave(daily, file = "output/daily.qs", nthreads = availableCores())
qsavem(FREH, monthly, after_one_year, model_3, model_3_results, 
       file = "output/FREH_model.qsm", nthreads = availableCores())

rm(FREH, monthly, after_one_year, model_3, model_3_results, test_data_3,
   train_data_3, training_samples_3, model_3_test, predicted_classes_3,
   probabilities_3)
