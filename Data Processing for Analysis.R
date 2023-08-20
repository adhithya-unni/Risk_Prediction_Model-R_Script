library(tidyverse)
library(corrplot)
library(lubridate)

# Load the data
data <- read_delim("final_data.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
str(data)

set.seed(123)


# Determine the number of rows for the 20% sample
sample_size <- floor(0.30 * nrow(data))

# Draw the random sample
sample_data <- data[sample(sample_size),]

final_data <- sample_data %>%
  pivot_longer(cols = -c(DiagnoseID, sex, status, ...1, cvd, death, followup, medication, age, status_date, date_diag),
               names_to = c(".value", "year"),
               names_pattern = "(\\w+)_(\\d{4})")

final_data$year = as.numeric(final_data$year)

final_data =  subset(final_data, !is.na(year))

final_data = subset(final_data, select = -c(...1, ldl, wgt, status_date, date_diag))

final_data$treated_bp = ifelse(final_data$medication == 1, final_data$sbp, 0)
final_data$untreated_bp = ifelse(final_data$medication == 0, final_data$sbp, 0)

final_data$months = (final_data$year - 2011) * 12

surv =  subset(final_data, year == 2011)

# Create the status_cvd and status_death variables with proper censoring for competing events
surv <- surv %>%
  mutate(
    status_cvd = ifelse(cvd == 1 & death == 0, 1, 
                        ifelse(cvd == 0 & death == 1, 0, cvd)),
    status_death = ifelse(death == 1 & cvd == 0, 1, 
                          ifelse(cvd == 1 & death == 0, 0, death))
  )


#Split train and validation test
set.seed(123)
# Create a list of unique patients in the longitudinal dataset
patient_ids <- unique(final_data$DiagnoseID)

# Determine the number of patients
num_patients <- length(patient_ids)

# Set the size of the train set based on the number of patients (70% for training)
train_size <- floor(0.7 * num_patients)

# Sample the patient IDs for the training set
train_ids <- sample(patient_ids, size = train_size)

# The rest of the IDs are for the test set
test_ids <- setdiff(patient_ids, train_ids)

# Subset both the longitudinal and survival datasets using the train and test IDs
final_data_train <- final_data[final_data$DiagnoseID %in% train_ids,]
survival_train <- surv[surv$DiagnoseID %in% train_ids,]

final_data_test <- final_data[final_data$DiagnoseID %in% test_ids,]
survival_test  <- surv[surv$DiagnoseID %in% test_ids,]

