#######################################################################################
#----------------------------- Imputation 30% ----------------------------------------#
#######################################################################################

setwd("/ANALYSE_AREA/P_INTG01")

library(nlme)
library(haven)
library(tidyverse)

wide <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/wide_data.csv", show_col_types = F)


str(wide)
set.seed(123)
sample_size <- floor(0.30 * nrow(wide))
sample_data <- wide[sample(sample_size),]

sum(sample_data$sex == "M") 
sum(sample_data$sex == "F") 



library(tidyverse)
long <- sample_data %>%
  pivot_longer(cols = -c(DiagnoseID, sex),
               names_to = c(".value", "year"),
               names_pattern = "(\\w+)_(\\d{4})")

long <- long %>% dplyr::select(-c(start_date, status_date, followup))
filtered_long <- long[rowSums(!is.na(long[, c("sbp", "dbp", "chl", "ldl", "hdl", "cvd", "ss",
                                              "diabetes", "death")])) > 0, ]
n_distinct(filtered_long$DiagnoseID) 

filtered_long <- filtered_long %>%
  mutate(status = ifelse(cvd == 1 | death == 1, 1, 0))
filtered_long$status <- as.numeric(filtered_long$status)
filtered_long <- filtered_long %>% mutate(year= recode(year, `2011` = 0, `2012` = 1, `2013` = 2,
                                                       `2014` = 3, `2015` = 4))

filtered_long1 <- filtered_long[complete.cases(filtered_long), ]
ls(filtered_long)
n_distinct(filtered_long1$DiagnoseID) 

# Exploring missing values

library(naniar)
pct_miss(sample_data)  

# Proportion of missing cases
pct_miss_case(sample_data) 


sample_data_male <- sample_data %>% filter(sex == "M")

# proportion of missing cases in males 
pct_miss(sample_data_male) 

sample_data_female <- sample_data %>% filter(sex == "F")

# proportion of missing cases in females 
pct_miss(sample_data_female) 

# Proportion of complete cases
pct_complete_case(sample_data) 

gg_miss_var(sample_data, show_pct = TRUE)

sample_data %>% 
  gg_miss_var(show_pct = TRUE, facet = death)

sample_data %>% 
  gg_miss_var(show_pct = TRUE, facet = cvd)

sample_data %>% 
  gg_miss_var(show_pct = TRUE, facet = sex)

vis_miss(sample_data, warn_large_data = FALSE)


#-------------------------------- Imputation long -------------------------------

wide <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/wide_data.csv", show_col_types = F)


str(wide)
set.seed(123)
sample_size <- floor(0.30 * nrow(wide))
sample_data <- wide[sample(sample_size),]

sum(sample_data$sex == "M")
sum(sample_data$sex == "F") 
n_distinct(long$DiagnoseID) 

library(tidyverse)
long <- sample_data %>%
  pivot_longer(cols = -c(DiagnoseID, sex),
               names_to = c(".value", "year"),
               names_pattern = "(\\w+)_(\\d{4})")

long1 <- long %>% dplyr::select(-c(start_date, status_date))
filtered_long <- long1[rowSums(!is.na(long1[, c("sbp", "dbp", "chl", "ldl", "hdl", "cvd",
                                                "death")])) > 0, ]

filtered_long <- filtered_long %>% mutate(ss = as.factor(ss),
                                          diabetes = as.factor(diabetes),
                                          year= recode(year, `2011` = 0, `2012` = 1, `2013` = 2,
                                                       `2014` = 3, `2015` = 4))

# Creating variables that represents missingness in the data
library(dplyr)

data <- filtered_long %>%
  mutate(
    R_sbp = ifelse(is.na(sbp), 1, 0),  # R_x represents missingness in variable x
    R_dbp = ifelse(is.na(dbp), 1, 0),  
    R_chl = ifelse(is.na(chl), 1, 0),
    R_hdl = ifelse(is.na(hdl), 1, 0),
    R_ldl = ifelse(is.na(ldl), 1, 0),
    R_ss = ifelse(is.na(ss), 1, 0),
    R_diabetes = ifelse(is.na(diabetes), 1, 0),
    age = scale(age, center = TRUE, scale = TRUE),
    year_numeric = recode(year,
                          `2011` = 0,
                          `2012` = 1,
                          `2013` = 2,
                          `2014` = 3,
                          `2015` = 4),
    sex = as.factor(sex)
  )

data <- data %>%
  group_by(DiagnoseID) %>%
  mutate(R_sbp_lag = lag(R_sbp, default = 0),
         R_dbp_lag = lag(R_dbp, default = 0),
         R_chl_lag = lag(R_chl, default = 0),
         R_hdl_lag = lag(R_hdl, default = 0),
         R_ldl_lag = lag(R_hdl, default = 0),
         R_ss_lag = lag(R_ss, default = 0),
         R_diabetes = lag(R_diabetes, default = 0)) %>%
  ungroup()


library(lme4)
m1 <- glmer(formula = R_sbp ~ year_numeric + age + R_sbp_lag +(1 | DiagnoseID),
            data = data,
            family = binomial(link = "logit"))

summary(m1)

m2 <- glmer(formula = R_dbp ~ year_numeric + age + R_dbp_lag +(1 | DiagnoseID),
            data = data,
            family = binomial(link = "logit"))

summary(m2)

m3 <- glmer(formula =  R_chl ~ year_numeric + age + R_chl_lag +(1 | DiagnoseID),
            data = data,
            family = binomial(link = "logit"))

summary(m3)

m4 <- glmer(formula =  R_hdl ~ year_numeric + age + R_hdl_lag + (1 | DiagnoseID),
            data = data,
            family = binomial(link = "logit"))

summary(m4)

m5 <- glmer(formula =  R_ldl ~ year_numeric + age + R_ldl_lag +(1 | DiagnoseID),
            data = data,
            family = binomial(link = "logit"))

summary(m5)

# Testing a model with complete case data to examine the fit

library(lme4)
library(lmerTest)

model <- lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl + (1 | DiagnoseID),
              data = filtered_long, na.action = na.exclude)
summary_model <- summary(model)
summary(model)
anova(model)


library(mice)
imp0 <- mice(filtered_long, maxit = 0)
imp0$loggedEvents
pred <- imp0$predictorMatrix
pred[, c('DiagnoseID', 'sex')] <- 0
head(pred)
meth <- imp0$method
meth

bin_var <- c("ss", "diabetes")

meth[bin_var] <- "logreg"
meth

mi <- mice(filtered_long, method = meth, predictorMatrix = pred, m = 10,
           maxit = 20, seed = 123, printFlag = FALSE)

# Iteration 1
iter1 <- mice::complete(mi, 1)
summary(iter1)
model <- lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl + (1 | DiagnoseID),
              data = iter1)
anova(model)

# Check the range
numeric_cols <- sapply(iter1, is.numeric)

# Calculate range for numeric columns
numeric_range <- sapply(iter1[, numeric_cols], range)
print(numeric_range)


# Iteration 2
iter2 <- mice:: complete(mi, 2)

model <- lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl + (1 | DiagnoseID),
              data = iter2)
anova(model)

# Check the range
numeric_cols <- sapply(iter2, is.numeric)

# Calculate range for numeric columns
numeric_range <- sapply(iter2[, numeric_cols], range)
print(numeric_range)

# Iteration 3

iter3 <- mice:: complete(mi, 3)

model <- lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl + (1 | DiagnoseID),
              data = iter3)
anova(model)

# Check the range
numeric_cols <- sapply(iter3, is.numeric)

# Calculate range for numeric columns
numeric_range <- sapply(iter3[, numeric_cols], range)
print(numeric_range)

# Iteration 4

iter4 <- mice:: complete(mi, 4)

model <- lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl + (1 | DiagnoseID),
              data = iter4)
anova(model)

# Check the range
numeric_cols <- sapply(iter4, is.numeric)

# Calculate range for numeric columns
numeric_range <- sapply(iter4[, numeric_cols], range)
print(numeric_range)

# Iteration 5

iter5 <- mice:: complete(mi, 5)

model <- lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl + (1 | DiagnoseID),
              data = iter5)
anova(model)

# Check the range
numeric_cols <- sapply(iter5, is.numeric)

# Calculate range for numeric columns
numeric_range <- sapply(iter5[, numeric_cols], range)
print(numeric_range)

# Iteration 6

iter6 <- mice:: complete(mi, 6)

model <- lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl + (1 | DiagnoseID),
              data = iter6)
anova(model)

# Check the range
numeric_cols <- sapply(iter6, is.numeric)

# Calculate range for numeric columns
numeric_range <- sapply(iter6[, numeric_cols], range)
print(numeric_range)

# Iteration 7

iter7 <- mice:: complete(mi, 7)

model <- lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl + (1 | DiagnoseID),
              data = iter7)
anova(model)

# Check the range
numeric_cols <- sapply(iter7, is.numeric)

# Calculate range for numeric columns
numeric_range <- sapply(iter7[, numeric_cols], range)
print(numeric_range)

# Iteration 8

iter8 <- mice:: complete(mi, 8)

model <- lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl + (1 | DiagnoseID),
              data = iter8)
anova(model)

# Check the range
numeric_cols <- sapply(iter8, is.numeric)

# Calculate range for numeric columns
numeric_range <- sapply(iter8[, numeric_cols], range)
print(numeric_range)

# Iteration 9

iter9 <- mice:: complete(mi, 9)

model <- lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl + (1 | DiagnoseID),
              data = iter9)
anova(model)

# Check the range
numeric_cols <- sapply(iter9, is.numeric)

# Calculate range for numeric columns
numeric_range <- sapply(iter9[, numeric_cols], range)
print(numeric_range)

# Iteration 10

iter10 <- mice:: complete(mi, 10)

model <- lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl + (1 | DiagnoseID),
              data = iter10)
anova(model)

# Check the range
numeric_cols <- sapply(iter10, is.numeric)

# Calculate range for numeric columns
numeric_range <- sapply(iter10[, numeric_cols], range)
print(numeric_range)

# Stripplots

stripplot(mi, sbp)
stripplot(mi, dbp)
stripplot(mi, hdl)
stripplot(mi, ldl)
stripplot(mi, chl)
stripplot(mi, ss)
stripplot(mi, diabetes)

library("broom.mixed")

fit <- with(mi, 
            lmer(chl ~ 1 + as.factor(ss) + as.factor(diabetes) + age + sbp + dbp + hdl 
                 + (1 | DiagnoseID)))
summary(pool(fit), conf.int = TRUE)

# Pool the results
fit_pooled <- pool(fit)

final_data <- complete(fit_pooled, action = "long")
write.csv(final_data, file = "/ANALYSE_AREA/P_INTG01/Adhithya/Data/final_data.csv", row.names = FALSE)

