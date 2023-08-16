#########################################################################################
##----------------------------- Cohort Analysis ---------------------------------------##
#########################################################################################
setwd("/ANALYSE_AREA/P_INTG01")

library('haven')
library('dplyr')
library('tidyverse')

rp <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/rp.csv", show_col_types = FALSE)
rp <- subset(rp, rp$age >= 30 & rp$age <= 74)

data <- read.csv("Tempmory/final_data.csv")

diag <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/diag.csv", show_col_types = F)
diag <- subset(diag, date_diag >= "2011-01-01" & date_diag <= "2015-12-31")

death <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/dead.csv", show_col_types = F)
death <- subset(death, date_death >= "2011-01-01" & date_death <= "2015-12-31")
death$death <- ifelse(death$PP0040B > 0, 1, 0)

cvd_list <- c("K74","K75","K76","K77","K78","K83","K86","K87","K89","K90","K91","K92")

diag$cvd <- ifelse(diag$ICPCCODE %in% cvd_list, 1, 0)

#********* Target patients of 2011 (no angioplasty, no people living in nh, no death and no cvd) *********

gezo <- read_sas("/ANALYSE_AREA/P_INTG01/Shauni_AF/gezondheid_ima.sas7bdat")
bal_angio <- gezo[, c("SS00010", "BALLOON_ANGIOPLASTY","year")]
bal_angio_11 <- bal_angio %>% subset(year == "2011") %>% filter(BALLOON_ANGIOPLASTY == 0)


sum(bal_angio_11$BALLOON_ANGIOPLASTY == 1, na.rm = T) # 86 balloon angioplasty


sum(bal_angio_11$BALLOON_ANGIOPLASTY == 1) # 836 balloon angioplasty

nh <- read_sas("/ANALYSE_AREA/P_INTG01/Shauni_AF/verblijf_robrvt.sas7bdat")

death_11 <- death %>% subset(date_death >= "2011-01-01" & date_death <= "2011-12-31") %>%
  group_by(SS00010) %>% slice(1)

# REMOVING cvd==1
nocvd_11 <- diag %>% subset(date_diag >= "2011-01-01" & date_diag <= "2011-12-31") %>%
  filter(cvd !=1) %>% 
  group_by(DiagnoseID) %>% slice(1)



# Merge nocvd_11 and death_11

cvd_death_11 <- merge(x=nocvd_11, y=death_11, by.x= "DiagnoseID", by.y = "SS00010", all.x = T)
sum(cvd_death_11$death == 1, na.rm=T) # 308 deaths
nocvd_death_11 <- cvd_death_11[, c('DiagnoseID', 'cvd', 'death', 'date_diag')]

# REMOVING death
nocvd_death_11 <- nocvd_death_11 %>% mutate(death = case_when(is.na(death) == T ~ 0,
                                                              is.na(death) == F ~ 1)) %>% filter(death == 0)

nocvd_death_angio_11 <- merge(x=nocvd_death_11, y=bal_angio_11, by.x= "DiagnoseID", by.y = "SS00010", all.x = T)


# ADDING BALLOON ANGIOPLASTY == 0
nocvd_death_angio_11 <- nocvd_death_angio_11 %>% 
  mutate(BALLOON_ANGIOPLASTY = ifelse(is.na(BALLOON_ANGIOPLASTY), 0, BALLOON_ANGIOPLASTY))
sum(is.na(nocvd_death_angio_11$BALLOON_ANGIOPLASTY == 1))

nocvd_nodeath_noangio_nonh_11 <- merge(x=nocvd_death_angio_11, y=nh, by.x= "DiagnoseID", by.y = "ID", all.x = T)
sum(nocvd_nodeath_noangio_nonh_11$ROBRVT == 1, na.rm = T) # 1136 nursing home patients

# REMOVING nursing home patients
nocvd_nodeath_noangio_nonh_11 <- nocvd_nodeath_noangio_nonh_11 %>% 
  mutate(ROBRVT = ifelse(is.na(ROBRVT), 0, ROBRVT)) %>%
  filter(ROBRVT == 0)

# Merging the rp table from 2011 values only

rp_11 <- rp[, c("ID", "sex", "age", "height", "wgt_2011", "dbp_2011", "sbp_2011",
                "chl_2011", "ldl_2011", "hdl_2011", "ss_2011", "diabetes_2011")]
data_11 <- merge(x= nocvd_nodeath_noangio_nonh_11, y= rp_11, by.x = "DiagnoseID", by.y = "ID", all.x = T)
data_11 <- data_11[, c("DiagnoseID", "sex", "age", "height", "wgt_2011", "dbp_2011", 
                       "sbp_2011", "chl_2011", "ldl_2011", "hdl_2011", "ss_2011", "diabetes_2011", "date_diag")]
n_distinct(data_11$DiagnoseID) # 28176 distinct patients
data_11 <- rename(data_11, age_2011 = age)

# FILTER for age
data_11 <- subset(data_11, age_2011 >= 30 & age_2011 <= 74) # 22389
n_distinct(data_11$DiagnoseID) # 22389

summary(data_11)

# Correlation Matrix
par(mfrow = c(1, 1))
cor_matrix = cor(data_11[,sapply(data_11, is.numeric)], use="pairwise.complete.obs")
corrplot(cor_matrix, method="color")

###*********************************************************************************************************
###****************************  EXPLORATORY DYNAMIC COHORT ANALYSIS  *********************************#####
###*********************************************************************************************************

p_11 <- data_11[,"DiagnoseID"]

diag_12 <- diag %>% subset(date_diag >= "2012-01-01" & date_diag <= "2012-12-31") %>%
  subset(ICPCCODE %in% cvd_list) %>%
  subset(DiagnoseID %in% p_11) %>%
  group_by(DiagnoseID) %>% slice(1)

n_distinct(diag_12$DiagnoseID) # 845 cvd patients in 2012

death_12 <- death %>% subset(date_death >= "2012-01-01" & date_death <= "2012-12-31") %>%
  subset(SS00010 %in% p_11) %>% 
  group_by(SS00010) %>% slice(1)

n_distinct(death_12$SS00010) # 79 death in 2012

cvd_12 <- diag %>%
  subset(date_diag >= "2012-01-01" & date_diag <= "2012-12-31") %>%
  subset(DiagnoseID %in% p_11) %>%
  group_by(DiagnoseID) %>%
  filter(cvd == 1 | !any(cvd == 1)) %>%
  arrange(cvd) %>%
  slice(1)

n_distinct(cvd_12$DiagnoseID) # 17025
sum(cvd_12$cvd == 1)

#************************************** 2012 data ****************************************************

rp_12 <- rp[, c("ID", "sex", "age", "height", "wgt_2012", "dbp_2012", "sbp_2012",
                "chl_2012", "ldl_2012", "hdl_2012", "ss_2012", "diabetes_2012")]

# Merge death and cvd 
cvd_death_12 <- merge(x=cvd_12, y=death_12, by.x= "DiagnoseID", by.y = "SS00010", all.x = T)
cvd_death_12 <- cvd_death_12[,c("DiagnoseID", "date_diag","cvd", "date_death", "death")]

cvd_death_12 <- merge(x=cvd_death_12, y=data_11,by.x= "DiagnoseID", by.y = "DiagnoseID", all.x = T)
cvd_death_12 <- cvd_death_12[,c("DiagnoseID", "date_diag.y", "date_diag.x", "date_death","cvd", "death")]
names(cvd_death_12) <- c("DiagnoseID", "start_date", "date_diag", "date_death", "cvd", "death")

cvd_death_12 <- cvd_death_12 %>%
  mutate(death = ifelse(is.na(death), 0, death)) %>%
  mutate(status_date = case_when(
    cvd == 0 & death == 0 ~ "2012-12-31",
    cvd == 1 & death == 0 ~ as.character(date_diag),
    cvd == 0 & death == 1 ~ as.character(date_death),
    cvd == 1 & death == 1 ~ as.character(date_death))) %>%
  mutate(followup = as.numeric(interval(start_date, status_date), "days"))

sum(is.na(cvd_death_12$status_date))

cvd_death_12 <- cvd_death_12[,c("DiagnoseID", "start_date","status_date","cvd",  "death", "followup")]

# Merge rp_12 with cvd_death_12

data_12 <- merge(x= cvd_death_12, y= rp_12, by.x = "DiagnoseID", by.y = "ID", all.x = T)
data_12$age_2012 <- data_12$age + 1
data_12 <- data_12[, c("DiagnoseID", "sex", "age_2012", "height", "wgt_2012", "dbp_2012", 
                       "sbp_2012", "chl_2012", "ldl_2012", "hdl_2012", "ss_2012", "diabetes_2012", 
                       "start_date","status_date","cvd",  "death", "followup")]

data_12$status_date <- as.Date(data_12$status_date)
n_distinct(data_12$DiagnoseID) # 17025
summary(data_12)

# Correlation Matrix
par(mfrow = c(1, 1))
cor_matrix = cor(data_12[,sapply(data_12, is.numeric)], use="pairwise.complete.obs")
corrplot(cor_matrix, method="color")

############################## Visualizations ################################################

library(ggplot2)

# Grouped box plot for parameters by gender and death
ggplot(data_12, aes(x = sex, y = age, fill = factor(death))) +
  geom_boxplot(position = position_dodge()) +
  facet_wrap(~c("weight", "sbp", "chl"), scales = "free_y") +
  labs(x = "Gender", y = "Parameter Value", fill = "Death") +
  ggtitle("Parameter Distribution by Gender and Death") +
  theme_minimal()

library(ggplot2)

# Violin plot with overlayed box plot of age by gender and cvd
ggplot(data_12, aes(x = sex, y = age, fill = factor(cvd))) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +
  labs(x = "Gender", y = "Age", fill = "CVD") +
  ggtitle("Age Distribution by Gender and CVD") +
  theme_minimal()

library(ggplot2)

# Faceted bar plot of death count and cvd count by gender
ggplot(data_12, aes(x = factor(death), fill = factor(cvd))) +
  geom_bar() +
  labs(x = "Death", y = "Count", fill = "CVD") +
  ggtitle("Death and CVD Count by Gender") +
  theme_minimal() +
  facet_wrap(~ sex, nrow = 1)

############################## Contiuation of the cohort analysis ################################################################

##******************************************** 2013 data *********************************************

p_12 <- data_12 %>% subset(death == 0)
p_12 <- p_12[,"DiagnoseID"]

diag_13 <- diag %>% subset(date_diag >= "2013-01-01" & date_diag <= "2013-12-31") %>%
  subset(ICPCCODE %in% cvd_list) %>%
  subset(DiagnoseID %in% p_12) %>%
  group_by(DiagnoseID) %>% slice(1)

n_distinct(diag_13$DiagnoseID) # 657 cvd patients in 2013

death_13 <- death %>% subset(date_death >= "2013-01-01" & date_death <= "2013-12-31") %>%
  subset(SS00010 %in% p_12) %>% 
  group_by(SS00010) %>% slice(1)

n_distinct(death_13$SS00010) # 82 death in 2012

cvd_13 <- diag %>%
  subset(date_diag >= "2013-01-01" & date_diag <= "2013-12-31") %>%
  subset(DiagnoseID %in% p_12) %>%
  group_by(DiagnoseID) %>%
  filter(cvd == 1 | !any(cvd == 1)) %>%
  arrange(cvd) %>%
  slice(1)

n_distinct(cvd_13$DiagnoseID) # 13705
sum(cvd_13$cvd == 1)


rp_13 <- rp[, c("ID", "sex", "age", "height", "wgt_2013", "dbp_2013", "sbp_2013",
                "chl_2013", "ldl_2013", "hdl_2013", "ss_2013", "diabetes_2013")]

# Merge death and cvd 
cvd_death_13 <- merge(x=cvd_13, y=death_13, by.x= "DiagnoseID", by.y = "SS00010", all.x = T)
cvd_death_13 <- cvd_death_13[,c("DiagnoseID", "date_diag","cvd", "date_death", "death")]

cvd_death_13 <- merge(x=cvd_death_13, y=data_11,by.x= "DiagnoseID", by.y = "DiagnoseID", all.x = T)
cvd_death_13 <- cvd_death_13[,c("DiagnoseID", "date_diag.y", "date_diag.x", "date_death","cvd", "death")]
names(cvd_death_13) <- c("DiagnoseID", "start_date", "date_diag", "date_death", "cvd", "death")

library(lubridate)

# Convert date columns to appropriate date format
cvd_death_13$start_date <- as.Date(cvd_death_13$start_date)
cvd_death_13$date_diag <- as.Date(cvd_death_13$date_diag)
cvd_death_13$date_death <- as.Date(cvd_death_13$date_death)

# Replace missing values in death column with 0
cvd_death_13$death <- ifelse(is.na(cvd_death_13$death), 0, cvd_death_13$death)

# Calculate status_date and followup variables
cvd_death_13 <- cvd_death_13 %>%
  mutate(status_date = case_when(
    cvd == 0 & death == 0 ~ as.Date("2013-12-31"),
    cvd == 1 & death == 0 ~ date_diag,
    cvd == 0 & death == 1 ~ date_death,
    cvd == 1 & death == 1 ~ date_death)) %>%
  mutate(followup = as.numeric(interval(start_date, status_date), "days"))

# Print the updated dataset
cvd_death_13


cvd_death_13 <- cvd_death_13[,c("DiagnoseID", "start_date","status_date","cvd",  "death", "followup")]

# Merge rp_13 with cvd_death_13

data_13 <- merge(x= cvd_death_13, y= rp_13, by.x = "DiagnoseID", by.y = "ID", all.x = T)
data_13$age_2013 <- data_13$age + 2
data_13 <- data_13[, c("DiagnoseID", "sex", "age_2013", "height", "wgt_2013", "dbp_2013", 
                       "sbp_2013", "chl_2013", "ldl_2013", "hdl_2013", "ss_2013", "diabetes_2013", 
                       "start_date","status_date","cvd",  "death", "followup")]
n_distinct(data_13$DiagnoseID) # 13025
summary(data_13)

# Correlation Matrix
par(mfrow = c(1, 1))
cor_matrix = cor(data_13[,sapply(data_13, is.numeric)], use="pairwise.complete.obs")
corrplot(cor_matrix, method="color")

##*********************************** 2014 data **************************************************8

p_13 <- data_13 %>% subset(death == 0)
p_13 <- p_13[,"DiagnoseID"]

diag_14 <- diag %>% subset(date_diag >= "2014-01-01" & date_diag <= "2014-12-31") %>%
  subset(ICPCCODE %in% cvd_list) %>%
  subset(DiagnoseID %in% p_13) %>%
  group_by(DiagnoseID) %>% slice(1)

n_distinct(diag_14$DiagnoseID) # 488 cvd patients in 2014

death_14 <- death %>% subset(date_death >= "2014-01-01" & date_death <= "2014-12-31") %>%
  subset(SS00010 %in% p_13) %>% 
  group_by(SS00010) %>% slice(1)

n_distinct(death_14$SS00010) # 88 death in 2014

cvd_14 <- diag %>%
  subset(date_diag >= "2014-01-01" & date_diag <= "2014-12-31") %>%
  subset(DiagnoseID %in% p_13) %>%
  group_by(DiagnoseID) %>%
  filter(cvd == 1 | !any(cvd == 1)) %>%
  arrange(cvd) %>%
  slice(1)

n_distinct(cvd_14$DiagnoseID) # 11185
sum(cvd_14$cvd == 1)


rp_14 <- rp[, c("ID", "sex", "age", "height", "wgt_2014", "dbp_2014", "sbp_2014",
                "chl_2014", "ldl_2014", "hdl_2014", "ss_2014", "diabetes_2014")]

# Merge death and cvd 
cvd_death_14 <- merge(x=cvd_14, y=death_14, by.x= "DiagnoseID", by.y = "SS00010", all.x = T)
cvd_death_14 <- cvd_death_14[,c("DiagnoseID", "date_diag","cvd", "date_death", "death")]

cvd_death_14 <- merge(x=cvd_death_14, y=data_11,by.x= "DiagnoseID", by.y = "DiagnoseID", all.x = T)
cvd_death_14 <- cvd_death_14[,c("DiagnoseID", "date_diag.y", "date_diag.x", "date_death","cvd", "death")]
names(cvd_death_14) <- c("DiagnoseID", "start_date", "date_diag", "date_death", "cvd", "death")

library(lubridate)

# Convert date columns to appropriate date format
cvd_death_14$start_date <- as.Date(cvd_death_14$start_date)
cvd_death_14$date_diag <- as.Date(cvd_death_14$date_diag)
cvd_death_14$date_death <- as.Date(cvd_death_14$date_death)

# Replace missing values in death column with 0
cvd_death_14$death <- ifelse(is.na(cvd_death_14$death), 0, cvd_death_14$death)

# Calculate status_date and followup variables
cvd_death_14 <- cvd_death_14 %>%
  mutate(status_date = case_when(
    cvd == 0 & death == 0 ~ as.Date("2014-12-31"),
    cvd == 1 & death == 0 ~ date_diag,
    cvd == 0 & death == 1 ~ date_death,
    cvd == 1 & death == 1 ~ date_death)) %>%
  mutate(followup = as.numeric(interval(start_date, status_date), "days"))



cvd_death_14 <- cvd_death_14[,c("DiagnoseID", "start_date","status_date","cvd",  "death", "followup")]

# Merge rp_14 with cvd_death_14

data_14 <- merge(x= cvd_death_14, y= rp_14, by.x = "DiagnoseID", by.y = "ID", all.x = T)
data_14$age_2014 <- data_14$age + 3
data_14 <- data_14[, c("DiagnoseID", "sex", "age_2014", "height", "wgt_2014", "dbp_2014", 
                       "sbp_2014", "chl_2014", "ldl_2014", "hdl_2014", "ss_2014", "diabetes_2014", 
                       "start_date","status_date","cvd",  "death", "followup")]
n_distinct(data_14$DiagnoseID) # 14025
summary(data_14)

# Correlation Matrix
par(mfrow = c(1, 1))
cor_matrix = cor(data_14[,sapply(data_14, is.numeric)], use="pairwise.complete.obs")
corrplot(cor_matrix, method="color")

#*********************************** 2015 data ****************************************************

p_14 <- data_14 %>% subset(death == 0)
p_14 <- p_14[,"DiagnoseID"]

diag_15 <- diag %>% subset(date_diag >= "2015-01-01" & date_diag <= "2015-12-31") %>%
  subset(ICPCCODE %in% cvd_list) %>%
  subset(DiagnoseID %in% p_14) %>%
  group_by(DiagnoseID) %>% slice(1)

n_distinct(diag_15$DiagnoseID) # 449 cvd patients in 2015

death_15 <- death %>% subset(date_death >= "2015-01-01" & date_death <= "2015-12-31") %>%
  subset(SS00010 %in% p_14) %>% 
  group_by(SS00010) %>% slice(1)

n_distinct(death_15$SS00010) # 72 death in 2014

cvd_15 <- diag %>%
  subset(date_diag >= "2015-01-01" & date_diag <= "2015-12-31") %>%
  subset(DiagnoseID %in% p_14) %>%
  group_by(DiagnoseID) %>%
  filter(cvd == 1 | !any(cvd == 1)) %>%
  arrange(cvd) %>%
  slice(1)

n_distinct(cvd_15$DiagnoseID) # 9081
sum(cvd_15$cvd == 1)


rp_15 <- rp[, c("ID", "sex", "age", "height", "wgt_2015", "dbp_2015", "sbp_2015",
                "chl_2015", "ldl_2015", "hdl_2015", "ss_2015", "diabetes_2015")]

# Merge death and cvd 
cvd_death_15 <- merge(x=cvd_15, y=death_15, by.x= "DiagnoseID", by.y = "SS00010", all.x = T)
cvd_death_15 <- cvd_death_15[,c("DiagnoseID", "date_diag","cvd", "date_death", "death")]

cvd_death_15 <- merge(x=cvd_death_15, y=data_11,by.x= "DiagnoseID", by.y = "DiagnoseID", all.x = T)
cvd_death_15 <- cvd_death_15[,c("DiagnoseID", "date_diag.y", "date_diag.x", "date_death","cvd", "death")]
names(cvd_death_15) <- c("DiagnoseID", "start_date", "date_diag", "date_death", "cvd", "death")

library(lubridate)

# Convert date columns to appropriate date format
cvd_death_15$start_date <- as.Date(cvd_death_15$start_date)
cvd_death_15$date_diag <- as.Date(cvd_death_15$date_diag)
cvd_death_15$date_death <- as.Date(cvd_death_15$date_death)

# Replace missing values in death column with 0
cvd_death_15$death <- ifelse(is.na(cvd_death_15$death), 0, cvd_death_15$death)

# Calculate status_date and followup variables
cvd_death_15 <- cvd_death_15 %>%
  mutate(status_date = case_when(
    cvd == 0 & death == 0 ~ as.Date("2015-12-31"),
    cvd == 1 & death == 0 ~ date_diag,
    cvd == 0 & death == 1 ~ date_death,
    cvd == 1 & death == 1 ~ date_death)) %>%
  mutate(followup = as.numeric(interval(start_date, status_date), "days"))



cvd_death_15 <- cvd_death_15[,c("DiagnoseID", "start_date","status_date","cvd",  "death", "followup")]

# Merge rp_15 with cvd_death_15

data_15 <- merge(x= cvd_death_15, y= rp_15, by.x = "DiagnoseID", by.y = "ID", all.x = T)
data_15$age_2015 <- data_15$age + 4
data_15 <- data_15[, c("DiagnoseID", "sex", "age_2015", "height", "wgt_2015", "dbp_2015", 
                       "sbp_2015", "chl_2015", "ldl_2015", "hdl_2015", "ss_2015", "diabetes_2015", 
                       "start_date","status_date","cvd",  "death", "followup")]
n_distinct(data_15$DiagnoseID) # 15025
summary(data_15)

# Correlation Matrix
par(mfrow = c(1, 1))
cor_matrix = cor(data_15[,sapply(data_15, is.numeric)], use="pairwise.complete.obs")
corrplot(cor_matrix, method="color")


#---------------------- MERGING ALL THE DATASETS ---------------------------------

data_list <- list(data_11, data_12, data_13, data_14, data_15)
wide_data <- data_list %>% reduce(full_join, by = c('DiagnoseID'))
library(dplyr)
wide_data <- wide_data[ , c("DiagnoseID", "sex.x", "age_2011", "age_2012", "age_2013", "age_2014", "age_2015",
                            "sbp_2011", "sbp_2012", "sbp_2013", "sbp_2014", "sbp_2015", "dbp_2011",
                            "dbp_2012", "dbp_2013", "dbp_2014", "dbp_2015", "chl_2011", "chl_2012", "chl_2013",
                            "chl_2014", "chl_2015", "ldl_2011", "ldl_2012", "ldl_2013", "ldl_2014", "ldl_2015",
                            "hdl_2011", "hdl_2012", "hdl_2013", "hdl_2014", "hdl_2015", "ss_2011", "ss_2012",
                            "ss_2013", "ss_2014", "ss_2015", "diabetes_2011", "diabetes_2012", "diabetes_2013",
                            "diabetes_2014", "diabetes_2015","date_diag", "start_date.x", "start_date.y", "start_date.x.x",
                            "start_date.y.y", "status_date.x", "status_date.y", "status_date.x.x", "status_date.y.y",
                            "cvd.x", "cvd.y","cvd.x.x", "cvd.y.y", "death.x", "death.y", "death.x.x", "death.y.y",
                            "followup.x", "followup.y", "followup.x.x", "followup.y.y")]


wide_data$cvd_2011 <- 0
wide_data$death_2011 <- 0
wide_data$status_date_2011 <- wide_data$date_diag 
wide_data$`followup_2011` <- as.numeric(interval(wide_data$date_diag, wide_data$status_date_2011), "days")

names(wide_data) <- c("DiagnoseID", "sex", "age_2011", "age_2012", "age_2013","age_2014","age_2015",
                      "sbp_2011", "sbp_2012", "sbp_2013", "sbp_2014", "sbp_2015","dbp_2011", 
                      "dbp_2012", "dbp_2013", "dbp_2014",
                      "dbp_2015",
                      "chl_2011", "chl_2012", "chl_2013", "chl_2014", "chl_2015", "ldl_2011",
                      "ldl_2012", "ldl_2013", "ldl_2014", "ldl_2015", "hdl_2011", "hdl_2012", 
                      "hdl_2013", "hdl_2014", "hdl_2015", "ss_2011", "ss_2012", "ss_2013",
                      "ss_2014", "ss_2015", "diabetes_2011", "diabetes_2012", "diabetes_2013",
                      "diabetes_2014", "diabetes_2015", "start_date_2011", "start_date_2012", 
                      "start_date_2013", "start_date_2014", "start_date_2015", "status_date_2012", 
                      "status_date_2013", "status_date_2014", "status_date_2015", "cvd_2012", "cvd_2013",
                      "cvd_2014", "cvd_2015", "death_2012", "death_2013", "death_2014", "death_2015", 
                      "followup_2012", "followup_2013", "followup_2014", "followup_2015", "cvd_2011",
                      "death_2011", "status_date_2011", "followup_2011")

wide_data <- wide_data[,c("DiagnoseID", "sex", "age_2011", "age_2012", "age_2013", "age_2014",
                          "age_2015", "dbp_2011", "dbp_2012", "dbp_2013", "dbp_2014",
                          "dbp_2015", "sbp_2011", "sbp_2012", "sbp_2013", "sbp_2014", "sbp_2015",
                          "chl_2011", "chl_2012", "chl_2013", "chl_2014", "chl_2015", "ldl_2011",
                          "ldl_2012", "ldl_2013", "ldl_2014", "ldl_2015", "hdl_2011", "hdl_2012", 
                          "hdl_2013", "hdl_2014", "hdl_2015", "ss_2011", "ss_2012", "ss_2013",
                          "ss_2014", "ss_2015", "diabetes_2011", "diabetes_2012", "diabetes_2013",
                          "diabetes_2014", "diabetes_2015", "start_date_2011", "start_date_2012", 
                          "start_date_2013", "start_date_2014", "start_date_2015", "status_date_2011",
                          "status_date_2012", "status_date_2013", "status_date_2014", "status_date_2015",
                          "cvd_2011", "cvd_2012", "cvd_2013", "cvd_2014", "cvd_2015", "death_2011", 
                          "death_2012", "death_2013", "death_2014", "death_2015", "followup_2011",
                          "followup_2012", "followup_2013", "followup_2014", "followup_2015")]

library(base)
write.csv(data_11, file = "/ANALYSE_AREA/P_INTG01/Adhithya/Data/data_11.csv", row.names = FALSE)
write.csv(data_12, file = "/ANALYSE_AREA/P_INTG01/Adhithya/Data/data_12.csv", row.names = FALSE)
write.csv(data_13, file = "/ANALYSE_AREA/P_INTG01/Adhithya/Data/data_13.csv", row.names = FALSE)
write.csv(data_14, file = "/ANALYSE_AREA/P_INTG01/Adhithya/Data/data_14.csv", row.names = FALSE)
write.csv(data_15, file = "/ANALYSE_AREA/P_INTG01/Adhithya/Data/data_15.csv", row.names = FALSE)
write.csv(wide_data, file = "/ANALYSE_AREA/P_INTG01/Adhithya/Data/wide_data.csv", row.names = FALSE)

data1 <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/data_11.csv", show_col_types = F)
data2 <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/data_12.csv", show_col_types = F)
data3 <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/data_13.csv", show_col_types = F)
data4<- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/data_14.csv", show_col_types = F)
data5 <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/data_15.csv", show_col_types = F)
wide <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/wide_data.csv", show_col_types = F)


summary(data5)

n_distinct(data1)
sum(data1$sex == "M") 
sum(data1$sex == "F") 

n_distinct(data2)
sum(data2$sex == "M") 
sum(data2$sex == "F") 

n_distinct(data3)
sum(data3$sex == "M") 
sum(data3$sex == "F") 

n_distinct(data4)
sum(data4$sex == "M") 
sum(data4$sex == "F") 
sum(is.na(data4$DiagnoseID))

n_distinct(data5)
sum(data5$sex == "M") 
sum(data5$sex == "F") 

sum(data3$cvd == 1 & data3$sex == "M") 
sum(data3$cvd == 1 & data3$sex == "F") 

sum(data4$cvd == 1 & data4$sex == "M") 
sum(data4$cvd == 1 & data4$sex == "F") 

sum(data5$cvd == 1 & data5$sex == "M") 
sum(data5$cvd == 1 & data5$sex == "F") 


sum(data2$cvd == 1 & data2$sex == "M") 
sum(data2$cvd == 1 & data2$sex == "F") 

sum(data3$cvd == 1 & data3$sex == "M") 
sum(data3$cvd == 1 & data3$sex == "F") 

sum(data4$cvd == 1 & data4$sex == "M") 
sum(data4$cvd == 1 & data4$sex == "F") 

sum(data5$cvd == 1 & data5$sex == "M") 
sum(data5$cvd == 1 & data5$sex == "F")

####################### Visualizations ##################################################

# age distribution by gender over the years

mean_ages <- aggregate(age_2011 ~ sex, data = data1, FUN = mean)

# Create separate age distribution histograms for males and females
ggplot(data1, aes(x = age_2011, fill = sex)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.7) +
  geom_vline(data = mean_ages, aes(xintercept = age_2011, color = sex), linetype = "dashed", size = 0.9) +
  labs(x = "Age", y = "Frequency", title = "Age Distribution by Gender 2011") +
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("darkblue", "red")) +
  scale_color_manual(name = "Sex", values = c("black", "yellow")) +
  theme_linedraw()


mean_ages <- aggregate(age_2012 ~ sex, data = data2, FUN = mean)

# Separate age distribution histograms for males and females
ggplot(data2, aes(x = age_2012, fill = sex)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.7) +
  geom_vline(data = mean_ages, aes(xintercept = age_2012, color = sex), linetype = "dashed", size = 0.9) +
  labs(x = "Age", y = "Frequency", title = "Age Distribution by Gender 2012") +
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("darkblue", "red")) +
  scale_color_manual(name = "Sex", values = c("black", "yellow")) +
  theme_linedraw()


mean_ages <- aggregate(age_2013 ~ sex, data = data3, FUN = mean)

# Separate age distribution histograms for males and females
ggplot(data3, aes(x = age_2013, fill = sex)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.7) +
  geom_vline(data = mean_ages, aes(xintercept = age_2013, color = sex), linetype = "dashed", size = 0.9) +
  labs(x = "Age", y = "Frequency", title = "Age Distribution by Gender 2013") +
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("darkblue", "red")) +
  scale_color_manual(name = "Sex", values = c("black", "yellow")) +
  theme_linedraw()


mean_ages <- aggregate(age_2014 ~ sex, data = data4, FUN = mean)

# Separate age distribution histograms for males and females
ggplot(data4, aes(x = age_2014, fill = sex)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.7) +
  geom_vline(data = mean_ages, aes(xintercept = age_2014, color = sex), linetype = "dashed", size = 0.9) +
  labs(x = "Age", y = "Frequency", title = "Age Distribution by Gender 2014") +
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("darkblue", "red")) +
  scale_color_manual(name = "Sex", values = c("black", "yellow")) +
  theme_linedraw()

mean_ages <- aggregate(age_2015 ~ sex, data = data5, FUN = mean)

# Separate age distribution histograms for males and females
ggplot(data5, aes(x = age_2015, fill = sex)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.7) +
  geom_vline(data = mean_ages, aes(xintercept = age_2015, color = sex), linetype = "dashed", size = 0.9) +
  labs(x = "Age", y = "Frequency", title = "Age Distribution by Gender 2015") +
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("darkblue", "red")) +
  scale_color_manual(name = "Sex", values = c("black", "yellow")) +
  theme_linedraw()


mean_ages <- aggregate(age ~ sex, data = risk_pred1115, FUN = mean)

# Separate age distribution histograms for males and females
ggplot(risk_pred1115, aes(x = age, fill = sex)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.7) +
  geom_vline(data = mean_ages, aes(xintercept = age, color = sex), linetype = "dashed", size = 0.9) +
  labs(x = "Age", y = "Frequency", title = "Age Distribution by Gender 2011-2015") +
  scale_fill_manual(name = "Sex", labels = c("Male", "Female"), values = c("darkblue", "red")) +
  scale_color_manual(name = "Sex", values = c("black", "yellow")) +
  theme_linedraw()



histogram <- ggplot(risk_pred1115, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  facet_wrap(~ sex, ncol = 2) +
  labs(x = "Age", y = "Frequency", title = "Age distribution by gender 2011-2015") 
print(histogram)


################################################################################################




