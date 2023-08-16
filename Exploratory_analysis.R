########################################################################################
#----------------------------- Exploratory Analysis -----------------------------------#
########################################################################################


setwd("/ANALYSE_AREA/P_INTG01")

data <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/wide_data.csv", show_col_types = F)
data <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/risk_pred1115.csv", show_col_types = F)
data <- subset(data, age>=30 & age<=74)
data <- subset(data, age_2011>=30 & age_2011<=74)

# Number of males and females
sum(data$sex == "M") # 10440
sum(data$sex == "F") # 11949

# Creating sex specific data
males <- subset(data, sex == "M")
females <- subset(data, sex == "F")

# Mean age
mean(females$age_2011) # 53.6
mean(males$age_2011) #53.9

# Mean systolic Blood Pressure for males and females

# MALE
sbp_male_mean <- rowMeans(males[, c("sbp_2011", "sbp_2012", "sbp_2013", "sbp_2014", "sbp_2015")],
                          na.rm = T)
mean(sbp_male_mean, na.rm = T) # 130.33

#FEMALE
sbp_female_mean <- rowMeans(females[, c("sbp_2011", "sbp_2012", "sbp_2013", "sbp_2014", "sbp_2015")],
                            na.rm =T)
mean(sbp_female_mean, na.rm = T) #128.221

# Diastolic blood pressure mean for males and females

#MALE
dbp_male_mean <- rowMeans(males[, c("dbp_2011", "dbp_2012", "dbp_2013", "dbp_2014", "dbp_2015")],
                          na.rm = T)
mean(dbp_male_mean, na.rm = T) # 80.01

#FEMALE
dbp_female_mean <- rowMeans(females[, c("dbp_2011", "dbp_2012", "dbp_2013", "dbp_2014", "dbp_2015")],
                            na.rm = T)
mean(dbp_female_mean, na.rm = T) #78.44

# Total cholesterol mean for males and females

#MALE
chl_male_mean <- rowMeans(males[, c("chl_2011", "chl_2012", "chl_2013", "chl_2014", "chl_2015")], 
                          na.rm = T)
mean(chl_male_mean, na.rm = T) # 197.886

#FEMALE
chl_female_mean <- rowMeans(females[, c("chl_2011", "chl_2012", "chl_2013", "chl_2014", "chl_2015")],
                            na.rm = T)
mean(chl_female_mean, na.rm = T) # 208.79


# LDL means for male and female

#MALE
ldl_male_mean <- rowMeans(males[, c("ldl_2011", "ldl_2012", "ldl_2013", "ldl_2014", "ldl_2015")],
                          na.rm = T)
mean(ldl_male_mean, na.rm = T) # 117.71

#FEMALE
ldl_female_mean <- rowMeans(females[, c("ldl_2011", "ldl_2012", "ldl_2013", "ldl_2014", "ldl_2015")],
                            na.rm = T)
mean(ldl_female_mean, na.rm = T) #121.68

# HDL means for males and females

# MALE
hdl_male_mean <- rowMeans(males[, c("hdl_2011", "hdl_2012", "hdl_2013", "hdl_2014", "hdl_2015")],
                          na.rm = T)
mean(hdl_male_mean, na.rm = T) # 50.50

#FEMALE
hdl_female_mean <- rowMeans(females[, c("hdl_2011", "hdl_2012", "hdl_2013", "hdl_2014", "hdl_2015")],
                            na.rm = T)
mean(hdl_female_mean, na.rm = T) # 63.24


# Count of smokers and non-smokers in Males and Females
data$smoker <- ifelse(rowSums(data[c("ss_2011", "ss_2012", "ss_2013", "ss_2014", "ss_2015")], na.rm=T)>=1,
                      1, 0)
ggplot(data, aes(x = sex, fill = factor(cvd))) +
  geom_bar() +
  labs(x = "sex", y = "Count", fill = "CVD Event") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No Event", "Event")) +
  theme_minimal()

# Checking the age distribution in the data
library(apyramid)
age_breaks <- seq(30, 75, by = 5)
age_labels <- paste(age_breaks[-length(age_breaks)], age_breaks[-1], sep = "-")
data$age_cat <- cut(data$age_2011, breaks = age_breaks, labels = age_labels, include.lowest = TRUE)
apyramid::age_pyramid(data = data,
                      age_group = "age_cat",
                      split_by = "sex",
                      proportional = TRUE,
                      show_midpoint = FALSE,            
                      pal = c("orange", "darkgreen"))

library(naniar)
library(visdat)

# Visualizations in cohort analysis is in cohort_analysis.R file

# -------------- Longitudinal explorations ------------------------------------------------

wide <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/wide_data.csv", show_col_types = F)
str(wide)
set.seed(123)
sample_size <- floor(0.30 * nrow(wide))
sample_data <- wide[sample(sample_size),]


long <- sample_data %>%
  pivot_longer(cols = -c(DiagnoseID, sex),
               names_to = c(".value", "year"),
               names_pattern = "(\\w+)_(\\d{4})")
ls(long)
long <- long %>% mutate(age = as.numeric(age),
                        chl = as.numeric(chl),
                        cvd = as.factor(cvd),
                        dbp = as.numeric(dbp),
                        death = as.factor(death),
                        diabetes = as.factor(diabetes),
                        followup = as.numeric(followup),
                        hdl = as.numeric(hdl),
                        ldl = as.numeric(ldl),
                        sbp = as.numeric(sbp),
                        sex = as.factor(sex),
                        ss = as.factor(ss),
                        DiagnoseID = as.character(DiagnoseID))

filtered_long <- long[rowSums(!is.na(long[, c("sbp", "dbp", "chl", "ldl", "hdl", "ss", "diabetes", "cvd",
                                              "death")])) > 0, ]
# Select 30 random males
random_males <- sample(unique(filtered_long$DiagnoseID[long$sex == "M"]), 100, replace = TRUE)

# Select 30 random females
random_females <- sample(unique(filtered_long$DiagnoseID[long$sex == "F"]), 100, replace = TRUE)

# Filter the data for the selected males and females
selected_data <- filtered_long[filtered_long$DiagnoseID %in% c(random_males, random_females), ]


selected_data$followup <- as.numeric(selected_data$followup*0.0329) #Follow-up days to months 
selected_data$followup <- floor(selected_data$followup)
selected_data <- selected_data %>% mutate(year= recode(year, `2011` = 0, `2012` = 1, `2013` = 2,
                                                       `2014` = 3, `2015` = 4),
                                          sex = as.factor(sex))

filtered_long <- filtered_long %>% mutate(year= recode(year, `2011` = 0, `2012` = 1, `2013` = 2,
                                                       `2014` = 3, `2015` = 4))
# SBP

library(ggplot2)
ggplot(filtered_long, aes(x = year, y = sbp)) +
  geom_line(aes(group=DiagnoseID)) +
  geom_point(aes(fill=as.factor(DiagnoseID)), pch=21, size=1, stroke=1) +
  facet_wrap(~sex) +
  scale_x_continuous(name = "sbp over the follow-up time", breaks=c(0:48)) +
  scale_y_continuous(name = "Systolic Blood pressure",limits=c(50,250)) +
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"),
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none")



#DBP

ggplot(filtered_long, aes(x = year, y = dbp)) +
  geom_line(aes(group=DiagnoseID)) +
  geom_point(aes(fill=as.factor(DiagnoseID)), pch=21, size=1, stroke=1) +
  facet_wrap(~sex) +
  scale_x_continuous(name = "dbp over the follow-up time", breaks=c(0:48)) +
  scale_y_continuous(name = "Diastolic Blood pressure",limits=c(0,150)) +
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"),
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none")

#CHL

ggplot(filtered_long, aes(x = year, y = chl)) +
  geom_line(aes(group=DiagnoseID)) +
  geom_point(aes(fill=as.factor(DiagnoseID)), pch=21, size=1, stroke=1) +
  facet_wrap(~sex) +
  scale_x_continuous(name = "chl over the follow-up time", breaks=c(0:48)) +
  scale_y_continuous(name = "Total Cholesterol",limits=c(50,315)) +
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"),
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none")


#HDL

ggplot(filtered_long, aes(x = year, y = hdl)) +
  geom_line(aes(group=DiagnoseID)) +
  geom_point(aes(fill=as.factor(DiagnoseID)), pch=21, size=1, stroke=1) +
  facet_wrap(~sex) +
  scale_x_continuous(name = "hdl over the follow-up time", breaks=c(0:48)) +
  scale_y_continuous(name = "High density lipoprotein",limits=c(0,175)) +
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"),
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none")

#LDL

ggplot(filtered_long, aes(x = year, y = ldl)) +
  geom_line(aes(group=DiagnoseID)) +
  geom_point(aes(fill=as.factor(DiagnoseID)), pch=21, size=1, stroke=1) +
  facet_wrap(~sex) +
  scale_x_continuous(name = "ldl over the follow-up time", breaks=c(0:48)) +
  scale_y_continuous(name = "Low density lipoprotein",limits=c(0,175)) +
  theme_bw() + theme(axis.text.x=element_text(size=8, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"),
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none")

#--------------- SBP ----------------------------

summary_data <- filtered_long %>%
  group_by(year, sex) %>%
  summarise(mean_outcome = mean(sbp, na.rm = TRUE),
            se_outcome = sd(sbp, na.rm = TRUE) / sqrt(sum(!is.na(sbp))))

ggplot(summary_data, aes(x = year, y = mean_outcome)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_outcome - 1.96 * se_outcome,
                    ymax = mean_outcome + 1.96 * se_outcome),
                width = 0.1) +
  facet_wrap(~ sex, ncol = 1, scales = "free_y") +
  labs(title = "Systolic Blood Pressure",
       x = "Time",
       y = "SBP Mean Outcome") +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 12),
        legend.position = "none")


#----------------- DBP -------------------

summary_data <- filtered_long %>%
  group_by(year, sex) %>%
  summarise(mean_outcome = mean(dbp, na.rm = TRUE),
            se_outcome = sd(dbp, na.rm = TRUE) / sqrt(sum(!is.na(dbp))))

ggplot(summary_data, aes(x = year, y = mean_outcome)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_outcome - 1.96 * se_outcome,
                    ymax = mean_outcome + 1.96 * se_outcome),
                width = 0.1) +
  facet_wrap(~ sex, ncol = 1, scales = "free_y") +
  labs(title = "Diastolic Blood Pressure",
       x = "Time",
       y = "DBP Mean Outcome") +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 12),
        legend.position = "none")


#--------------- CHL ---------------------

summary_data <- filtered_long %>%
  group_by(year, sex) %>%
  summarise(mean_outcome = mean(chl, na.rm = TRUE),
            se_outcome = sd(chl, na.rm = TRUE) / sqrt(sum(!is.na(chl))))

ggplot(summary_data, aes(x = year, y = mean_outcome)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_outcome - 1.96 * se_outcome,
                    ymax = mean_outcome + 1.96 * se_outcome),
                width = 0.1) +
  facet_wrap(~ sex, ncol = 1, scales = "free_y") +
  labs(title = "Total Cholesterol",
       x = "Time",
       y = "CHL Mean Outcome") +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 12),
        legend.position = "none")

#--------------- LDL -------------

summary_data <- filtered_long %>%
  group_by(year, sex) %>%
  summarise(mean_outcome = mean(ldl, na.rm = TRUE),
            se_outcome = sd(ldl, na.rm = TRUE) / sqrt(sum(!is.na(ldl))))

ggplot(summary_data, aes(x = year, y = mean_outcome)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_outcome - 1.96 * se_outcome,
                    ymax = mean_outcome + 1.96 * se_outcome),
                width = 0.1) +
  facet_wrap(~ sex, ncol = 1, scales = "free_y") +
  labs(title = "Low-density lipoprotein",
       x = "Time",
       y = "LDL Mean Outcome") +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 12),
        legend.position = "none")


#------------- HDL -----------------

summary_data <- filtered_long %>%
  group_by(year, sex) %>%
  summarise(mean_outcome = mean(hdl, na.rm = TRUE),
            se_outcome = sd(hdl, na.rm = TRUE) / sqrt(sum(!is.na(hdl))))

ggplot(summary_data, aes(x = year, y = mean_outcome)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_outcome - 1.96 * se_outcome,
                    ymax = mean_outcome + 1.96 * se_outcome),
                width = 0.1) +
  facet_wrap(~ sex, ncol = 1, scales = "free_y") +
  labs(title = "High-density lipoprotein",
       x = "Time",
       y = "HDL Mean Outcome") +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 12),
        legend.position = "none")

#--------------- Mean-levels ----------------------------------

risk_factors <- c("dbp", "sbp", "chl", "ldl", "hdl")
longitudinal_data <- long %>%
  group_by(year) %>%
  summarise(across(all_of(risk_factors), ~mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = -year, names_to = "risk_factor", values_to = "value")

# Mean levels of Continuous variables
ggplot(longitudinal_data, aes(x = year, y = value, color = risk_factor)) +
  geom_point() +
  labs(x = "Year", y = "Mean Level", color = "Risk Factor") +
  ggtitle("Longitudinal Analysis: Mean Levels of Risk Factor Predictors over Time")