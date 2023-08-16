###############################################################################################
##---------------------------------- Data Extraction -----------------------------------------#
###############################################################################################

setwd("/ANALYSE_AREA/P_INTG01")
library('haven')
library('dplyr')
library('tidyverse')

# load measurements dataset to extract sbp, dbp, wgt, hgt and ss
measurements <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/measurements.csv", show_col_types = F)

# Systolic Blood Pressure

sbp <- measurements %>% subset(MEASURE_TEST == "SYSTOLIC BLOOD PRESSURE") %>% 
  mutate(Date = as.Date(MTWDAT, "%d/ %m/ %Y")) %>% 
  subset(Date >= "2011-01-01" & Date <= "2015-12-31") %>%
  select(c(CPROJECT, Date, MEASURE_VALUE))

sbp11 <- subset(sbp, Date >= "2011-01-01" & Date <= "2011-12-31") # sbp for patients in 2011
n_distinct(sbp11$CPROJECT) # 19618

sbp12 <- subset(sbp, Date >= "2012-01-01" & Date <= "2012-12-31") # sbp for patients in 2012
n_distinct(sbp12$CPROJECT) # 20591

sbp13 <- subset(sbp, Date >= "2013-01-01" & Date <= "2013-12-31") # sbp for patients in 2013
n_distinct(sbp13$CPROJECT) # 20610

sbp14 <- subset(sbp, Date >= "2014-01-01" & Date <= "2014-12-31") # sbp for patients in 2014
n_distinct(sbp14$CPROJECT) # 21689

sbp15 <- subset(sbp, Date >= "2015-01-01" & Date <= "2015-12-31") # sbp for patients in 2015
n_distinct(sbp15$CPROJECT) # 21030

# Selecting latest reading of the risk parameters from 2011-15

library(lubridate)
sbp11 <- sbp11 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
sbp12 <- sbp12 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
sbp13 <- sbp13 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
sbp14 <- sbp14 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
sbp15 <- sbp15 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)

sbp_list <- list(sbp11, sbp12, sbp13, sbp14, sbp15) # list of sbp in 2011-15

sbp1115 <- sbp_list %>% reduce(full_join, by = 'CPROJECT') # Merging sbp from 2011-15
ls(sbp1115)

sbp1115 <- sbp1115[, c("CPROJECT", "MEASURE_VALUE.x", "MEASURE_VALUE.y", "MEASURE_VALUE.x.x",
                       "MEASURE_VALUE.y.y", "MEASURE_VALUE")]
names(sbp1115) <- c("ID", "sbp_2011", "sbp_2012", "sbp_2013", "sbp_2014", "sbp_2015")

write.csv(sbp1115, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/sbp1115.csv", row.names=FALSE)

# Diastolic Blood Pressure

dbp <- measurements %>% subset(MEASURE_TEST == "DIASTOLIC BLOOD PRESSURE") %>%
  mutate(Date = as.Date(MTWDAT, "%d/ %m/ %Y")) %>%
  subset(Date >= "2011-01-01" & Date <= "2015-12-31") %>%
  select(c(CPROJECT, Date, MEASURE_VALUE))

dbp11 <- subset(dbp, Date >= "2011-01-01" & Date <= "2011-12-31") # dbp for patients 2011
n_distinct(dbp11$CPROJECT)

dbp12 <- subset(dbp, Date >= "2012-01-01" & Date <= "2012-12-31") # dbp for patients 2012
n_distinct(dbp12$CPROJECT)

dbp13 <- subset(dbp, Date >= "2013-01-01" & Date <= "2013-12-31") # dbp for patients 2013

dbp14 <- subset(dbp, Date >= "2014-01-01" & Date <= "2014-12-31") # dbp for patients 2014

dbp15 <- subset(dbp, Date >= "2015-01-01" & Date <= "2015-12-31") # dbp for patients 2015

# Selecting latest reading of the risk parameters from 2011-15

library(lubridate)
dbp11 <- dbp11 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
dbp12 <- dbp12 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
dbp13 <- dbp13 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
dbp14 <- dbp14 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
dbp15 <- dbp15 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)


dbp_list <- list(dbp11, dbp12, dbp13, dbp14, dbp15)

dbp1115 <- dbp_list %>% reduce(full_join, by = 'CPROJECT')
ls(dbp1115)

dbp1115 <- dbp1115[, c("CPROJECT", "MEASURE_VALUE.x", "MEASURE_VALUE.y", "MEASURE_VALUE.x.x",
                       "MEASURE_VALUE.y.y", "MEASURE_VALUE")]
names(dbp1115) <- c("ID", "dbp_2011", "dbp_2012", "dbp_2013", "dbp_2014", "dbp_2015")

write.csv(dbp1115, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/dbp1115.csv", row.names=FALSE)

# Weight

wgt <- measurements %>% subset(MEASURE_TEST == "WEIGHT") %>%
  mutate(Date = as.Date(MTWDAT, "%d/ %m/ %Y")) %>%
  subset(Date >= "2011-01-01" & Date <= "2015-12-31") %>%
  select(c(CPROJECT, Date, MEASURE_VALUE))

wgt11 <- subset(wgt, Date >= "2011-01-01" & Date <= "2011-12-31") # wgt for patients 2011

wgt12 <- subset(wgt, Date >= "2012-01-01" & Date <= "2012-12-31") # wgt for patients 2012

wgt13 <- subset(wgt, Date >= "2013-01-01" & Date <= "2013-12-31") # wgt for patients 2013

wgt14 <- subset(wgt, Date >= "2014-01-01" & Date <= "2014-12-31") # wgt for patients 2014

wgt15 <- subset(wgt, Date >= "2015-01-01" & Date <= "2015-12-31") # # wgt for patients 2015

# Selecting latest reading of the risk parameters from 2011-15

wgt11 <- wgt11 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
wgt12 <- wgt12 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
wgt13 <- wgt13 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
wgt14 <- wgt14 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)
wgt15 <- wgt15 %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1)

wgt_list <- list(wgt11, wgt12, wgt13, wgt14, wgt15)

wgt1115 <- wgt_list %>% reduce(full_join, by = 'CPROJECT') # Merging 
ls(wgt1115)

wgt1115 <- wgt1115[, c("CPROJECT", "MEASURE_VALUE.x", "MEASURE_VALUE.y", "MEASURE_VALUE.x.x",
                       "MEASURE_VALUE.y.y", "MEASURE_VALUE")]
names(wgt1115) <- c("ID", "wgt_2011", "wgt_2012", "wgt_2013", "wgt_2014", "wgt_2015")

write.csv(wgt1115, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/wgt1115.csv", row.names=FALSE)


# Height

hgt <- measurements %>% subset(MEASURE_TEST == "HEIGHT") %>%
  mutate(Date = as.Date(MTWDAT, "%d/ %m/ %Y")) %>%
  subset(Date >= "2011-01-01" & Date <= "2015-12-31") %>%
  select(c(CPROJECT, Date, MEASURE_VALUE))

hgt <- hgt %>% group_by(CPROJECT) %>% mutate(Date = max(Date)) %>% slice(1) # last reading of hgt 
names(hgt) <- c("ID", "Date", "height")
n_distinct(hgt$ID)

# Smoking Status

ss <- measurements %>% subset(MEASURE_TEST == "SMOKING STATUS") %>%
  subset(MTWDAT >= "2011" & MTWDAT <= "2015") %>%
  select(c(CPROJECT, MTWDAT, MEASURE_VALUE)) %>%
  mutate(MEASURE_VALUE = ifelse(MEASURE_VALUE == 'smoker', 1, 0)) %>% #ex-smoker & smoker = 1
  rename(Year = MTWDAT)

ss11 <- subset(ss, Year == '2011') # ss for patients 2011
ss12 <- subset(ss, Year == '2012') # ss for patients 2012
ss13 <- subset(ss, Year == '2013') # ss for patients 2013
ss14 <- subset(ss, Year == '2014') # ss for patients 2014
ss15 <- subset(ss, Year == '2015') # ss for patients 2015

# Selecting latest reading of the risk parameters from 2011-15

ss11 <- ss11 %>% group_by(CPROJECT) %>% mutate(Year = max(Year)) %>% slice(1)
as.data.frame(ss11)
write.csv(ss11, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/ss11.csv", row.names=FALSE)

ss12 <- ss12 %>% group_by(CPROJECT) %>% mutate(Year = max(Year)) %>% slice(1)
as.data.frame(ss12)
write.csv(ss12, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/ss12.csv", row.names=FALSE)

ss13 <- ss13 %>% group_by(CPROJECT) %>% mutate(Year = max(Year)) %>% slice(1)
as.data.frame(ss13)
write.csv(ss13, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/ss13.csv", row.names=FALSE)

ss14 <- ss14 %>% group_by(CPROJECT) %>% mutate(Year = max(Year)) %>% slice(1)
as.data.frame(ss14)
write.csv(ss14, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/ss14.csv", row.names=FALSE)

ss15 <- ss15 %>% group_by(CPROJECT) %>% mutate(Year = max(Year)) %>% slice(1)
as.data.frame(ss15)
write.csv(ss15, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/ss15.csv", row.names=FALSE)

ss_list <- list(ss11, ss12, ss13, ss14, ss15)
ss1115 <- ss_list %>% reduce(full_join, by = 'CPROJECT')
ls(ss1115)

ss1115 <- ss1115[, c("CPROJECT", "MEASURE_VALUE.x", "MEASURE_VALUE.y", "MEASURE_VALUE.x.x",
                     "MEASURE_VALUE.y.y", "MEASURE_VALUE")]
names(ss1115) <- c("ID", "ss_2011", "ss_2012", "ss_2013", "ss_2014", "ss_2015")
write.csv(ss1115, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/ss1115.csv", row.names=FALSE)


# Load laboratory dataset to extract variables chl, ldl and hdl
laboratory <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/laboratory.csv", show_col_types = F)

# Total cholestrol

chl <- laboratory %>% subset(LAB_TEST == "TOTAL_CHOLESTEROL") %>%
  mutate(LABDAT = as.Date(LABDAT)) %>%
  subset(LABDAT >= "2011-01-01" & LABDAT <= "2015-12-31") %>%
  select(c(CPROJECT, LABDAT, LAB_VALUE))
n_distinct(chl$CPROJECT)

chl11 <- subset(chl, LABDAT >= "2011-01-01" & LABDAT <= "2011-12-31") # chl for patients 2011
chl12 <- subset(chl, LABDAT >= "2012-01-01" & LABDAT <= "2012-12-31") # chl for patients 2012
chl13 <- subset(chl, LABDAT >= "2013-01-01" & LABDAT <= "2013-12-31") # chl for patients 2013
chl14 <- subset(chl, LABDAT >= "2014-01-01" & LABDAT <= "2014-12-31") # chl for patients 2014
chl15 <- subset(chl, LABDAT >= "2015-01-01" & LABDAT <= "2015-12-31") # chl for patients 2015

# Selecting latest reading of the risk parameters from 2011-15

chl11 <- chl11 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
chl12 <- chl12 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
chl13 <- chl13 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
chl14 <- chl14 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
chl15 <- chl15 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)


chl_list <- list(chl11, chl12, chl13, chl14, chl15)
chl1115 <- chl_list %>% reduce(full_join, by = 'CPROJECT')
ls(chl1115)

chl1115 <- chl1115[, c("CPROJECT", "LAB_VALUE.x", "LAB_VALUE.y", "LAB_VALUE.x.x",
                       "LAB_VALUE.y.y", "LAB_VALUE")]
names(chl1115) <- c("ID", "chl_2011", "chl_2012", "chl_2013", "chl_2014", "chl_2015")

write.csv(chl1115, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/chl1115.csv", row.names=FALSE)


# HDL 

hdl <- laboratory %>% subset(LAB_TEST == "HDL_CHOLESTEROL") %>%
  mutate(LABDAT = as.Date(hdl$LABDAT)) %>%
  subset(LABDAT >= "2011-01-01" & LABDAT <= "2015-12-31") %>%
  select(c(CPROJECT, LABDAT, LAB_VALUE))

hdl11 <- subset(hdl, LABDAT >= "2011-01-01" & LABDAT <= "2011-12-31") # hdl for patients 2011
hdl12 <- subset(hdl, LABDAT >= "2012-01-01" & LABDAT <= "2012-12-31") # hdl for patients 2012
hdl13 <- subset(hdl, LABDAT >= "2013-01-01" & LABDAT <= "2013-12-31") # hdl for patients 2013
hdl14 <- subset(hdl, LABDAT >= "2014-01-01" & LABDAT <= "2014-12-31") # hdl for patients 2014
hdl15 <- subset(hdl, LABDAT >= "2015-01-01" & LABDAT <= "2015-12-31") # hdl for patients 2015

# Selecting latest reading of the risk parameters from 2011-15

hdl11 <- hdl11 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
hdl12 <- hdl12 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
hdl13 <- hdl13 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
hdl14 <- hdl14 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
hdl15 <- hdl15 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)

library(tidyverse)
hdl_list <- list(hdl11, hdl12, hdl13, hdl14, hdl15)
hdl1115 <- hdl_list %>% reduce(full_join, by = 'CPROJECT')
ls(hdl1115)

hdl1115 <- hdl1115[, c("CPROJECT", "LAB_VALUE.x", "LAB_VALUE.y", "LAB_VALUE.x.x",
                       "LAB_VALUE.y.y", "LAB_VALUE")]
names(hdl1115) <- c("ID", "hdl_2011", "hdl_2012", "hdl_2013", "hdl_2014", "hdl_2015")

write.csv(hdl1115, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/hdl1115.csv", row.names=FALSE)


# LDL 

ldl <- laboratory %>% subset(LAB_TEST == "LDL_CHOLESTEROL") %>%
  mutate(LABDAT = as.Date(LABDAT)) %>%
  subset(LABDAT >= "2011-01-01" & LABDAT <= "2015-12-31") %>%
  select(c(CPROJECT, LABDAT, LAB_VALUE))

ldl11 <- subset(ldl, LABDAT >= "2011-01-01" & LABDAT <= "2011-12-31") # ldl for patients 2011
ldl12 <- subset(ldl, LABDAT >= "2012-01-01" & LABDAT <= "2012-12-31") # ldl for patients 2012
ldl13 <- subset(ldl, LABDAT >= "2013-01-01" & LABDAT <= "2013-12-31") # ldl for patients 2013
ldl14 <- subset(ldl, LABDAT >= "2014-01-01" & LABDAT <= "2014-12-31") # ldl for patients 2014
ldl15 <- subset(ldl, LABDAT >= "2015-01-01" & LABDAT <= "2015-12-31") # ldl for patients 2015

# Selecting latest reading of the risk parameters from 2011-15

ldl11 <- ldl11 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
ldl12 <- ldl12 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
ldl13 <- ldl13 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
ldl14 <- ldl14 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)
ldl15 <- ldl15 %>% group_by(CPROJECT) %>% mutate(LABDAT = max(LABDAT)) %>% slice(1)


ldl_list <- list(ldl11, ldl12, ldl13, ldl14, ldl15)
ldl1115 <- ldl_list %>% reduce(full_join, by = 'CPROJECT')
ls(ldl1115)

ldl1115 <- ldl1115[, c("CPROJECT", "LAB_VALUE.x", "LAB_VALUE.y", "LAB_VALUE.x.x",
                       "LAB_VALUE.y.y", "LAB_VALUE")]
names(ldl1115) <- c("ID", "ldl_2011", "ldl_2012", "ldl_2013", "ldl_2014", "ldl_2015")

write.csv(ldl1115, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/ldl1115.csv", row.names=FALSE)

# Diagnosis data to extract diabetes variable

diag <- read_csv("/ANALYSE_AREA/P_INTG01/Adhithya/Data/diag.csv", show_col_types = F)
diag <- filter(diag, date_diag >= "2011-01-01" & date_diag <= "2015-12-31")

# Diabetes

diab <- c("T89", "T90") # ICPC codes to filter diabetes 
diabetes <- subset(diag, diag$ICPCCODE %in% diab) 

diab11 <- diabetes %>% subset(date_diag >= "2011-01-01" & date_diag <= "2011-12-31") %>%
  mutate(diabetes = ifelse(ICPCCODE == 'NA', 0, 1)) 

diab12 <- diabetes %>% subset(date_diag >= "2012-01-01" & date_diag <= "2012-12-31") %>%
  mutate(diabetes = ifelse(ICPCCODE == 'NA', 0, 1))

diab13 <- diabetes %>% subset(date_diag >= "2013-01-01" & date_diag <= "2013-12-31") %>%
  mutate(diabetes = ifelse(ICPCCODE == 'NA', 0, 1))

diab14 <- diabetes %>% subset(date_diag >= "2014-01-01" & date_diag <= "2014-12-31") %>%
  mutate(diabetes = ifelse(ICPCCODE == 'NA', 0, 1))

diab15 <- diabetes %>% subset(date_diag >= "2015-01-01" & date_diag <= "2015-12-31") %>%
  mutate(diabetes = ifelse(ICPCCODE == 'NA', 0, 1))

diab11 <- diab11 %>% group_by(DiagnoseID) %>% mutate(date_diag = max(date_diag)) %>% slice(1)
as.data.frame(diab11)
write.csv(diab11, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/diab11.csv", row.names=FALSE)

diab12 <- diab12 %>% group_by(DiagnoseID) %>% mutate(date_diag = max(date_diag)) %>% slice(1)
as.data.frame(diab12)
write.csv(diab12, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/diab12.csv", row.names=FALSE)

diab13 <- diab13 %>% group_by(DiagnoseID) %>% mutate(date_diag = max(date_diag)) %>% slice(1)
as.data.frame(diab13)
write.csv(diab13, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/diab13.csv", row.names=FALSE)

diab14 <- diab14 %>% group_by(DiagnoseID) %>% mutate(date_diag = max(date_diag)) %>% slice(1)
as.data.frame(diab14)
write.csv(diab14, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/diab14.csv", row.names=FALSE)

diab15 <- diab15 %>% group_by(DiagnoseID) %>% mutate(date_diag = max(date_diag)) %>% slice(1)
as.data.frame(diab15)
write.csv(diab15, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/diab15.csv", row.names=FALSE)


diab_list <- list(diab11, diab12, diab13, diab14, diab15)
diab1115 <- diab_list %>% reduce(full_join, by = 'DiagnoseID')
ls(diab1115)

diab1115 <- diab1115[, c("DiagnoseID", "diabetes.x", "diabetes.y", "diabetes.x.x",
                         "diabetes.y.y", "diabetes")]
names(diab1115) <- c("ID", "diabetes_2011", "diabetes_2012", "diabetes_2013", 
                     "diabetes_2014", "diabetes_2015")
# Assuming 0 to people who did not mark diabetes 
diab1115 <- diab1115 %>% mutate(diabetes_2011 = case_when(is.na(diabetes_2011) == T ~ 0,
                                                          is.na(diabetes_2011) == F ~ 1),
                                diabetes_2012 = case_when(is.na(diabetes_2012) == T ~ 0,
                                                          is.na(diabetes_2012) == F ~ 1),
                                diabetes_2013 = case_when(is.na(diabetes_2013) == T ~ 0,
                                                          is.na(diabetes_2013) == F ~ 1),
                                diabetes_2014 = case_when(is.na(diabetes_2014) == T ~ 0,
                                                          is.na(diabetes_2014) == F ~ 1),
                                diabetes_2015 = case_when(is.na(diabetes_2015) == T ~ 0,
                                                          is.na(diabetes_2015) == F ~ 1))

write.csv(diab1115, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/diab1115.csv", row.names=FALSE)

na_counts <- colSums(is.na(diab1115))
na_counts

#------------ reading ATC Codes ---------------------------------

############# Year by year #############
atc2011 <- read.csv("/ANALYSE_AREA/P_INTG01/Adhithya/IG_MEDICATIE2011_H20_V01.CSV", sep = ";")[c("SS00010","ATC")]
atc2012 <- read.csv("/ANALYSE_AREA/P_INTG01/Adhithya/IG_MEDICATIE2012_H20_V01.CSV", sep = ";")[c("SS00010","ATC")]
atc2013 <- read.csv("/ANALYSE_AREA/P_INTG01/Adhithya/IG_MEDICATIE2013_H20_V01.CSV", sep = ";")[c("SS00010","ATC")]
atc2014 <- read.csv("/ANALYSE_AREA/P_INTG01/Adhithya/IG_MEDICATIE2014_H20_V01.CSV", sep = ";")[c("SS00010","ATC")]
atc2015 <- read.csv("/ANALYSE_AREA/P_INTG01/Adhithya/IG_MEDICATIE2015_H20_V01.CSV", sep = ";")[c("SS00010","ATC")]


atc2011$ATC <- as.character(atc2011$ATC)
atc2011 <- subset(atc2011, startsWith(ATC, c("C02", "C03", "C07", "C08", "C09")))

atc2012$ATC <- as.character(atc2012$ATC)
atc2012 <- subset(atc2012, startsWith(ATC, c("C02", "C03", "C07", "C08", "C09")))

atc2013$ATC <- as.character(atc2013$ATC)
atc2013 <- subset(atc2013, startsWith(ATC, c("C02", "C03", "C07", "C08", "C09")))

atc2014$ATC <- as.character(atc2014$ATC)
atc2014 <- subset(atc2014, startsWith(ATC, c("C02", "C03", "C07", "C08", "C09")))

atc2015$ATC <- as.character(atc2015$ATC)
atc2015 <- subset(atc2015, startsWith(ATC, c("C02", "C03", "C07", "C08", "C09")))

atc_list <- list(atc2011, atc2012, atc2013, atc2014, atc2015)
atc_code <- atc_list %>% reduce(full_join, by = 'SS00010') %>% dplyr::select("SS00010", "ATC.x")

# Removing duplicates from a data frame
atc_code <- distinct(atc_code, .keep_all = TRUE)
names(atc_code) <- c("DiagnoseID", "ATC")


# Data - risk parameters

rp_list <- list(hgt, wgt1115, sbp1115, dbp1115, chl1115, hdl1115, ldl1115, ss1115, diab1115, atc_code)
rp <- rp_list %>% reduce(full_join, by = 'ID') # Merging all the datasets
rp <- distinct(rp, DiagnoseID, .keep_all = TRUE)

# Transforming medication column into a binary variable
rp$medication <- ifelse(is.na(rp$ATC.x), 0, 1)

rp <- rp[, -2] # need "gender" and "age"

# Patients dataset for gender and date of birth

patients <- read_sas("/ANALYSE_AREA/P_INTG01/patients_ima.sas7bdat")
patients <- patients %>% subset(patients, patients$ID %in% rp$ID) %>%
  subset(!duplicated(ID)) %>%
  mutate(year = 2011, age = year - Gebjr_ima) %>% 
  select(c(ID, sex_ima, age)) %>%
  rename(sex = sex_ima)


rp <- merge(x = patients, y = rp, by.x = "ID", by.y = "ID", all.x = T) # Merge Age and sex to rp data
write.csv(rp, "/ANALYSE_AREA/P_INTG01/Adhithya/Data/rp.csv", row.names=FALSE)