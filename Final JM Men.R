library(JMbayes2)
library(glmulti)

#------------- For men ------------------#
long_data_men <- subset(final_data_train, sex == "M")
survival_data_men <- subset(survival_train, sex == "M")
long_data_men_val = subset(final_data_test, sex == "M")

#--------------- Function for computing time dependent AUC for Joint Models --------------------#
compute_aucs <- function(model, data) {
  # Define the time intervals
  intervals <- list(
    c(0, 12),
    c(12, 12),
    c(24, 12),
    c(36, 12),
    c(48, 10)
  )
  
  # Iterate through the time intervals, compute ROC and AUC, and print the results
  for (i in 1:length(intervals)) {
    Tstart <- intervals[[i]][1]
    Dt <- intervals[[i]][2]
    
    roc <- tvROC(model, newdata = data, Tstart = Tstart, Dt = Dt, cores = 10L, type_weights='IPCW')
    auc <- tvAUC(roc)$auc
    
    cat("AUC for time interval", Tstart, "to", Tstart + Dt, "is:", auc, "\n")
  }
}


#----------------- LMM For Men, ---------------#

# Define custom fitting function for lme
lme_fit <- function(formula, data, random) {
  lme(fixed = formula, data = data, random = random, control = lmeControl(opt="optim"))
}
random_effects <- ~ 1 | DiagnoseID

#-------------- Cholesterol -----------------#

# Define fixed effects and random effects formula
fixed_effects <- log(chl) ~ months + age + as.factor(diabetes) + as.factor(ss) + sbp + dbp + hdl + as.factor(medication)

# Run glmulti function
glmulti_chl_aic <- glmulti(
  y = fixed_effects,
  data = long_data_men,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)

opt_chl_men =  lme(log(chl)~1+months+age+dbp+hdl, data = long_data_men, random = ~ 1 | DiagnoseID)
plot(fitted(opt_chl_men), resid(opt_chl_men))

# ------------ HDL -------- #

# Finding optimal model based on AIC and BIC

# Define fixed effects and random effects formula
fixed_effects_hdl <- hdl ~ months + age + as.factor(diabetes) + as.factor(ss) + sbp + dbp + chl + as.factor(medication)

# Run glmulti function
glmulti_hdl_aic <- glmulti(
  y = fixed_effects_hdl,
  data = long_data_men,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)


opt_hdl_men =  lme(hdl~1+as.factor(medication)+age+dbp+chl, data = long_data_men, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

# ---------- SBP --------- #

# Define fixed effects and random effects formula
fixed_effects_sbp <- sbp ~ months + age + as.factor(diabetes) + as.factor(ss) + chl + dbp + hdl + as.factor(medication)

# Run glmulti function
glmulti_treated_bp_aic <- glmulti(
  y = fixed_effects_sbp,
  data = long_data_men,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)

# --------------- DBP --------------- #
opt_sbp_men =  lme(sbp~1+as.factor(medication)+months+age+chl+dbp, data = long_data_men, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

# Define fixed effects and random effects formula
fixed_effects_dbp <- dbp ~ months + age + as.factor(diabetes) + as.factor(ss) + sbp + chl + hdl + as.factor(medication)

# Run glmulti function
glmulti_treated_bp_aic <- glmulti(
  y = fixed_effects_dbp,
  data = long_data_men,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)

opt_dbp_men =  lme(dbp~1+age+sbp+chl+hdl, data = long_data_men, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))


# --------------- Untreated SBP --------------- #

# Define fixed effects and random effects formula
fixed_effects_untreated_bp <- untreated_bp ~ months + age + as.factor(diabetes) + as.factor(ss) +  treated_bp + hdl + chl + dbp

# Run glmulti function
glmulti_untreated_bp_aic <- glmulti(
  y = fixed_effects_untreated_bp,
  data = long_data_men,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)

opt_untreated_bp_men =  lme(untreated_bp~1+months+age+treated_bp+chl+dbp, data = long_data_men, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

# --------------- Treated SBP --------------- #

# Define fixed effects and random effects formula
fixed_effects_treated_bp <- treated_bp ~  months + age + as.factor(diabetes) + as.factor(ss) +  untreated_bp + hdl + chl + dbp

# Run glmulti function
glmulti_treated_bp_aic <- glmulti(
  y = fixed_effects_treated_bp,
  data = long_data_men,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)

opt_treated_bp_men =  lme(treated_bp~1+age+untreated_bp+dbp, data = long_data_men, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))


#------------- Optimals LMM models ---------------#
opt_chl_men =  lme(chl~1+as.factor(ss)+as.factor(medication)+months+age+sbp+dbp+hdl, 
                   data = long_data_men, random = ~ 1 | DiagnoseID)

plot(fitted(opt_chl_men), resid(opt_chl_men))


opt_hdl_men =  lme(hdl~1+as.factor(medication)+age+dbp+chl, 
                   data = long_data_men, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

plot(fitted(opt_hdl_men), resid(opt_hdl_men))

opt_sbp_men =  lme(sbp~1+as.factor(medication)+months+age+chl+dbp, 
                   data = long_data_men, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

plot(fitted(opt_sbp_men), resid(opt_sbp_men))


opt_dbp_men =  lme(dbp~1+age+sbp+chl+hdl, 
                   data = long_data_men, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

plot(fitted(opt_dbp_men), resid(opt_dbp_men))


opt_untreated_bp_men =  lme(untreated_bp~1+months+age+treated_bp+chl+dbp, 
                            data = long_data_men, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

plot(fitted(opt_untreated_bp_men), resid(opt_untreated_bp_men))


opt_treated_bp_men =  lme(treated_bp~1+age+untreated_bp+dbp, 
                          data = long_data_men, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

plot(fitted(opt_treated_bp_men), resid(opt_treated_bp_men))

#--------------- Survival Sub Models ---------------------#

Cox_men_cvd <- coxph(Surv(time = followup, 
                            event = status) ~ age + as.factor(diabetes) + as.factor(ss), + as.factor(medication), 
                       data = survival_data_men, 
                       x = TRUE)

Cox_men_cvd_1 <- coxph(Surv(time = followup, 
                              event = status) ~ age + as.factor(ss), 
                         data = survival_data_men, 
                         x = TRUE)

Cox_men_fram <- coxph(Surv(time = followup, 
                             event = status) ~ age + as.factor(diabetes) + as.factor(ss), 
                        data = survival_data_men, 
                        x = TRUE)


fForms1<- list("chl" = ~ slope(chl) + value(chl),
               "hdl" = ~ slope(hdl) + value(hdl),
               "treated_bp" = ~ slope(treated_bp) + value(treated_bp),
               "untreated_bp" = ~ slope(untreated_bp) + value(untreated_bp))


#---------------- Joint Model Framingham ------------------#
fForms1<- list("chl" = ~ slope(chl) + value(chl),
               "hdl" = ~ slope(hdl) + value(hdl),
               "treated_bp" = ~ slope(treated_bp) + value(treated_bp),
               "untreated_bp" = ~ slope(untreated_bp) + value(untreated_bp))




JM_opt_men_fram <- jm(Cox_men_fram, 
                        list(opt_treated_bp_men,opt_untreated_bp_men,opt_chl_men,opt_hdl_men), 
                        time_var = "months", 
                        n_iter = 5000L,  
                        n_burnin = 200L, 
                        n_thin = 2L,
                        n_chains = 4L,
                        control = list(GK_k = 7,             
                                       cores = 11,           
                                       save_random_effects = FALSE),
                        functional_forms = fForms1
)

summary(JM_opt_men_fram)

#---------------- Joint Model ------------------#

fForms <- list("chl" = ~ slope(chl) + value(chl),
               #"hdl" = ~ slope(hdl) + value(hdl),
               "sbp" = ~ slope(sbp) + value(sbp),
               "dbp" = ~ slope(dbp) + value(dbp))

JM_opt_men_1 <- jm(Cox_men_cvd, 
                     list(opt_chl_men,opt_hdl_men,opt_sbp_men,opt_dbp_men), 
                     time_var = "months", 
                     n_iter = 4000L,  
                     n_burnin = 200L, 
                     n_thin = 2L,
                     n_chains = 4L,
                     control = list(GK_k = 7,             
                                    cores = 11,           
                                    save_random_effects = FALSE),
                     functional_forms = fForms
)

summary(JM_opt_men_1)



JM_opt_men_2 <- jm(Cox_men_cvd, 
                   list(opt_sbp_men,opt_dbp_men), 
                   time_var = "months", 
                   n_iter = 8000L,  
                   n_burnin = 200L, 
                   n_thin = 2L,
                   n_chains = 4L,
                   control = list(GK_k = 7,             
                                  cores = 11,           
                                  save_random_effects = FALSE),
                   functional_forms = fForms
)

summary(JM_opt_men_2)

JM_opt_men_3 <- jm(Cox_men_cvd_1, 
                   list(opt_sbp_men,opt_dbp_men, opt_chl_men), 
                   time_var = "months", 
                   n_iter = 8000L,  
                   n_burnin = 200L, 
                   n_thin = 2L,
                   n_chains = 4L,
                   control = list(GK_k = 7,             
                                  cores = 11,           
                                  save_random_effects = FALSE),
                   functional_forms = fForms
)

summary(JM_opt_men_3)

JM_opt_men_4 <- jm(Cox_men_cvd_1, 
                   list(opt_sbp_men,opt_dbp_men, opt_chl_men), 
                   time_var = "months", 
                   n_iter = 8000L,  
                   n_burnin = 200L, 
                   n_thin = 2L,
                   n_chains = 4L,
                   control = list(GK_k = 7,             
                                  cores = 11,           
                                  save_random_effects = FALSE),
                   functional_forms = fForms
)

summary(JM_opt_men_4)


# Model Evaluations
compute_aucs(JM_opt_men_2, long_data_men)
compute_aucs(JM_opt_men_fram, long_data_men_val)

#---------------- Dynamic Risks Predictions ------------------#
t0 <- 12
ND <- survival_data_men
ND <- ND[ND$months <= t0, ]
ND$status <- 0
ND$followup <- t0

t1 <- 36
ND1 <- survival_data_men
ND1 <- ND1[ND1$months <= t1, ]
ND1$status <- 0
ND1$followup <- t1

predLong <- predict(JM_opt_men_2, newdata = ND,
                    times = seq(t0, 59, by=2),
                    return_newdata = TRUE, cores=11)

predLong1 <- predict(JM_opt_men_2, newdata = ND1,
                     times = seq(t1, 59, by=2),
                     return_newdata = TRUE, cores=11)

predSurv <- predict(JM_opt_men_2, newdata = ND, process = "event",
                    times = seq(t0, 59, by=2),
                    return_newdata = TRUE, cores=11)

predSurv1 <- predict(JM_opt_men_2, newdata = ND1, process = "event",
                     times = seq(t1, 59, by=2),
                     return_newdata = TRUE, cores=11)


plot(predLong, predSurv,outcomes = 1:2)

plot(predLong1, predSurv1,outcomes = 1:2)

#----------------- Model Comparison -------------------#
auc_JM_men <- data.frame(
  Time = rep(c(12, 24, 36, 48, 58), 2),
  AUC = c(0.5075558, 0.4407315, 0.5093931, 0.414772, 0.4983361, 
          0.5055026, 0.4229509, 0.4830718, 0.4408087, 0.497935),
  Model = rep(c("Optimized JM", "Framingham JM"), each = 5)
)

print(auc_JM_women)

plot <- ggplot(auc_JM_men, aes(x=Time, y=AUC, color=Model)) +
  geom_line(size=1.2) +
  geom_point(size=3) +
  scale_color_manual(values=c("Framingham JM"="blue", "Optimized JM"="red")) +
  labs(title="Time-Dependent AUC between Joint Models",
       subtitle="AUC values for Framingham Model and Optimized Model For Men",
       x="Time",
       y="AUC",
       color="Models") +
  theme_minimal() + theme(
    text = element_text(size=16),
    legend.title = element_text(size=16, face="bold"),
    legend.position = c(0.80, 0.90),
    legend.text = element_text(size=14),# Top-right positioning
    plot.title = element_text(hjust = 0.5, face="bold", size=18), # Increased size
    plot.subtitle = element_text(hjust = 0.5, size=15), # Increased size
    axis.title.x = element_text(size=16, face="bold"), # Increased size for x-axis label
    axis.title.y = element_text(size=16, face="bold"),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)# Increased size for y-axis label
  )
plot = plot + scale_x_continuous(breaks = c(12, 24, 36, 48, 58), 
                                 labels = c("12 months", "24 months", "36 months", "48 months", "58 months"))
plot


#-------------------------- Model Comparison JM ------------------------#
auc_men_opt = data.frame(
  Time = rep(c(12, 24, 36, 48, 58), 2),
  AUC = c(0.5479267, 0.5681126, 0.5824520, 0.6036994, 0.5744366, 
          0.5075558, 0.4407315, 0.5093931, 0.414772, 0.4983361),
  Model = rep(c("Optimized Static", "Optimized JM"), each = 5)
)

plot <- ggplot(auc_men_opt, aes(x=Time, y=AUC, color=Model)) +
  geom_line(size=1.2) +
  geom_point(size=3) +
  scale_color_manual(values=c("Optimized Static"="blue", "Optimized JM"="red")) +
  labs(title="Time-Dependent AUC between Joint and Static Models",
       subtitle="Optimized Model Structure For Men",
       x="Time",
       y="AUC",
       color="Models") +
  theme_minimal() + theme(
    text = element_text(size=16),
    legend.title = element_text(size=16, face="bold"),
    legend.position = c(0.80, 0.60),
    legend.text = element_text(size=14),# Top-right positioning
    plot.title = element_text(hjust = 0.5, face="bold", size=18), # Increased size
    plot.subtitle = element_text(hjust = 0.5, size=15), # Increased size
    axis.title.x = element_text(size=16, face="bold"), # Increased size for x-axis label
    axis.title.y = element_text(size=16, face="bold"),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)# Increased size for y-axis label
  )


plot = plot + scale_x_continuous(breaks = c(12, 24, 36, 48, 58), 
                                 labels = c("12 months", "24 months", "36 months", "48 months", "58 months"))
plot
