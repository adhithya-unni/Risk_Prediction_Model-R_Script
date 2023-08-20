library(JMbayes2)
library(glmulti)
library(ggplot2)

long_data_women <- subset(final_data_train, sex == "F")
survival_data_women <- subset(survival_train, sex == "F")
survival_data_women_val = subset(final_data_test, sex == "F")

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



#----------------- LMM for Women ---------------#

# Define custom fitting function for lme
lme_fit <- function(formula, data, random) {
  lme(fixed = formula, data = data, random = random, control = lmeControl(opt="optim"))
}
random_effects <- ~ 1 | DiagnoseID

#-------------- Cholesterol -----------------#

# Finding optimal model based on AIC and BIC

# Define fixed effects and random effects formula
fixed_effects <- chl ~ months + age + as.factor(diabetes) + as.factor(ss) + sbp + dbp + hdl + as.factor(medication)

# Run glmulti function
glmulti_chl_aic <- glmulti(
  y = fixed_effects,
  data = long_data_women,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)


# Run glmulti function
glmulti_chl_bic <- glmulti(
  y = fixed_effects,
  data = long_data_women,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "bic"# Random effects structure
)


opt_chl_women =  lme(chl~1+as.factor(ss)+as.factor(medication)+months+sbp+dbp+hdl, data = long_data_women, random = ~ 1 | DiagnoseID)
AIC(opt_chl_women)
summary(opt_chl_women)

# ------------ HDL -------- #

# Finding optimal model based on AIC and BIC

# Define fixed effects and random effects formula
fixed_effects_hdl <- hdl ~ months + age + as.factor(diabetes) + as.factor(ss) + sbp + dbp + chl + as.factor(medication)

# Run glmulti function
glmulti_hdl_aic <- glmulti(
  y = fixed_effects_hdl,
  data = long_data_women,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)

# Run glmulti function
glmulti_hdl_bic <- glmulti(
  y = fixed_effects_hdl,
  data = long_data_women,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "bic"# Random effects structure
)

opt_hdl_women =  lme(hdl~1+as.factor(medication)+dbp+chl, data = long_data_women, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))
AIC(opt_hdl_women)
summary(opt_hdl_women)

# ---------- SBP --------- #

# Define fixed effects and random effects formula
fixed_effects_sbp <- sbp ~ months + age + as.factor(diabetes) + as.factor(ss) + chl + dbp + hdl + as.factor(medication)

# Run glmulti function
glmulti_treated_bp_aic <- glmulti(
  y = fixed_effects_sbp,
  data = long_data_women,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)

# --------------- DBP --------------- #
opt_sbp =  lme(sbp~1+as.factor(medication)+months+age+chl+dbp, data = long_data_women, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

# Define fixed effects and random effects formula
fixed_effects_dbp <- dbp ~ months + age + as.factor(diabetes) + as.factor(ss) + sbp + chl + hdl + as.factor(medication)

# Run glmulti function
glmulti_treated_bp_aic <- glmulti(
  y = fixed_effects_dbp,
  data = long_data_women,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)

opt_dbp =  lme(dbp~1+as.factor(diabetes)+months+age+sbp+chl, data = long_data_women, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))


# --------------- Untreated SBP --------------- #

# Define fixed effects and random effects formula
fixed_effects_untreated_bp <- untreated_bp ~ months + age + as.factor(diabetes) + as.factor(ss) +  treated_bp + hdl + chl + dbp

# Run glmulti function
glmulti_untreated_bp_aic <- glmulti(
  y = fixed_effects_untreated_bp,
  data = long_data_women,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)

opt_untreated_bp_women =  lme(untreated_bp~1+months+age+treated_bp+chl+dbp, data = long_data_women, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

# --------------- Treated SBP --------------- #

# Define fixed effects and random effects formula
fixed_effects_treated_bp <- treated_bp ~  months + age + as.factor(diabetes) + as.factor(ss) +  untreated_bp + hdl + chl + dbp

# Run glmulti function
glmulti_treated_bp_aic <- glmulti(
  y = fixed_effects_treated_bp,
  data = long_data_women,
  level = 1, # Include pairwise interactions
  method = "h", # Exhaustive search
  fitfunction = lme_fit, # Custom fitting function
  random = random_effects,
  crit = "aic"# Random effects structure
)

opt_treated_bp =  lme(treated_bp~1+months+age+untreated_bp+dbp, data = long_data_women, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))


#------------- Optimals LMM models ---------------#

opt_chl_women =  lme(chl~1+as.factor(ss)+as.factor(medication)+months+sbp+dbp+hdl, 
                     data = long_data_women, random = ~ 1 | DiagnoseID)

plot(fitted(opt_chl_women), resid(opt_chl_women))


opt_hdl_women =  lme(hdl~1+as.factor(medication)+dbp+chl+months, 
                     data = long_data_women, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

plot(fitted(opt_hdl_women), resid(opt_hdl_women))


opt_sbp_women =  lme(sbp~1+as.factor(medication)+months+age+chl+dbp, 
               data = long_data_women, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))


plot(fitted(opt_sbp_women), resid(opt_sbp_women))


opt_dbp_women =  lme(dbp~1+as.factor(diabetes)+months+age+sbp+chl, 
               data = long_data_women, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))


plot(fitted(opt_dbp_women), resid(opt_dbp_women))


opt_untreated_bp_women =  lme(untreated_bp~1+months+age+treated_bp+chl+dbp, 
                              data = long_data_women, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

opt_treated_bp =  lme(treated_bp~1+months+age+untreated_bp+dbp, 
                      data = long_data_women, random = ~ 1 | DiagnoseID, control = lmeControl(opt="optim"))

#--------------- Cox Models ---------------------#

# Cox Proportional Hazards Model cvd or death
Cox_women_cvd <- coxph(Surv(time = followup, 
                            event = status) ~ age + as.factor(diabetes) + as.factor(ss) + as.factor(medication), 
                       data = survival_data_women, 
                       x = TRUE)

Cox_women_cvd_1 <- coxph(Surv(time = followup, 
                            event = status) ~ age + as.factor(medication), 
                       data = survival_data_women, 
                       x = TRUE)
Cox_women_fram <- coxph(Surv(time = followup, 
                             event = status) ~ age + as.factor(diabetes) + as.factor(ss), 
                        data = survival_data_women, 
                        x = TRUE)

#---------------- Joint Model Framingham ------------------#

fForms1<- list("chl" = ~ slope(chl) + value(chl),
               "hdl" = ~ slope(hdl) + value(hdl),
               "treated_bp" = ~ slope(treated_bp) + value(treated_bp),
               "untreated_bp" = ~ slope(untreated_bp) + value(untreated_bp))



JM_opt_women_fram <- jm(Cox_women_fram, 
                     list(opt_untreated_bp_women,opt_chl_women,opt_hdl_women,opt_treated_bp), 
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

summary(JM_opt_women_fram)

compute_aucs(JM_opt_women_fram, survival_data_women_val)


#---------------- Joint Model ------------------#

fForms <- list("chl" = ~ slope(chl) + value(chl),
               "hdl" = ~ slope(hdl) + value(hdl),
               "sbp" = ~ slope(sbp) + value(sbp),
               "dbp" = ~ slope(dbp) + value(dbp))


#---------------- Model 1 ------------------#


JM_opt_women_1 <- jm(Cox_women_cvd, 
                       list(opt_sbp_women,opt_chl_women,opt_hdl_women,opt_dbp_women), 
                       time_var = "months", 
                       n_iter = 5000L,  
                       n_burnin = 200L, 
                       n_thin = 2L,
                       n_chains = 4L,
                       control = list(GK_k = 7,             
                                      cores = 11,           
                                      save_random_effects = FALSE),
                       functional_forms = fForms
)


summary(JM_opt_women_1)

compute_aucs(JM_opt_women_1, survival_data_women_val)

#---------------- Model 2 ------------------#

JM_opt_women_2 <- jm(Cox_women_cvd_1, 
                     list(opt_sbp_women,opt_chl_women,opt_hdl_women,opt_dbp), 
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

summary(JM_opt_women_2)

#---------------- Dynamic Risks Predictions ------------------#
t0 <- 12
ND <- survival_data_women_val
ND <- ND[ND$months <= t0, ]
ND$status <- 0
ND$followup <- t0

t1 <- 36
ND1 <- survival_data_women_val
ND1 <- ND1[ND1$months <= t1, ]
ND1$status <- 0
ND1$followup <- t1

predLong <- predict(JM_opt_women_1, newdata = ND,
                     times = seq(t0, 59, by=2),
                     return_newdata = TRUE, cores=11)

predLong1 <- predict(JM_opt_women_1, newdata = ND1,
                    times = seq(t1, 59, by=2),
                    return_newdata = TRUE, cores=11)

predSurv <- predict(JM_opt_women_1, newdata = ND, process = "event",
                    times = seq(t0, 59, by=2),
                    return_newdata = TRUE, cores=11)

predSurv1 <- predict(JM_opt_women_1, newdata = ND1, process = "event",
                    times = seq(t1, 59, by=2),
                    return_newdata = TRUE, cores=11)
par(font.axis=1)

plot(predLong, predSurv,outcomes = 1:3,
     pos_ylab_long = c(120, 220, 80), cex_ylab_long = 1.7,  
     cex_main = 1.2, cex_xlab = 1.1, cex_axis=1.3, 
     cex_ylab_event=1.2,font.axis=1, lwd_long = 3.5, lwd_event = 3.5,main="Cumulative Risks using information from the first 12 Months")

plot(predLong1, predSurv1,outcomes = 1:3, pos_ylab_long = c(120, 220, 80),
     cex_ylab_long = 1.8,  cex_main = 1.2, cex_xlab = 1.1, cex_axis=1.3,
     cex_ylab_event=1.2,font.axis=1, lwd_long = 3.5, lwd_event = 3.5,main="Cumulative Risks using information from the first 36 Months")

#---------- Model Comparison -------------#
# Defining the Time intervals
time_intervals <- c(0, 12, 24, 36, 48, 58)

# AUC for Framingham joint model for men
framingham_auc_women <- c(0.4229344, 0.4275736, 0.5185163, 0.5219828, 0.596513)

# AUC for the optimal model for women
optimal_auc_women <- c(0.4515622, 0.462685, 0.5198633, 0.5375216, 0.6172145)

# Creating the data frame
auc_JM_women <- data.frame(
  Time = rep(time_intervals[-1], 2), 
  AUC = c(framingham_auc_women, optimal_auc_women),
  Model = c(rep("Framingham JM", length(framingham_auc_women)), rep("Optimal JM", length(optimal_auc_women)))
)

# View the data frame
print(auc_JM_women)

plot <- ggplot(auc_JM_women, aes(x=Time, y=AUC, color=Model)) +
  geom_line(size=1.2) +
  geom_point(size=3) +
  scale_color_manual(values=c("Framingham JM"="blue", "Optimal JM"="red")) +
  labs(title="Time-Dependent AUC between Joint Models",
       subtitle="AUC values for Framingham Model and Optimal Model for Women",
       x="Time",
       y="AUC",
       color="Models") +
  theme_minimal() +
  theme(
    text = element_text(size=16),
    legend.title = element_text(size=16, face="bold"),
    legend.position = c(0.80, 0.30),
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

#-------------- Model Comparison Static vs JM ------------------------#
auc_women_opt = data.frame(
  Time = rep(c(12, 24, 36, 48, 58), 2),
  AUC = c(0.7541573, 0.6701212, 0.6426111, 0.6163735, 0.6255955, 
          0.4515622, 0.462685, 0.5198633, 0.5375216, 0.6172145),
  Model = rep(c("Optimized Static", "Optimized JM"), each = 5)
)

plot <- ggplot(auc_women_opt, aes(x=Time, y=AUC, color=Model)) +
  geom_line(size=1.2) +
  geom_point(size=3) +
  scale_color_manual(values=c("Optimized Static"="blue", "Optimized JM"="red")) +
  labs(title="Time-Dependent AUC between Joint and Static Models",
       subtitle="Optimized Model Structure For Women",
       x="Time",
       y="AUC",
       color="Models") +
  theme_minimal() +
  theme(
    text = element_text(size=16),
    legend.title = element_text(size=16, face="bold"),
    legend.position = c(0.80, 0.80),
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
