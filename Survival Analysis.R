### Competing Risks Analysis ### 
library(survival)
library(survminer)
library(Hmisc)
library(glmulti)
library(timeROC)


#---------- Time ROC Function ---------#
compute_timeROC <- function(model, validation_data, times) {
  # Extract follow-up time and status
  followup <- validation_data$followup
  status <- validation_data$status
  
  # Compute the predicted risk scores
  predicted_risk <- predict(model, newdata = validation_data, type = "risk")
  
  # Create survival object
  survival_obj_validation <- Surv(time = followup, event = status)
  
  # Calculate time-dependent ROC
  ROC_obj_validation <- timeROC(T = followup,
                                delta = status,
                                marker = predicted_risk,
                                cause = 1,
                                weighting = "cox",
                                times = times,
                                iid = FALSE)
  
  # Return ROC object
  return(ROC_obj_validation)
}

# For women
survival_data_women <- subset(survival_train, sex == "F")
validation_data_women <- subset(survival_test, sex == "F")

# Cox Proportional Hazards Model cvd
CoxFit_women_cvd <- coxph(Surv(time = followup, 
                               event = status) ~ age + as.factor(diabetes) + as.factor(ss) + treated_bp + untreated_bp + chl + hdl, 
                          data = survival_data_women, 
                          x = TRUE)


summary(CoxFit_women_cvd)

BIC(CoxFit_women_cvd)
AIC(CoxFit_women_cvd)


#----- ROC FOR FRAM WOMEN ------#
times <- c(12, 24, 36, 48, 58)
ROC_result <- compute_timeROC(model = CoxFit_women_cvd, validation_data = survival_data_women_validation, times = times)
print(ROC_result)

###--------------- Assessing the proportional hazards assumption using Schoenfeld residuals --------------###
cox.zph_result <- cox.zph(CoxFit_women_cvd)

###-------------------------- Print the result ---------------------###
print(cox.zph_result)

###-------------------------- Plot the result -----------------------###
ggcoxzph(cox.zph_result) 

#----------------- Survival curves ------------------------------#
surv_curve <- survfit(CoxFit_women_cvd)
ggsurvplot(surv_curve, data=survival_data_women)

#External Validation
predictions <- predict(CoxFit_women_cvd, newdata=survival_data_women_validation, type="lp")
c_index <- rcorr.cens(x=predictions, S=Surv(survival_data_women_validation$followup, survival_data_women_validation$status_cvd))
print(c_index)

#-------------- Model Search For Women --------------------#

formula =  as.formula(Surv(time = followup, 
                           event = status) ~ age + as.factor(diabetes) + as.factor(ss) +  hdl + chl + sbp + dbp + as.factor(medication)) 

glmulti_result <- glmulti(
  y = formula, # or use the Surv object if using the 'xr' argument # omit if using the full formula
  data = survival_data_women,
  fitfunction = "coxph", # fitting function for Cox model
  level = 1, # if you want to include pairwise interactions
  method = "h", # using genetic algorithm; use "h" for exhaustive
  crit = "aic",# criterion for model selection
  confsetsize = 10,
  # Other optional arguments as needed
)

head(weightable(glmulti_result))

opt_model_women = coxph(Surv(time = followup, event = status_cvd) ~ 1 + age + sbp, data = survival_data_women)

summary(opt_model_women)

#--------------- ROC FOR OPTIMAL MODEL FOR WOMEN ---------------------#
times <- seq(1, 59, by=1)
ROC_result <- compute_timeROC(model = opt_model_women, validation_data = survival_data_women_validation, times = times)
print(ROC_result)

# Assessing the proportional hazards assumption using Schoenfeld residuals
cox.zph_result <- cox.zph(opt_model_women)

# Print the result
print(cox.zph_result)

# Plot the result
plot(cox.zph_result)

# Survival curves
surv_curve <- survfit(opt_model_women)
ggsurvplot(surv_curve, data=survival_data_women)

#External Validation
predictions <- predict(opt_model_women, newdata=survival_data_women_validation, type="lp")
c_index <- rcorr.cens(x=predictions, S=Surv(survival_data_women_validation$followup, survival_data_women_validation$status_cvd))
print(c_index)

#---------- Model Comparison for Women --------------#
# Load ggplot2 for enhanced plotting
library(ggplot2)

# Time points

times <- c(12, 24, 36, 48, 58)

# AUC values for Model 1
auc_model_1 <- c(0.7153326, 0.6644709, 0.6446566, 0.6204124, 0.6301216)

# AUC values for Model 2
auc_model_2 <- c(0.7541573, 0.6701212, 0.6426111, 0.6163735, 0.6255955)

data$Model <- factor(data$Model, levels=c("Model 1", "Model 2"), labels=c("Framingham", "Optimal"))


# Combine AUC values for Model 1 and Model 2 into a single data frame
plot <- ggplot(data, aes(x=Time, y=AUC, color=Model)) +
  geom_line(size=1.2) +
  geom_point(size=3) +
  scale_color_manual(values=c("Framingham"="blue", "Optimal"="red"), labels=c("Framingham Static", "Optimized Static")) +
  labs(title="Time-Dependent AUC between Static Models",
       subtitle="AUC values for Framingham Model and Optimal Model For Women",
       x="Time",
       y="AUC",
       color="Models") +
  theme_minimal() +
  theme(
    text = element_text(size=16),
    legend.title = element_text(size=16, face="bold"),
    legend.position = c(0.80, 0.40),
    legend.text = element_text(size=14),# Top-right positioning
    plot.title = element_text(hjust = 0.5, face="bold", size=18), # Increased size
    plot.subtitle = element_text(hjust = 0.5, size=15), # Increased size
    axis.title.x = element_text(size=16, face="bold"), # Increased size for x-axis label
    axis.title.y = element_text(size=16, face="bold"),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)# Increased size for y-axis label
  )


# Print the plot
plot = plot + scale_x_continuous(breaks = c(12, 24, 36, 48, 58), 
                                 labels = c("12 months", "24 months", "36 months", "48 months", "58 months"))
plot


#---------------- For men ------------------#

survival_data_men <- subset(survival_train, sex == "M")
survival_data_men_validation <- subset(survival_test, sex == "M")

# Cox Proportional Hazards Model cvd
CoxFit_men_cvd <- coxph(Surv(time = followup, 
                               event = status) ~ age + as.factor(diabetes) + as.factor(ss) + treated_bp + untreated_bp + chl + hdl, 
                          data = survival_data_men, 
                          x = TRUE)

summary(CoxFit_men_cvd)

AIC(CoxFit_men_cvd)
BIC(CoxFit_men_cvd)

#------------ ROC FRAM MAN -----------#
times <- c(12, 24, 36, 48, 58)
ROC_result <- compute_timeROC(model = CoxFit_men_cvd, validation_data = survival_data_men_validation, times = times)
print(ROC_result)



# Assessing the proportional hazards assumption using Schoenfeld residuals
cox.zph_result <- cox.zph(CoxFit_men_cvd)

# Print the result
print(cox.zph_result)

# Plot the result
plot(cox.zph_result)

# Survival curves
surv_curve <- survfit(CoxFit_men_cvd)
ggsurvplot(surv_curve, data=survival_data_men)

#External Validation
predictions <- predict(CoxFit_men_cvd, newdata=survival_data_men_validation, type="lp")
c_index <- rcorr.cens(x=predictions, S=Surv(survival_data_men_validation$followup, survival_data_men_validation$status_cvd))
print(c_index)

# Model Search

formula =  as.formula(Surv(time = followup, 
                        event = status_cvd) ~ age + as.factor(diabetes) + as.factor(ss) +  hdl + chl + sbp + dbp + as.factor(medication)) 

glmulti_result <- glmulti(
  y = formula, # or use the Surv object if using the 'xr' argument # omit if using the full formula
  data = survival_data_men,
  fitfunction = "coxph", # fitting function for Cox model
  level = 1, # if you want to include pairwise interactions
  method = "h", # using genetic algorithm; use "h" for exhaustive
  crit = "aic",# criterion for model selection
  confsetsize = 10,
)

head(weightable(glmulti_result))

opt_model_men = coxph(Surv(time = followup, event = status) ~ 1 + age + dbp, data = survival_data_men)
summary(opt_model_men)

AIC(opt_model_men)
BIC(opt_model_men)

#-------------- ROC OPTIMAL MEN --------------#
times <- c(12, 24, 36, 48, 58)
ROC_result <- compute_timeROC(model = opt_model_men, validation_data = survival_data_men_validation, times = times)
print(ROC_result)

# Assessing the proportional hazards assumption using Schoenfeld residuals
cox.zph_result <- cox.zph(opt_model_men)

# Print the result
print(cox.zph_result)

# Plot the result
plot(cox.zph_result)

# Survival curves
surv_curve <- survfit(opt_model_men)
ggsurvplot(surv_curve, data=survival_data_men)

#External Validation
predictions <- predict(opt_model_men, newdata=survival_data_men_validation, type="lp")
c_index <- rcorr.cens(x=predictions, S=Surv(survival_data_men_validation$followup, survival_data_men_validation$status_cvd))
print(c_index)

#---------- Model Comparison Men ------------------#

# Time-dependent AUC for the Framingham model
framingham_auc <- c(0.5375967, 0.5660002, 0.5716303, 0.5923146, 0.5713357)

# Time-dependent AUC for the optimal model
optimal_auc <- c(0.5479267, 0.5681126, 0.5824520, 0.6036994, 0.5744366)

# Time points
times <- c(12, 24, 36, 48, 58)

# Combine the results into a data frame
auc_df <- data.frame(
  Time = rep(times, 2),
  AUC = c(framingham_auc, optimal_auc),
  Model = factor(rep(c("Framingham", "Optimal"), each = length(times)))
)

print(auc_df)
# Combine AUC values for Model 1 and Model 2 into a single data frame
plot <- ggplot(auc_df, aes(x=Time, y=AUC, color=Model)) +
  geom_line(size=1.2) +
  geom_point(size=3) +
  scale_color_manual(values=c("Framingham"="blue", "Optimal"="red"), labels=c("Framingham Static", "Optimized Static")) + 
  labs(title="Time-Dependent AUC between Static Models",
       subtitle="AUC values for Framingham Model and Optimal Model For Men",
       x="Time",
       y="AUC",
       color="Models") +
  theme_minimal() +
  theme(
    text = element_text(size=16),
    legend.title = element_text(size=16, face="bold"),
    legend.position = c(0.80, 0.40),
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
