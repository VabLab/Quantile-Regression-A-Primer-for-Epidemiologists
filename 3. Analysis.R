#------------------------------------------------------------------------------
# Coders: Jilly Hebert and Aayush Khadka
# Date: 12/1/2023

# R code to fit ordinary least squares (OLS), conditional quantile regression
# (CQR), and unconditional quantile regression (UQR) models. All models 
# include bootstrapped confidence intervals (500 resamples). 

# Lines 17-22: Load libraries
# Lines 23-37: Load and format data
# Lines 38-82: Fit OLS models
# Lines 83-136: Fit CQR models (1st-99th quantiles)
# Lines 137-204: Fit OLS models (1st-99th quantiles)
# Lines 205-213: Combine all model results into one dataset

#------------------------------------------------------------------------------
library(tidyverse)
library(boot)
library(quantreg)
library(dineq)
library(haven)

#------------------------------------------------------------------------------
# Load and format data
#------------------------------------------------------------------------------
data <- read_rds("Data/SBPCCData.rds")

data <- data %>%
  rename("mom_ed" = "rameduc",
         "dad_ed" = "rafeduc",
         "y08" = "wave9",
         "y10" = "wave10",
         "y12" = "wave11",
         "y14" = "wave12",
         "y16" = "wave13",
         "y18" = "wave14")

#------------------------------------------------------------------------------
# OLS
#------------------------------------------------------------------------------
ols <- lm(sbp ~ schlyrs + age + age2 + female + black + latinx + southern + 
            mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + y18, data = data)
summary(ols)


#Bootstrap ci
boot_lm <- function(data, id){
  fit <- lm(sbp ~ schlyrs + age + age2 + female + black + latinx + southern + 
              mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + y18,
            data = data[id, ])
  coef(fit)
  
}

b <- boot(data, boot_lm, 500)
ols_ci <- data.frame("index" = NA,
                     "lower" = NA,
                     "upper" = NA)

for(i in 1:nrow(summary(ols)$coefficient)){
  
  boot <- boot.ci(b, index = i, type = "perc")
  ols_ci[i, "index"] = i
  ols_ci[i, "lower"] = boot$percent[, 4]
  ols_ci[i, "upper"] = boot$percent[, 5]
  
}

ols_est <- cbind(ols_ci[, -1], summary(ols)$coefficient)
rownames(ols_est) <- rownames(summary(ols)$coefficient)
ols_est

ols_save <- ols_est[2, 1:3]
rownames(ols_save) <- NULL
ols_save$quantile <- -6

ols_save <- ols_save %>%
  dplyr::select(quantile, Estimate, lower, upper) %>%
  rename("est" = "Estimate",
         "lci" = "lower",
         "uci" = "upper")

#------------------------------------------------------------------------------
# Conditional
#------------------------------------------------------------------------------
#Function
cqr_func <- function(data){
  
  conditional_results <- data.frame()
  
  for(i in seq(1, 99, by = 1)){
    
    i <- round(i, 0)
    t <- i / 100
    
    con <- rq(sbp ~ schlyrs + age + age2 + female + black + latinx + southern + 
                mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + y18,
              data = data, tau = t) 
    coef <- summary(con, se = "boot", bsmethod = "xy", R = 500) #Bootstrapped SE
    
    #Bootstrap ci
    boot <- boot.rq(cbind(1, data$schlyrs, data$age, data$age2, data$female,
                          data$black, data$latinx, data$southern, data$mom_ed,
                          data$dad_ed, data$y08, data$y10, data$y12, data$y14,
                          data$y16, data$y18),
                    data$sbp, tau = t, R = 500) #Takes a little while to run
    ci <- t(apply(boot$B, 2, quantile, c(0.025, 0.975)))
    
    cqr_est <- cbind(i, ci, coef$coefficient)
    rownames(cqr_est) <- rownames(coef$coefficient)
    
    schlyr_est <- data.frame(t(cqr_est["schlyrs", 1:4]))
    
    conditional_results <- rbind(conditional_results, schlyr_est)
    
  }
  
  names(conditional_results) <- c("quantile", "lower_ci", "upper_ci",
                                  "estimate")
  conditional_results <- conditional_results %>%
    dplyr::select(quantile, lower_ci, estimate, upper_ci)
  return(conditional_results)
  
}

cqr_results <- cqr_func(data)
#Nonunique solutions warning prompted by categorical variables in model

cqr_save <- cqr_results %>%
  dplyr::select(quantile, estimate, lower_ci, upper_ci) %>%
  rename("est" = "estimate",
         "lci" = "lower_ci",
         "uci" = "upper_ci")

#write_rds(cqr_save, "Results/cqr_results.rds")

#------------------------------------------------------------------------------
# UQR - RIF
#------------------------------------------------------------------------------
#Function
uqr_func <- function(data){
  
  unconditional_results <- data.frame()
  
  for(i in seq(1, 99, by = 1)){
    
    i <- round(i, 0)
    t <- i / 100
    
    data$rif_sbp <- rif(data$sbp, weights = NULL, method = "quantile",
                        quantile = t)
    uqr <- lm(rif_sbp ~ schlyrs + age + age2 + female + black + latinx +
                southern + mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + y18, 
              data = data)
    
    #Bootstrap ci
    boot_uqr <- function(data, id){
      
      fit <- lm(rif_sbp ~ schlyrs + age + age2 + female + black + latinx +
                  southern + mom_ed + dad_ed + y08 + y10 + y12 + y14 + y16 + 
                  y18, data = data[id, ])
      coef(fit)
    }
    
    b <- boot(data, boot_uqr, 500)
    uqr_ci <- data.frame("quantile" = NA,
                         "lower_ci" = NA,
                         "upper_ci" = NA)
    
    
    for(j in 1:nrow(summary(uqr)$coefficient)){
      
      boot <- boot.ci(b, index = j, type = "perc")
      uqr_ci[j, "quantile"] = i
      uqr_ci[j, "lower_ci"] = boot$percent[, 4]
      uqr_ci[j, "upper_ci"] = boot$percent[, 5]
    }
    
    uqr_est <- cbind(uqr_ci, summary(uqr)$coefficient)
    
    uqr_schlyr_est <- data.frame(uqr_est[2, 1:4])
    
    unconditional_results <- rbind(unconditional_results, uqr_schlyr_est)
    
  }
  
  names(unconditional_results) <- c("quantile", "lower_ci", "upper_ci",
                                    "estimate")
  unconditional_results <- unconditional_results %>%
    dplyr::select(quantile, lower_ci, estimate, upper_ci)
  return(unconditional_results)
  
}

uqr_results <- uqr_func(data)

uqr_save <- uqr_results %>%
  dplyr::select(quantile, estimate, lower_ci, upper_ci) %>%
  rename("est" = "estimate",
         "lci" = "lower_ci",
         "uci" = "upper_ci")

#write_rds(uqr_save, "Results/uqr_results.rds")

#------------------------------------------------------------------------------
# Combine
#------------------------------------------------------------------------------
ols_save$regtype <- "OLS"
cqr_save$regtype <- "CQR"
uqr_save$regtype <- "UQR"

results <- rbind(ols_save, cqr_save, uqr_save)
#write_rds(results, "Results/AllResults.rds")

