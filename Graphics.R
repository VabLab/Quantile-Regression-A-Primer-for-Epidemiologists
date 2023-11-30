#------------------------------------------------------------------------------
# Coders: Jilly Hebert and Aayush Khadka
# Date: 12/1/2023

# R code to fit ordinary least squares (OLS), conditional quantile regression
# (CQR), and unconditional quantile regression (UQR) models. All models 
# include bootstrapped confidence intervals (500 resamples). 

# Lines 23-35: Load libraries
# Lines 36-72: Read in sample dataset and create table 1
# Lines 73-148: Create location and scale shift graphic
# Lines 149-194: Create density and CDF plots
# Lines 195-361: Create recentered influence plots (RIF)
# Lines 362-378: Read in results datasets
# Lines 379-492: Create plots comparing model estimations
# Lines 493-593: Create counterfactual distributions based on model estimations  
# and create density plots of empirical and counterfactual distributions
# Lines 594-749: Create density plots by age group
# Lines 750-871: Create graphics comparing model estimations with restricted 
# quantiles (CQR and UQR 10-90th quantiles as opposed to 1st-99th quantiles)

#------------------------------------------------------------------------------
library(tidyverse)
library(haven)
library(gt)
library(gtsummary)
library(ggthemes)
library(ggpattern)
library(ggpubr)
library(berryFunctions)
library(patchwork)
library(dineq)
library(radiant.data)
library(grid)

#------------------------------------------------------------------------------
# Load and format data for table 1
#------------------------------------------------------------------------------
data <- read_rds("Data/SBPCCData.rds")


#Table1
table1 <- data %>%
  dplyr::select(gender, race, birthyr, birthplc, schlyrs, rameduc, rafeduc, 
                year, sbp, age, age2) %>%
  dplyr::mutate(group = case_when(schlyrs < 12 ~ "<12",
                                  schlyrs == 12 ~ "12",
                                  schlyrs > 12 ~ ">12"))
table(table1$group, useNA = "always")
table1$group <- factor(table1$group, levels = c("<12", "12", ">12"))

table1$gender <- labelled::to_factor(table1$gender)
table1$race <- labelled::to_factor(table1$race)
table1$birthplc <- labelled::to_factor(table1$birthplc)

table1$rameduc <- as.numeric(table1$rameduc)
table1$rafeduc <- as.numeric(table1$rafeduc)

#By education level
table1_out <- table1 %>%
  tbl_summary(by = group,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) %>%
  bold_labels()
table1_out

#Overall
table1 %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) %>%
  bold_labels()

#------------------------------------------------------------------------------
# Manuscript figure: Location and scale shifts
#------------------------------------------------------------------------------
#Location shift
set.seed(543)
cont <- rnorm(100, mean = 0, sd = 1)
loc_t <- cont + 3

ls <- data.frame("Outcome" = c(cont, loc_t),
                 "Group" = factor(c(rep("Control", length(cont)),
                                    rep("Treatment", length(loc_t)))))

location_shift <- ggplot(aes(Outcome, color = Group, linetype = Group), 
                         data = ls) +
  geom_density(size = 1.5) +
  xlim(-8, 10) +
  ylab("Density") +
  labs(title = "A.",
       x = "") +
  scale_color_manual(values = c("#D3D3D3", "#808080")) + #values = c("black", "#FF9300")) + 
  theme_classic() +
  theme(strip.background = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom")


#Scale shift
set.seed(543)
scl_t <- rnorm(100, mean = 0, sd = 3)

ss <- data.frame("Outcome" = c(cont, scl_t),
                 "Group" = factor(c(rep("Control", length(cont)),
                                    rep("Treatment", length(scl_t)))))

scale_shift <- ggplot(aes(Outcome, color = Group, linetype = Group),
                      data = ss) +
  geom_density(size = 1.5) +
  xlim(-8, 10) +
  ylab("Density") +
  labs(title = "B.",
       x = "") +
  scale_color_manual(values = c("#D3D3D3", "#808080")) + #values = c("black", "#FF9300")) + 
  theme_classic() +
  theme(strip.background = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom")


#Location and Scale shift
ls_t <- scl_t + 3

lss <- data.frame("Outcome" = c(cont, ls_t),
                  "Group" = factor(c(rep("Control", length(cont)),
                                     rep("Treatment", length(ls_t)))))

loc_scale_shift <- ggplot(aes(Outcome, color = Group, linetype = Group), 
                          data = lss) +
  geom_density(size = 1.5) +
  xlim(-8, 10) +
  ylab("Density") +
  labs(title = "C.",
       x = "") + 
  scale_color_manual(values = c("#D3D3D3", "#808080")) + #values = c("black", "#FF9300")) + 
  theme_classic() +
  theme(strip.background = element_blank(),
        text = element_text(size = 15),
        legend.position = "bottom")

#Final plot
(location_shift | scale_shift) / 
(loc_scale_shift + plot_spacer())

ggarrange(location_shift, scale_shift, loc_scale_shift,
          nrow = 2, ncol = 2, align = "hv", common.legend = T,
          legend = "bottom")

#------------------------------------------------------------------------------
# Density and CDF plots
#------------------------------------------------------------------------------
#Find cutoffs
q10 <- as.numeric(quantile(data$sbp, probs = 0.10)) 
q25 <- as.numeric(quantile(data$sbp, probs = 0.25)) #Quantile number (checks out with summary)
q50 <- as.numeric(quantile(data$sbp, probs = 0.50)) #Quantile number (checks out with summary)
q75 <- as.numeric(quantile(data$sbp, probs = 0.75)) #Quantile number (checks out with summary)
q90 <- as.numeric(quantile(data$sbp, probs = 0.90)) 

overall_density <- density(data$sbp)
overall_density <- as.data.frame(overall_density[1:2])


#Histogram/density plot
ggplot(data, aes(x = sbp)) + 
  geom_density(aes(y = after_stat(density))) +
  xlab("Systolic Blood Pressure (mmHg)") +
  ylab("Density")  +
  labs(title = "a) Marginal density of systolic blood pressure") + 
  theme_classic() +
  theme(strip.background = element_blank(),
        text = element_text(size = 20)) + 
  geom_vline(aes(xintercept = q10), color = "red", size = 1.5) +
  geom_vline(aes(xintercept = q25), color = "dark orange", size = 1.5) +
  geom_vline(aes(xintercept = q50), color = "dark green", size = 1.5) +
  geom_vline(aes(xintercept = q75), color = "blue", size = 1.5) +
  geom_vline(aes(xintercept = q90), color = "purple", size = 1.5)


#CDF plot
ggplot(data, aes(x = sbp)) +
  stat_ecdf(geom = "line") +
  xlim(75, 230) +
  xlab("Systolic Blood Pressure (mmHg)") +
  ylab("F(Y)") +
  labs(title = "b) Cumulative distribution of systolic blood pressure") +
  theme_classic() +
  theme(strip.background = element_blank(),
        text = element_text(size = 20)) +
  geom_vline(aes(xintercept = q10), color = "red", size = 1.5) +
  geom_vline(aes(xintercept = q25), color = "dark orange", size = 1.5) +
  geom_vline(aes(xintercept = q50), color = "dark green", size = 1.5) +
  geom_vline(aes(xintercept = q75), color = "blue", size = 1.5) +
  geom_vline(aes(xintercept = q90), color = "purple", size = 1.5)

#------------------------------------------------------------------------------
# RIF Graphics
#------------------------------------------------------------------------------
data$rif_10 <- rif(data$sbp, weights = NULL, method = "quantile",
                   quantile = 0.1)
data$rif_25 <- rif(data$sbp, weights = NULL, method = "quantile",
                   quantile = 0.25)
data$rif_50 <- rif(data$sbp, weights = NULL, method = "quantile",
                   quantile = 0.5)
data$rif_75 <- rif(data$sbp, weights = NULL, method = "quantile",
                   quantile = 0.75)
data$rif_90 <- rif(data$sbp, weights = NULL, method = "quantile",
                   quantile = 0.9)

#Establish high and low values (HARD CODED)
rif_data <- data %>%
  dplyr::select(hhidpn, starts_with("rif")) %>%
  pivot_longer(!hhidpn, names_to = "quantile", values_to = "rif")
rif_data$quantile <- gsub("rif_", "", rif_data$quantile)
rif_data$group <- ifelse(almost.equal(rif_data$rif, 28.8186804467409)| 
                           almost.equal(rif_data$rif, 75.9435588661003) |
                           almost.equal(rif_data$rif, 102.998545028217) |
                           almost.equal(rif_data$rif, 121.650954819836) | 
                           almost.equal(rif_data$rif, 138.851719649121),
                         "Low", "High")

#Summary RIF values
ggplot(aes(rif, fill = quantile, pattern = factor(group)), data = rif_data) + 
  geom_bar() + 
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  scale_pattern_manual(values = c(Low = "stripe", High = "none"),
                       name = "RIF Value") +
  scale_fill_manual(values = c("red", "dark orange", "dark green",
                                    "blue", "purple"),
                                    labels = c("10th", "25th", "50th",
                                               "75th", "90th"),
                    name = "Quantile") +
  xlab("RIF Value") +
  ylab("Count") +
  theme_classic() +
  theme(strip.background = element_blank(),
        text = element_text(size = 20)) + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))



#Reduced RIF plot
data <- data %>%
  dplyr::select(-c(rif_10, rif_90))

rif_data <- rif_data %>%
  dplyr::filter(quantile != 10 & quantile != 90)

ggplot(aes(rif, fill = quantile, pattern = factor(group)), data = rif_data) + 
  geom_bar() + 
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  scale_pattern_manual(values = c(Low = "stripe", High = "none"),
                       name = "RIF Value") +
  scale_fill_manual(values = c("dark orange", "dark green", "blue"),
                                    labels = c("25th", "50th", "75th"),
                    name = "Quantile") +
  xlab("RIF Value") +
  ylab("Count") +
  theme_classic() +
  theme(strip.background = element_blank(),
        text = element_text(size = 20)) + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
  

#Panel version (Manuscript figure)
rif25 <- rif_data %>%
  dplyr::filter(quantile == 25)

ggplot(aes(rif, pattern = factor(group)), data = rif25) + 
  geom_bar(width = 0.2) + 
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   fill = "#D3D3D3",
                   width = 8,
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  scale_pattern_manual(values = c(Low = "stripe", High = "none"),
                       name = "RIF Value") +
  xlab("RIF Value") +
  ylab("Count") +
  labs(title = "A.") +
  theme_classic() +
  theme(strip.background = element_blank(),
        text = element_text(size = 30)) + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  scale_x_continuous(breaks = seq(50, 200, by = 25),
                     limits = c(50, 200))


rif50 <- rif_data %>%
  dplyr::filter(quantile == 50)

ggplot(aes(rif, pattern = factor(group)), data = rif50) + 
  geom_bar(width = 0.2) + 
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   fill = "#D3D3D3",
                   width = 8,
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  scale_pattern_manual(values = c(Low = "stripe", High = "none"),
                       name = "RIF Value") +
  xlab("RIF Value") +
  ylab("Count") +
  labs(title = "B.") + 
  theme_classic() +
  theme(strip.background = element_blank(),
        text = element_text(size = 30)) + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  scale_x_continuous(breaks = seq(50, 200, by = 25),
                     limits = c(50, 200))


rif75 <- rif_data %>%
  dplyr::filter(quantile == 75)

ggplot(aes(rif, pattern = factor(group)), data = rif75) + 
  geom_bar(width = 0.1) + 
  geom_bar_pattern(position = "identity",
                   fill = "#D3D3D3",
                   width = 8,
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) + 
  scale_pattern_manual(values = c(Low = "stripe", High = "none"),
                       name = "RIF Value") +
  xlab("RIF Value") +
  ylab("Count") +
  labs(title = "C.") +
  theme_classic() +
  theme(strip.background = element_blank(),
        text = element_text(size = 30)) + 
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  scale_x_continuous(breaks = seq(50, 200, by = 25),
                     limits = c(50, 200))

#-------------------------------------------------------------------------------
# Read in results data for graphics (From Analysis.R)
#-------------------------------------------------------------------------------
results <- read_rds("Results/AllResults.rds")
results$regtype <- factor(results$regtype, levels = c("OLS", "CQR", "UQR"))
names(results) <- c("Quantile", "Estimate", "Lower", "Upper", "regtype")

#Break results into each model
ols <- results %>%
  dplyr::filter(regtype == "OLS")

cqr <- results %>%
  dplyr::filter(regtype == "CQR")

uqr <- results %>%
  dplyr::filter(regtype == "UQR")

#-------------------------------------------------------------------------------
# Estimation plots
#-------------------------------------------------------------------------------
#All results
ggplot(data = results %>% filter(regtype != "OLS"),
       aes(x = Quantile, y = Estimate, group = regtype,
           color = regtype, fill = regtype)) +
  geom_line(alpha = 50, linewidth = 0.75) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.27, color = NA) +
  geom_point(data = results %>% filter(regtype == "OLS")) +
  geom_errorbar(data = results %>% filter(regtype == "OLS"),
                aes(ymin = Lower, ymax = Upper), width = 0.01) +
  geom_hline(yintercept = 0, alpha = 0.5) + 
  geom_vline(xintercept = -2.5, color = "gray", linetype = "dashed") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")  + 
  scale_color_manual(breaks = c("OLS", "CQR", "UQR"),
                     labels = c("OLS", "CQR", "UQR"),
                     values = c("#F2494C", "#468C8A", "#FF9300")) +
  scale_fill_manual(breaks = c("OLS", "CQR", "UQR"),
                    labels = c("OLS", "CQR", "UQR"),
                    values = c("#F2494C", "#468C8A", "#FF9300"))  +
  scale_x_continuous(limits = c(-10, 100),
                     breaks = c(-6, 1, 10, 20, 30, 40, 50,
                                60, 70, 80, 90, 99), 
                     labels = c("OLS", "q1", "q10", "q20", "q30", "q40",
                                "q50", "q60", "q70", "q80", "q90", "q99")) + 
  labs(x = "",
       y = expression("Change in SBP (mmHg)"),
       color = "Model", group = "Model", fill = "Model") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(size = 20))



#OLS and CQR
ols_cqr <- results %>%
  dplyr::filter(regtype != "UQR")

ols_cqr_p <- ggplot(data = ols_cqr %>% filter(regtype != "OLS"),
       aes(x = Quantile, y = Estimate, group = regtype,
           color = regtype, fill = regtype)) +
  geom_line(alpha = 50, size = 0.75) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.27, color = NA) +
  geom_point(data = ols_cqr %>% filter(regtype == "OLS")) +
  geom_errorbar(data = ols_cqr %>% filter(regtype == "OLS"),
                aes(ymin = Lower, ymax = Upper), width = 0.01) +
  geom_hline(yintercept = 0, alpha = 0.5, color = "#000000") + 
  geom_vline(xintercept = -2.5, color = "red", linetype = "dashed") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 14),) + 
  scale_color_manual(values = c("#808080", "#808080")) +
  scale_fill_manual(values = c("#808080", "#808080"))  + 
  #scale_color_manual(values = c("#468C8A", "#F2494C")) + #For colored plot
  #scale_fill_manual(values = c("#468C8A", "#F2494C"))  + 
  scale_x_continuous(limits = c(-10, 100),
                     breaks = c(-6, 1, 10, 20, 30, 40, 50,
                                60, 70, 80, 90, 99), 
                     labels = c("OLS", "Q1", "Q10", "Q20", "Q30", "Q40",
                                "Q50", "Q60", "Q70", "Q80", "Q90", "Q99")) +
  labs(title = "A.",
       x = "",
       y = "Change in Systolic Blood Pressure (mmHg)")



#OLS and UQR
ols_uqr <- results %>%
  dplyr::filter(regtype != "CQR")

ols_uqr_p <- ggplot(data = ols_uqr %>% filter(regtype != "OLS"),
       aes(x = Quantile, y = Estimate, group = regtype,
           color = regtype, fill = regtype)) +
  geom_line(alpha = 50, size = 0.75) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.27, color = NA) +
  geom_point(data = ols_uqr %>% filter(regtype == "OLS")) +
  geom_errorbar(data = ols_uqr %>% filter(regtype == "OLS"),
                aes(ymin = Lower, ymax = Upper), width = 0.01) +
  geom_hline(yintercept = 0, alpha = 0.5, color = "#000000") + 
  geom_vline(xintercept = -2.5, color = "red", linetype = "dashed") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 14)) + 
  scale_color_manual(values = c("#808080", "#808080")) +
  scale_fill_manual(values = c("#808080", "#808080"))  + 
  #scale_color_manual(values = c("#F2494C", "#FF9300")) + #For colored plot
  #scale_fill_manual(values = c("#F2494C", "#FF9300"))  + 
  scale_x_continuous(limits = c(-10, 100),
                     breaks = c(-6, 1, 10, 20, 30, 40, 50,
                                60, 70, 80, 90, 99), 
                     labels = c("OLS", "Q1", "Q10", "Q20", "Q30", "Q40",
                                "Q50", "Q60", "Q70", "Q80", "Q90", "Q99")) + 
  labs(title = "C.",
       x = "",
       y = "Change in Systolic Blood Pressure (mmHg)")


#CQR and UQR plots together
ols_cqr_p / ols_uqr_p

#-------------------------------------------------------------------------------
# Counterfactual plots
#-------------------------------------------------------------------------------
#CQR
cqr_coef <- cqr %>%
  dplyr::select(Quantile, Estimate) %>%
  rename("quant" = "Quantile",
         "coef" = "Estimate")

cqr_red <- data %>%
  dplyr::select(sbp) %>%
  mutate(quant = xtile(sbp, n = 99))

cqr_merg <- right_join(cqr_coef, cqr_red, by = "quant")

#Create counterfactual and factual distributions
cqr_cf <- cqr_merg %>%
  mutate(sbp = sbp + coef,
         group = "Counterfactual") %>%
  select(group, sbp)

cqr_f <- cqr_merg %>%
  mutate(group = "Factual") %>%
  select(group, sbp)

cqr_plot <- rbind(cqr_f, cqr_cf)
cqr_plot$group <- relevel(factor(cqr_plot$group), ref = "Factual")


#Plotting
cqr_dens_plot <- ggplot(data = cqr_plot, aes(x = sbp, color = group,
                                             linetype = group)) +
  geom_density(aes(y = after_stat(density)), size = 1.5) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 14),
        legend.position = "bottom") +
  labs(title = "B.",
       color = "",
       linetype = "",
       x = "Systolic Blood Pressure (mmHg)",
       y = "Density") +
  scale_color_manual(values = c("#D3D3D3", "#808080"))

cqr_dens_plot



#UQR
uqr_coef <- uqr %>%
  dplyr::select(Quantile, Estimate) %>%
  rename("quant" = "Quantile",
         "coef" = "Estimate")

uqr_red <- data %>%
  dplyr::select(sbp) %>%
  mutate(quant = xtile(sbp, n = 99))

uqr_merg <- right_join(uqr_coef, uqr_red, by = "quant")

#Create counterfactual and factual distributions
uqr_cf <- uqr_merg %>%
  mutate(sbp = sbp + coef,
         group = "Counterfactual") %>%
  select(group, sbp)

uqr_f <- uqr_merg %>%
  mutate(group = "Factual") %>%
  select(group, sbp)

uqr_plot <- rbind(uqr_f, uqr_cf)
uqr_plot$group <- relevel(factor(uqr_plot$group), ref = "Factual")

#Plotting
uqr_dens_plot <- ggplot(data = uqr_plot, aes(x = sbp, color = group,
                                             linetype = group)) +
  geom_density(aes(y = after_stat(density)), size = 1.5) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 14),
        legend.position = "bottom") +
  labs(title = "D.",
       color = "",
       linetype = "",
       x = "Systolic Blood Pressure (mmHg)",
       y = "Density") +
  scale_color_manual(values = c("#D3D3D3", "#808080"))

uqr_dens_plot


#Estimation and counterfactual plots together (Manuscript figure)
ggarrange(ols_cqr_p, cqr_dens_plot, nrow = 1, ncol = 2)
ggarrange(ols_uqr_p, uqr_dens_plot, nrow = 1, ncol = 2)

#-------------------------------------------------------------------------------
# Density plots by age (Manuscript figure)
#-------------------------------------------------------------------------------
#Create high/low education
data$educ <- ifelse(data$schlyrs < 12, "Low", "High")
table(data$educ, data$schlyrs)
data$educ <- relevel(factor(data$educ), ref = "Low")

#Below 60
d59 <- data %>%
  dplyr::filter(age < 60) %>%
  dplyr::select(hhidpn, age, educ, sbp)
summary(d59)

#60-69
d60 <- data %>%
  dplyr::filter(between(age, 60, 69)) %>%
  dplyr::select(hhidpn, age, educ, sbp)
summary(d60)

#70-79
d70 <- data %>%
  dplyr::filter(between(age, 70, 79)) %>%
  dplyr::select(hhidpn, age, educ, sbp)
summary(d70)

#80+
d80 <- data %>%
  dplyr::filter(age >= 80) %>%
  dplyr::select(hhidpn, age, educ, sbp)
summary(d80)


#Create density plot for each age group
den59 <- ggplot(aes(y = sbp, fill = educ, color = educ), data = d59) +
  geom_density(alpha = 0.3) +
  ylim(70, 240) +
  labs(title = "A.",
       x = "",
       y = "",
       fill = "Education",
       color = "Education") +
  scale_fill_manual(values = c("#D3D3D3", "#808080")) +
  scale_color_manual(values = c("#D3D3D3", "#808080")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 14),
        legend.position = "none") 
  

den60 <- ggplot(aes(y = sbp, fill = educ, color = educ), data = d60) +
  geom_density(alpha = 0.3) +
  ylim(70, 240) +
  labs(title = "B.",
       x = "",
       y = "",
       fill = "Education",
       color = "Education") +
  scale_fill_manual(values = c("#D3D3D3", "#808080")) +
  scale_color_manual(values = c("#D3D3D3", "#808080")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) 


den70 <- ggplot(aes(y = sbp, fill = educ, color = educ), data = d70) +
  geom_density(alpha = 0.3) +
  ylim(70, 240) +
  labs(title = "C.",
       x = "",
       y = "",
       fill = "Education",
       color = "Education") +
  scale_fill_manual(values = c("#D3D3D3", "#808080")) +
  scale_color_manual(values = c("#D3D3D3", "#808080")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) 


den80 <- ggplot(aes(y = sbp, fill = educ, color = educ), data = d80) +
  geom_density(alpha = 0.3) +
  ylim(70, 240) +
  labs(title = "D.",
       x = "",
       y = "",
       fill = "Education",
       color = "Education") +
  scale_fill_manual(values = c("#D3D3D3", "#808080")) +
  scale_color_manual(values = c("#D3D3D3", "#808080")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) 

#Combine all plots
dens_plot <- ggarrange(den59, den60, den70, den80, nrow = 1, ncol = 4,
                       common.legend = TRUE, legend = "bottom")
annotate_figure(dens_plot, left =
                  textGrob("Systolic Blood Pressure (mmHg)",
                           rot = 90, vjust = 1, gp = gpar(cex = 1.3)))

#Create dataframes for high/low education for each age group
l59 <- d59 %>% dplyr::filter(educ == "Low")
h59 <- d59 %>% dplyr::filter(educ == "High")
l60 <- d60 %>% dplyr::filter(educ == "Low")
h60 <- d60 %>% dplyr::filter(educ == "High")
l70 <- d70 %>% dplyr::filter(educ == "Low")
h70 <- d70 %>% dplyr::filter(educ == "High")
l80 <- d80 %>% dplyr::filter(educ == "Low")
h80 <- d80 %>% dplyr::filter(educ == "High")

#Find mean and q25 cutoffs for each age group/education level
means <- data.frame("group" = c("<60", "<60", "60-69", "60-69",
                                "70-79", "70-79", "80+", "80+"),
                    "educ" = rep(c("Low", "High"), 4),
                    "mean" = c(mean(l59$sbp), mean(h59$sbp),
                               mean(l60$sbp), mean(h60$sbp),
                               mean(l70$sbp), mean(h70$sbp),
                               mean(l80$sbp), mean(h80$sbp)))

q25s <- data.frame("group" = c("<60", "<60", "60-69", "60-69",
                               "70-79", "70-79", "80+", "80+"),
                    "educ" = rep(c("Low", "High"), 4),
                    "mean" = c(as.numeric(quantile(l59$sbp, probs = 0.25)),
                               as.numeric(quantile(h59$sbp, probs = 0.25)),
                               as.numeric(quantile(l60$sbp, probs = 0.25)),
                               as.numeric(quantile(h60$sbp, probs = 0.25)),
                               as.numeric(quantile(l70$sbp, probs = 0.25)),
                               as.numeric(quantile(h70$sbp, probs = 0.25)),
                               as.numeric(quantile(l80$sbp, probs = 0.25)),
                               as.numeric(quantile(h80$sbp, probs = 0.25))))

#-------------------------------------------------------------------------------
# q10-90 graphics
#-------------------------------------------------------------------------------
#Restrict to 10-90th quantiles
results_min <- results %>%
  dplyr::filter(Quantile == 0 | between(Quantile, 10, 91)) %>% #Between works different for CQR and UQR
  dplyr::filter(Quantile != 91)
table(results_min$Quantile) #10-90

results_min <- rbind(ols, results_min)
results_min$Quantile <- ifelse(results_min$regtype == "OLS", 0,
                               results_min$Quantile)

#All results
ggplot(data = results_min %>% filter(regtype != "OLS"),
       aes(x = Quantile, y = Estimate, group = regtype,
           color = regtype, fill = regtype)) +
  geom_line(alpha = 50, linewidth = 0.75) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.27, color = NA) +
  geom_point(data = results_min %>% filter(regtype == "OLS")) +
  geom_errorbar(data = results_min %>% filter(regtype == "OLS"),
                aes(ymin = Lower, ymax = Upper), width = 0.01) +
  geom_hline(yintercept = 0, alpha = 0.5) + 
  geom_vline(xintercept = 5, color = "gray", linetype = "dashed") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")  + 
  scale_color_manual(breaks = c("OLS", "CQR", "UQR"),
                     labels = c("OLS", "CQR", "UQR"),
                     values = c("#F2494C", "#468C8A", "#FF9300")) +
  scale_fill_manual(breaks = c("OLS", "CQR", "UQR"),
                    labels = c("OLS", "CQR", "UQR"),
                    values = c("#F2494C", "#468C8A", "#FF9300"))  +
  scale_x_continuous(limits = c(-5, 90),
                     breaks = c(0, 10, 20, 30, 40, 50,
                                60, 70, 80, 90), 
                     labels = c("OLS", "q10", "q20", "q30", "q40",
                                "q50", "q60", "q70", "q80", "q90")) + 
  labs(x = "",
       y = expression("Change in SBP (mmHg)"),
       color = "Model", group = "Model", fill = "Model") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(size = 20))


#OLS and CQR
ols_cqr_min <- results_min %>%
  dplyr::filter(regtype != "UQR")

ols_cqr_min_p <- ggplot(data = ols_cqr_min %>% filter(regtype != "OLS"),
                        aes(x = Quantile, y = Estimate, group = regtype,
                            color = regtype, fill = regtype)) +
  geom_line(alpha = 50, size = 0.75) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.27, color = NA) +
  geom_point(data = ols_cqr_min %>% filter(regtype == "OLS")) +
  geom_errorbar(data = ols_cqr_min %>% filter(regtype == "OLS"),
                aes(ymin = Lower, ymax = Upper), width = 0.01) +
  geom_hline(yintercept = 0, alpha = 0.5, color = "#000000") + 
  geom_vline(xintercept = 5, color = "red", linetype = "dashed") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 14),) + 
  scale_color_manual(values = c("#808080", "#808080")) +
  scale_fill_manual(values = c("#808080", "#808080"))  + 
  #scale_color_manual(values = c("#468C8A", "#F2494C")) + #For colored plot
  #scale_fill_manual(values = c("#468C8A", "#F2494C"))  + 
  scale_x_continuous(limits = c(-5, 90),
                     breaks = c(0, 10, 20, 30, 40, 50,
                                60, 70, 80, 90), 
                     labels = c("OLS", "Q10", "Q20", "Q30", "Q40",
                                "Q50", "Q60", "Q70", "Q80", "Q90")) +
  labs(title = "OLS and CQR Estimates",
       x = "",
       y = "Change in Systolic Blood Pressure (mmHg)")



#OLS and UQR
ols_uqr_min <- results_min %>%
  dplyr::filter(regtype != "CQR")

ols_uqr_min_p <- ggplot(data = ols_uqr_min %>% filter(regtype != "OLS"),
                    aes(x = Quantile, y = Estimate, group = regtype,
                        color = regtype, fill = regtype)) +
  geom_line(alpha = 50, size = 0.75) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.27, color = NA) +
  geom_point(data = ols_uqr_min %>% filter(regtype == "OLS")) +
  geom_errorbar(data = ols_uqr_min %>% filter(regtype == "OLS"),
                aes(ymin = Lower, ymax = Upper), width = 0.01) +
  geom_hline(yintercept = 0, alpha = 0.5, color = "#000000") + 
  geom_vline(xintercept = 5, color = "red", linetype = "dashed") + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 14)) + 
  scale_color_manual(values = c("#808080", "#808080")) +
  scale_fill_manual(values = c("#808080", "#808080"))  + 
  #scale_color_manual(values = c("#F2494C", "#FF9300")) + #For colored plot
  #scale_fill_manual(values = c("#F2494C", "#FF9300"))  + 
  scale_x_continuous(limits = c(-5, 90),
                     breaks = c(0, 10, 20, 30, 40, 50,
                                60, 70, 80, 90), 
                     labels = c("OLS", "Q10", "Q20", "Q30", "Q40",
                                "Q50", "Q60", "Q70", "Q80", "Q90")) + 
  labs(title = "OLS and UQR Estimates",
       x = "",
       y = "Change in Systolic Blood Pressure (mmHg)")


#CQR and UQR plots together
ols_cqr_min_p / ols_uqr_min_p

