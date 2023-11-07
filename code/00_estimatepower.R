

############################
# Power analysis: OSSE Reimagining DC High Schools 
# Analyst: Rebecca A. Johnson
# Reviewer: Ryan T. Moore
############################

# Packages
library(estimatr)
library(here)
library(reshape2)
library(scales)
library(tidyverse)

# Colors
lab_blue <- "#2B4888"
lab_pink <- "#DE4057"


## Treatment group sample sizes:
# Cohort 1: AIP participants SY21-22: n=75                      Kilin/One Note
# Cohort 2: AIP participants SY22-23: n=193                     Kilin/One Note
# Cohort 3: AIP participants SY23-24: n = 250                   Slack
# Full sample: AIP participants SY21-22 and SY22-23: n=268      Kilin/One Note

## Control group sample sizes:
# Cohort 1: CTE - AIP: 1225                                     Correspondence
# Cohort 2: CTE - AIP: 1102                                     Correspondence
# Cohort 3: CTE - AIP: 1050                                     Correspondence


# Number of secondary students - CTE participants: n=3724                        Perkins V DC2021
# Number of secondary students - 3rd + 4th level CTE concentrators: n=1300       Perkins V DC2021 + email communication
# we use concentrators and divide by two below to reflect three cohorts

## Main confirmatory outcomes
# Enrolled in post-secondary education w/in 1 year HS exit:         277/417; p = 0.66           CAR report Table 3S1
# Employed (general labor force participation rate):                63/417; p = 0.15            CAR report Table 3S1
# College/career readiness index (update later)                     mu = 6; sd = 1              N/A   


#' one_sim_binary_coh
#'
#' Simulates one iteration of generating binary outcomes/estimating tx effect
#' Tx effect for cohort 1 allowed to differ due to pilot stage/shorter internship duration
#' 
#' @param cohort_info nested list where each element is a cohort; each cohort contains df w/ cohort indicator and prob tx 
#' @param tx_effect treatment effect in percentage point terms
#' @param y_br base rate for y
#' @param KEEP_ALL assume matching/weighting keeps all control group members (default is TRUE) 
#' @param prop_attenuate how much tx effect is attenuated in cohort 1 (default is 0.5)
#' 
#' @return p value for that simulation - from lm regressing outcome on tx indicator
one_sim_binary_coh <- function(cohort_info, tx_effect, y_br, prop_to_keep = 1, prop_attenuate = 0.5){
  
  # simulate data
  ## iterate over each cohort's item and return dataframe with treatment indicator
  ## separate since diff tx probabilities
  cohort_wtx_indic <- lapply(cohort_info, 
                             function(x) x$df %>% mutate(z_tmp = rbinom(nrow(x$df), 1, 
                                                              prob = x$prob)))
  
  ## separate into tx and control - separate since diff DGP for outcome/Y
  cohorts_tx <- lapply(cohort_wtx_indic, function(x) x %>% filter(z_tmp == 1))
  cohorts_control <- lapply(cohort_wtx_indic, function(x) x %>% filter(z_tmp == 0))
  
  ## calculate outcomes in treatment group
  ## assume we keep all treatment members; tx effect half for c1; whole for others
  cohort_tx_outcomes <- lapply(cohorts_tx,
                               function(x) if(unique(x$cohort) == "cohort_1"){
                                 tibble(z_tmp = 1,
                                        y_tmp = sample(c(TRUE, FALSE), nrow(x), replace = TRUE, 
                                                       prob = c(tx_effect*prop_attenuate  + y_br, 
                                                                1 - (tx_effect*prop_attenuate + y_br))))
                               } else{
                                 tibble(z_tmp = 1,
                                        y_tmp = sample(c(TRUE, FALSE), nrow(x), replace = TRUE, 
                                                       prob = c(tx_effect  + y_br, 1 - (tx_effect + y_br))))
                               }) 
  # return(cohort_tx_outcomes) # uncomment for debugging, making sure cohort 1 mean y is lower than cohort 2 and 3 if 
  # attenuating the treatment effect
  cohort_tx_outcomes_df <- do.call(rbind.data.frame, cohort_tx_outcomes) # rowbind into a single dataframe
  
  ## calculate outcomes in control group under diff assumptions of whether we keep all controls
  cohort_control_outcomes <- lapply(cohorts_control,
                                    function(x) tibble(z_tmp = 0,
                                                       y_tmp = sample(c(TRUE, FALSE), round(nrow(x)*prop_to_keep, 0), replace = TRUE, 
                                                                      prob = c(y_br, 1 - y_br))))
  
  ## bind the dataframes
  cohort_control_outcomes_df <- do.call(rbind.data.frame, cohort_control_outcomes) # rowbind into a single dataframe
  df_woutcome <- rbind.data.frame(cohort_tx_outcomes_df, cohort_control_outcomes_df) # rowbind tx and control
  
  ## estimate the model
  lm_out <- lm_robust(y_tmp ~ z_tmp, data = df_woutcome)
  p_tmp <- lm_out$p.value["z_tmp"]
  names(p_tmp) <- "pval"
  return(p_tmp)
  
}

#' one_sim_survey_continuous
#'
#' Simulates one iteration of generating binary outcomes/estimating tx effect
#' Tx effect for cohort 1 allowed to differ due to pilot stage/shorter internship duration
#' 
#' @param n_samp sample size
#' @param tx_effect treatment effect in terms of changes in continuous dv
#' @param tx_prob probability of treatment (defaults to mean of cohorts 2 and 3)
#' 
#' @return p value for that simulation - from lm regressing outcome on tx indicator
one_sim_survey_continuous <- function(n_samp, tx_effect, tx_prob = mean_c2c3_prob){
  
  ## get true estimate
  df_permute <- tibble(z_tmp = rbinom(n_samp, 1, prob = tx_prob)) %>%
    mutate(y_tmp = z_tmp * tx_effect + rnorm(n_samp, mean = 6, sd = 1))
  lm_out <- lm_robust(y_tmp ~ z_tmp, data = df_permute)
  stat_sig_tmp <- lm_out$p.value["z_tmp"]
  return(stat_sig_tmp)
  
}


#####################
# Set up constants/function inputs for binary outcomes in administrative data sample 
######################

# Set base rates
college_enroll_baserate <- 0.66
employ_baserate <- 0.15
grad_rate <- 0.97
combined_baserate <- (college_enroll_baserate + employ_baserate - 0.05) * grad_rate # or logic, so assuming additive and then 
# subtracting out overlap we assume is 5%; since denom in CAR report
# is out of those who graduate high school, discount by high school graduation rate from CAR report

# Store the sample sizes from above
## treatment group
n_treat_c1 <- 75
n_treat_c2 <- 193
n_treat_c3 <- 250 
n_treat_total <- sum(n_treat_c1, n_treat_c2, n_treat_c3)

## control group: we're assuming despite different treatment sizes,
## similar numbers of control 3rd and 4th level CTE so just divide our 
## number above by 2 (since reflects two cohorts) and multiply by 3
## other approach is to adjust cohort size by tx group relative size
## but we decided constant size makes more sense
n_control_c1 <- 1225
n_control_c2 <- 1102
n_control_c3 <- 1050 
n_control_total <- n_control_c1 + n_control_c2 + n_control_c3 

## total 
n_samp <- n_treat_total + n_control_total

## get size of each cohort across tx and control
c1_N <- n_treat_c1 + n_control_c1
c2_N <- n_treat_c2 + n_control_c2
c3_N <- n_treat_c3 + n_control_c3

## create data with cohort indicator (since function treats cohort 1 differently) 
df_c1 <- tibble(cohort = rep("cohort_1", each = c1_N))
df_c2 <- tibble(cohort = rep("cohort_2", each = c2_N))
df_c3 <- tibble(cohort = rep("cohort_3", each = c3_N))

## calculate cohort-specific treatment probabilities - note increasing prob
## across cohorts
c1_prob <- n_treat_c1/c1_N
c2_prob <- n_treat_c2/c2_N
c3_prob <- n_treat_c3/c3_N
mean_c2c3_prob <- mean(c2_prob, c3_prob)

## store in a nested list for use in function defined above
cohort_info <- list(c1 = list(df = df_c1, prob = c1_prob),
                    c2 = list(df = df_c2, prob = c2_prob),
                    c3 = list(df = df_c3, prob = c3_prob))

## create version of data without cohort 1 for scenario that drops then
cohort_minusc1 <- cohort_info %>% purrr::list_modify("c3" = NULL)

## before simulations, do gut checks w/ a few different parameters
set.seed(91988)

#####################
# Simulate 6 the different scenarios
# 1. Keep cohort 1, same tx effect, all controls
# 2. Keep cohort 1, same tx effect, lose controls
# 3. Keep cohort 1, half tx effect, all controls
# 4. Keep cohort 1, half tx effect, lose controls
# 5. Keep cohort 1, 0 tx effect for that c, all controls
# 6. Keep cohort 1, 0 tx effect for that c, lose controls
# 7. Drop cohort 1, keep all controls (for other cohorts)
# 8. Drop cohort 1, lose controls (for other cohorts)
######################
set.seed(91988)

# real parameters
n_sims <- 1000
tx_effects <- seq(from = 0.02, to = 0.10, by = 0.005) # iterate between 2 and 10 pp based on past research

# test parameters for quicker runtime
tx_effects_test <- seq(from = 0.02, to = 0.03, by = 0.05) # test treatment effects for code changes
n_sims_test <- 10

# storage perameters
all_p <- vector("list", length = 2) # initialize empty list to store scenario-specific p values
names(all_p) <- sprintf("scenario_%s", 1:2) # name list elements

# for each scenario, iterate over tx effects and run simulation
# with the scenario's parameters 
# for the published plan, we just do two scenarios 
# for other decisions, we also included scenarios that
# varied whether attenuated treatment effect for cohort 1
# (so prop_attenuate != 1)

# scenario 1: keep cohort 1 (cohort_info), same tx effect (prop_attenuate = 1), all controls (keep_all default)
for(i in 1:length(tx_effects)){
  one_res <- replicate(n_sims, 
                       one_sim_binary_coh(cohort_info = cohort_info, 
                                          tx_effect = tx_effects[i],
                                          prop_attenuate = 1,
                                          y_br = combined_baserate))
  all_p$scenario_1[i] <- mean(one_res <= 0.05)
}

# scenario 2: keep cohort 1, same tx effect, lose 40% of controls
for(i in 1:length(tx_effects)){
  one_res <- replicate(n_sims, 
                       one_sim_binary_coh(cohort_info = cohort_info, 
                                          tx_effect = tx_effects[i],
                                          prop_attenuate = 1,
                                          y_br = combined_baserate,
                                          prop_to_keep = 0.6))
  all_p$scenario_2[i] <- mean(one_res <= 0.05)
}

summary_poweradmin <- do.call(cbind.data.frame, all_p) %>%
  mutate(effect_size_pp = tx_effects*100)

## melt to long format
summary_poweradmin_long <- reshape2::melt(summary_poweradmin, id.var = "effect_size_pp") %>%
                          mutate(power_show = value * 100,
                                 variable = ifelse(variable == "scenario_1",
                                                   "Keep all controls",
                                                   "Lose 40% of controls"))


## for plot for pre-ap, just do tx effect same for cohort 1
## and contrast the different matching/weighting lose of controls

## plot the distribution of effect sizes on x axis; power on y axis 
ggplot(summary_poweradmin_long, aes(x = effect_size_pp, y = power_show, 
                                    group = variable, color = variable)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 15, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  xlab("Percentage point change in enrollment outcome (employment or college)") +
  ylab("Statistical power (alpha = 0.05)") +
  geom_hline(yintercept = 80,
             color = "red",
             linetype = "dashed") +
  theme_classic(base_size = 24) +
  theme(legend.position = c(0.8, 0.2)) +
  scale_color_manual(values = c("Keep all controls" = lab_blue, 
                                "Lose 40% of controls" = lab_pink)) +
  labs(color = "Matching result")

## save result
ggsave(here("figs/power_curve.png"),
       plot = last_plot(),
       device = "png",
       width = 12,
       height = 8)

#####################
# simulation-based power for survey outcomes
######################

## 4-point scale (strongly disagree to strongly agree)
## 3 questions
## code as 0 = strongly disagree to 3 = strongly agree
## max is 9; min is 0
samp_size_combined <- c2_N + c3_N
samp_size_1cohort <- c2_N # doing cohort 2 since smaller
samp_size_compare <- round(c(samp_size_combined * c(0.2, 0.4, 0.6), 
                             samp_size_1cohort * c(0.2, 0.4, 0.6)))
tx_effect_compare <- seq(0.1, 0.8, by = 0.1)
all_combos <- expand.grid(samp_size_compare, tx_effect_compare)
colnames(all_combos) <- c("n_respond", "tx_effect")
all_sims <- list()
for(i in 1:nrow(all_combos)){
  
  tx_effect = all_combos$tx_effect[i]
  n_samp = all_combos$n_respond[i]
  all_p  <- replicate(1000, one_sim_survey_continuous(n_samp = n_samp, tx_effect = tx_effect))
  all_power <- mean(all_p <= 0.05)
  all_sims[[sprintf("n: %s; effect: %s", n_samp, tx_effect)]] <- all_power
  
}

## consolidate into one dataframe
power_summary <- data.frame(power = unlist(all_sims),
                            condition = names(all_sims)) %>%
  mutate(n_respond = gsub("n:\\s([0-9]+);.*", "\\1", condition),
         tx_effect = gsub(".*effect:\\s([0-9]\\.[0-9])", "\\1", condition),
         prop_respond_both = as.numeric(n_respond)/samp_size_combined,
         prop_respond_one = as.numeric(n_respond)/samp_size_1cohort)
rownames(power_summary) <- NULL

## organize by n respond
power_summary %>%
  select(contains("prop_respond"), tx_effect, power) %>%
  arrange(desc(power)) %>%
  filter(prop_respond_one == 0.6)
