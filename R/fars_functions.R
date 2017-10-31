##### Functions for Fars Analysis | R Programming | Fall 2017 | Ethan Walker
library(tidyverse)

#Confidence intervals for proportions
perc_cis <- function(x, n) {
  proportion <- x / n
  standard_error <- sqrt((proportion * (1 - proportion)) / n)
  upper_ci <- (proportion + (1.96 * standard_error))
  lower_ci <- (proportion - (1.96 * standard_error))
  proportion_perc <- round((proportion * 100), digits = 1)
  upper_ci_perc <- round((upper_ci * 100), digits = 1)
  lower_ci_perc <- round((lower_ci * 100), digits = 1)
  final_results <- paste0(proportion_perc, "% (", lower_ci_perc, 
                          "%, ", upper_ci_perc, "%)")
  final_results
}


#Testing for trend using Cochran-Armitage trend test
test_trend_ca <- function(drug, data = clean_fars) {
  if(drug == "Nonalcohol"){
    to_test <- clean_fars %>%
      filter(drug_type != "Alcohol") %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
  } else{
    to_test <- clean_fars %>%
      filter(drug_type == drug) %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
  }
  ca_alcohol <- prop.trend.test(x = to_test$positive,
                                n = to_test$trials)
  Z <- round(sqrt(ca_alcohol$statistic), digits = 1)
  p.value <- round(ca_alcohol$p.value, digits = 3)
  final_results <- data.frame(Z, p.value)
  tibble::remove_rownames(final_results)
  return(final_results)
}


#Testing for trend using logistic regression
test_trend_log_reg <- function(drug, data = clean_fars) {
  if(drug == "Nonalcohol"){
    to_test <- clean_fars %>%
      filter(!is.na(drug_type)) %>% 
      filter(drug_type != "Alcohol")
  } else{
    to_test <- clean_fars %>%
      filter(!is.na(drug_type)) %>%
      filter(drug_type == drug)
  }
  log_reg <- glm(positive_for_drug ~ year, data = to_test,
                 family = binomial(link = "logit"))
  log_reg_sum <- slice(broom::tidy(log_reg), 2)
  Z <- round(log_reg_sum$statistic, digits = 1)
  p.value <- round(log_reg_sum$p.value, digits = 3)
  final_results <- data.frame(Z, p.value)
  tibble::remove_rownames(final_results) 
  return(final_results)
}

