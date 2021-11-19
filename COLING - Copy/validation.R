library(brms)
library(dplyr)
library(cmdstanr)
library(loo)

# Full Model To Test
model_file_full = "COLING - Copy/all_words_full.rds"
# Model without factor to test or NULL model
model_file_null = "COLING - Copy/all_words_null.rds"
# Model with only cp own
model_file_cp_own = "COLING - Copy/all_words_only_cp_own.rds"
# Model with only cp partner
model_file_cp_partner = "COLING - Copy/all_words_only_cp_partner.rds"
# data file for data to use
data_file = "COLING - Copy/single_word_mixed_effect_term_0625.csv"

# load model
model_full <- readRDS(model_file_full)
model_null <- readRDS(model_file_null)
model_cp_own <- readRDS(model_file_cp_own)
model_cp_partner <- readRDS(model_file_cp_partner)


# load data
word_list_data = read.csv(data_file, header = T, stringsAsFactors = F)

# preprocessing
study_term <- c("F2018", "S2019",'F2019-1')
word_list_data_study <- subset(word_list_data, term %in% study_term)

word_list_data_study = word_list_data_study %>%
  mutate(term_group = paste0(term, sep = '_', Group ),
         group_speaker = paste0(term_group, sep = '_', Speaker ))

# factorize the random effect variables
word_list_data_study$word = factor(word_list_data_study$word)
word_list_data_study$term_group = factor(word_list_data_study$term_group)
word_list_data_study$group_speaker = factor(word_list_data_study$group_speaker)

# WAIC/LOO

# Full Model

model_full <- add_criterion(model_full, c("waic","loo"), save_psis = TRUE, loo_moment_match = TRUE)
print(model_full$criteria)

saveRDS(model_full, "all_words_model_full_with_criteria.rds")

# Null Model

model_null <- add_criterion(model_null, c("waic","loo"), save_psis = TRUE, loo_moment_match = TRUE)

# cp own Model

model_cp_own <- add_criterion(model_cp_own, c("waic","loo"), save_psis = TRUE, loo_moment_match = TRUE)

# cp partner Model

model_cp_partner <- add_criterion(model_cp_partner, c("waic","loo"), save_psis = TRUE, loo_moment_match = TRUE)

loo_compare(model_full, model_null, model_cp_own, model_cp_partner, criterion = "waic")
loo_compare(model_full, model_null, model_cp_own, model_cp_partner, criterion = "loo")

# Bayes Factor

brm_bf = bayes_factor(model_full, model_null)
BF = c(0,brm_bf$bf)

bayes_R2(model_full)

model_weights(model_full, model_null, weights = "loo")

plot(model_full$criteria$loo)

yrep <- posterior_predict(model_full)

ppc_loo_pit_overlay(
  y = model_full$data$count_target,
  yrep = yrep,
  lw = weights(model_full$criteria$loo$psis_object)
)

yrep <- posterior_predict(model_null)

ppc_loo_pit_overlay(
  y = model_null$data$count_target,
  yrep = yrep,
  lw = weights(model_null$criteria$loo$psis_object)
)