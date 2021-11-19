library(brms)
library(dplyr)
# library(cmdstanr)

# load data
syntax_rule_data = read.csv("COLING - Copy/syntax_rule_mixed_effect_term_0625.csv", header = T, stringsAsFactors = F)

# preprocessing
study_term <- c("F2018", "S2019",'F2019-1')
syntax_rule_data_study <- subset(syntax_rule_data, term %in% study_term)

syntax_rule_data_study = syntax_rule_data_study %>%
  mutate(term_group = paste0(term, sep = '_', Group ),
         group_speaker = paste0(term_group, sep = '_', Speaker ))

# factorize the random effect variables
syntax_rule_data_study$key = factor(syntax_rule_data_study$key)
syntax_rule_data_study$term_group = factor(syntax_rule_data_study$term_group)
syntax_rule_data_study$group_speaker = factor(syntax_rule_data_study$group_speaker)

# full model

syntax_rule_full <- brm(count_target | trunc(lb=1) ~ count_prime_own + count_prime_partner +
                (1 | group)+ (1|group_speaker)+ (1|key) + (1|group_turn_id) + (1|term),
                data = syntax_rule_data_study, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))

# save model
saveRDS(syntax_rule_full, "syntax_rule_full.rds" )

# model with only count_prime_own
syntax_rule_only_cp_own <- brm(count_target | trunc(lb=1) ~ count_prime_own +
                (1 | group)+ (1|group_speaker)+ (1|key) + (1|group_turn_id) + (1|term),
                data = syntax_rule_data_study, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))

# save model
saveRDS(syntax_rule_only_cp_own, "syntax_rule_only_cp_own.rds")

# model with only count_prime_partner
syntax_rule_only_cp_partner <- brm(count_target | trunc(lb=1) ~ count_prime_partner +
               (1 | group)+ (1|group_speaker)+ (1|key) + (1|group_turn_id) + (1|term),
                data = syntax_rule_data_study, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))

# save model
saveRDS(syntax_rule_only_cp_partner, "syntax_rule_only_cp_partner.rds")

# null model
syntax_rule_null <- brm(count_target | trunc(lb=1) ~
                (1 | group)+ (1|group_speaker)+ (1|key) + (1|group_turn_id) + (1|term),
                data = syntax_rule_data_study, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))

# save model
saveRDS(syntax_rule_null, "syntax_rule_null.rds")

# SUMMARY MODELS
# data
syntax_rule_sum = syntax_rule_data_study %>%
  filter(term != 'F2019-2')%>% # filter the F2019-2 data
  group_by(term, term_group, group_speaker, word)%>%
  summarize(turnContainsCount = n(),
            count_target = sum(count_target),
            count_prime_own = sum(count_prime_own),
            count_prime_partner = sum(count_prime_partner))


# full summary model
sum_syntax_rule_full <- brm(count_target | trunc(lb=1) ~ count_prime_own + count_prime_partner +
                (1 | group)+ (1|group_speaker)+ (1|key) + (1|group_turn_id) + (1|term),
                data = syntax_rule_sum, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))
# save full model
saveRDS(sum_syntax_rule_full, "sum_syntax_rule_full.rds")

# summary model with only count_prime_own
sum_syntax_rule_only_cp_own <- brm(count_target | trunc(lb=1) ~ count_prime_own +
                (1 | group)+ (1|group_speaker)+ (1|key) + (1|group_turn_id) + (1|term),
                data = syntax_rule_sum, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))
# save cp_own model
saveRDS(sum_syntax_rule_only_cp_own, "sum_syntax_rule_only_cp_own.rds")

# full summary model
sum_syntax_rule_only_cp_partner <- brm(count_target | trunc(lb=1) ~ count_prime_partner +
                (1 | group)+ (1|group_speaker)+ (1|key) + (1|group_turn_id) + (1|term),
                data = syntax_rule_sum, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))
# save full model
saveRDS(sum_syntax_rule_only_cp_partner, "sum_syntax_rule_only_cp_partner.rds")

# null summary model
sum_syntax_rule_null <- brm(count_target | trunc(lb=1) ~
                (1 | group)+ (1|group_speaker)+ (1|key) + (1|group_turn_id) + (1|term),
                data = syntax_rule_sum, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))
# save null model
saveRDS(sum_syntax_rule_null, "sum_syntax_rule_null.rds")
