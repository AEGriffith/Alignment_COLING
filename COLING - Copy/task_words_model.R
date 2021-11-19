library(brms)
library(dplyr)
library(cmdstanr)


# load data
word_list_data = read.csv("task_relevant_word_mixed_effect_term_0625.csv", header = T, stringsAsFactors = F)

# preprocessing
study_term <- c("F2018", "S2019",'F2019-1')
word_list_data_study <- subset(word_list_data, term %in% study_term)

word_list_data_study = word_list_data_study %>%
  mutate(term_group = paste0(term, sep = '_', group ),
         group_speaker = paste0(term_group, sep = '_', Speaker ))

# factorize the random effect variables
word_list_data_study$word = factor(word_list_data_study$word)
word_list_data_study$term_group = factor(word_list_data_study$term_group)
word_list_data_study$group_speaker = factor(word_list_data_study$group_speaker)

# full model
task_words_full <- brm(count_target | trunc(lb=1) ~ count_prime_own + count_prime_partner +
                (1|group)+ (1|group_speaker)+ (1|word) + (1|group_turn_id) + (1|term),
                data = word_list_data_study, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 4, cores = 4, backend = "cmdstanr", threads = threading(8))

# save model
saveRDS(task_words_full, "task_words_full.rds" )

# model with only count_prime_own
task_words_only_cp_own <- brm(count_target | trunc(lb=1) ~ count_prime_own +
                (1 | group)+ (1|group_speaker)+ (1|word) + (1|group_turn_id) + (1|term),
                data = word_list_data_study, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))

# save model
saveRDS(task_words_only_cp_own, "task_words_only_cp_own.rds")

# model with only count_prime_partner
task_words_only_cp_partner <- brm(count_target | trunc(lb=1) ~ count_prime_partner +
                (1 | group)+ (1|group_speaker)+ (1|word) + (1|group_turn_id) + (1|term),
                data = word_list_data_study, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))

# save model
saveRDS(task_words_only_cp_partner, "task_words_only_cp_partner.rds")

# null model
task_words_null <- brm(count_target | trunc(lb=1) ~
                (1 | group)+ (1|group_speaker)+ (1|word) + (1|group_turn_id) + (1|term),
                data = word_list_data_study, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))

# save model
saveRDS(task_words_null, "task_words_null.rds")

# SUMMARY MODELS
# data
task_words_sum = word_list_data_study %>%
  filter(term != 'F2019-2')%>% # filter the F2019-2 data
  group_by(term, term_group, group_speaker, word)%>%
  summarize(turnContainsCount = n(),
            count_target = sum(count_target),
            count_prime_own = sum(count_prime_own),
            count_prime_partner = sum(count_prime_partner))


# full summary model
sum_task_words_full <- brm(count_target | trunc(lb=1) ~ count_prime_own + count_prime_partner +
                (1 | term_group)+ (1|group_speaker)+ (1|word) + (1|term),
                data = task_words_sum, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))
# save full model
saveRDS(sum_task_words_full, "sum_task_words_full.rds")


# summary model with only count_prime_own
sum_task_words_only_cp_own <- brm(count_target | trunc(lb=1) ~ count_prime_own +
                (1 | term_group)+ (1|group_speaker)+ (1|word) + (1|term),
                data = task_words_sum, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 4, cores = 6, backend = "cmdstanr", threads = threading(8))
# save cp_own model
saveRDS(sum_task_words_only_cp_own, "sum_task_words_only_cp_own.rds")

# full summary model
sum_task_words_only_cp_partner <- brm(count_target | trunc(lb=1) ~ count_prime_partner +
                (1 | term_group)+ (1|group_speaker)+ (1|word) + (1|term),
                data = task_words_sum, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))
# save full model
saveRDS(sum_task_words_only_cp_partner, "sum_task_words_only_cp_partner.rds")

# null summary model
sum_task_words_null <- brm(count_target | trunc(lb=1) ~
                (1 | term_group)+ (1|group_speaker)+ (1|word) + (1|term),
                data = task_words_sum, family = poisson(),
                save_pars = save_pars(all = TRUE ), control = list(adapt_delta=0.99, max_treedepth= 25),
                chains = 6, cores = 6, backend = "cmdstanr", threads = threading(12))
# save null model
saveRDS(sum_task_words_null, "sum_task_words_null.rds")
