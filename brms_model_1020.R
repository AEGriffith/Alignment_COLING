
# 1. all word models
## 1.1 single word
word_list_data = read.csv("single_word_mixed_effect_term_0625.csv", header = T, stringsAsFactors = F)

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

# building brms models 
model1_1 <- brm(count_target | trunc(lb=1) ~ count_prime_own + count_prime_partner +
                (1 | group)+ (1|group_speaker)+ (1|word) + (1|group_turn_id) + (1|term),
              data = word_list_data_study, family = poisson(),
                chains = 4, cores = 4, backend = 'cmdstanr', threads = threading(8))
summary(model1_1)
# 1.2 word aggregated across sentences for each speaker

# aggregate across sentences
all_word_sum = word_list_data_study %>%
  filter(term != '')%>% # filter the F2019-2 data
  group_by(term, term_group, group_speaker, word)%>%
  summarize(turnContainsCount = n(),
            count_target = sum(count_target), 
            count_prime_own = sum(count_prime_own),
            count_prime_partner = sum(count_prime_partner))
# model
model1_2 <- brm(count_target | trunc(lb=1) ~ count_prime_own + count_prime_partner +
                  (1 | term_group)+ (1|group_speaker)+ (1|word) + (1|term),
                data = all_word_sum, family = poisson(),
chains = 4, cores = 4, backend = 'cmdstanr', threads = threading(8))
summary(model1_2)


# 2. task-relavant word models
word_list_data = read.csv("task_relevant_word_mixed_effect_term_0625.csv", header = T, stringsAsFactors = F)

# preprocessing 
word_list_data_study <- subset(word_list_data, term %in% study_term)

word_list_data_study = word_list_data_study %>%
  mutate(term_group = paste0(term, sep = '_', Group ),
         group_speaker = paste0(term_group, sep = '_', Speaker ))

# factorize the random effect variables
word_list_data_study$word = factor(word_list_data_study$word)
word_list_data_study$term_group = factor(word_list_data_study$term_group)
word_list_data_study$group_speaker = factor(word_list_data_study$group_speaker)

# building brms models 
model2_1 <- brm(count_target | trunc(lb=1) ~ count_prime_own + count_prime_partner +
                  (1 | group)+ (1|group_speaker)+ (1|word) + (1|group_turn_id) + (1|term),
                data = word_list_data_study, family = poisson(),
chains = 4, cores = 4, backend = 'cmdstanr', threads = threading(8))
summary(model2_1)
# 2.2 word aggregated across sentences for each speaker

# aggregate across sentences
all_word_sum = word_list_data_study %>%
  filter(term != '')%>% # filter the F2019-2 data
  group_by(term, term_group, group_speaker, word)%>%
  summarize(turnContainsCount = n(),
            count_target = sum(count_target), 
            count_prime_own = sum(count_prime_own),
            count_prime_partner = sum(count_prime_partner))
# model
model2_2 <- brm(count_target | trunc(lb=1) ~ count_prime_own + count_prime_partner +
                  (1 | term_group)+ (1|group_speaker)+ (1|word) + (1|term),
                data = all_word_sum, family = poisson(),
chains = 4, cores = 4, backend = 'cmdstanr', threads = threading(8))
summary(model2_2)

# 3. syntactic structure models
syntax_rule_data = read.csv("syntax_rule_mixed_effect_term_0625.csv", header = T, stringsAsFactors = F)

# preprocessing 
syntax_rule_data_study <- subset(syntax_rule_data, term %in% study_term)

syntax_rule_data_study = syntax_rule_data_study %>%
  mutate(term_group = paste0(term, sep = '_', Group ),
         group_speaker = paste0(term_group, sep = '_', Speaker ))

# factorize the random effect variables
syntax_rule_data_study$key = factor(syntax_rule_data_study$key)
syntax_rule_data_study$term_group = factor(syntax_rule_data_study$term_group)
syntax_rule_data_study$group_speaker = factor(syntax_rule_data_study$group_speaker)

# building brms models 
model3_1 = brm(count_target | trunc(lb=1) ~ count_prime_own + count_prime_partner +
                         (1 | group)+ (1|group_speaker)+ (1|key) + (1|group_turn_id) + (1|term),
                       data = syntax_rule_data_study, family = poisson(),
chains = 4, cores = 4, backend = 'cmdstanr', threads = threading(8))
  
summary(model3_1)
# 3.2 sytax rules aggregated across sentences for each speaker

# aggregate across sentences
syntax_rule_sum = syntax_rule_data_study %>%
  filter(term != '')%>% # filter the F2019-2 data
  group_by(term, term_group, group_speaker, key)%>%
  summarize(turnContainsCount = n(),
            count_target = sum(count_target), 
            count_prime_own = sum(count_prime_own),
            count_prime_partner = sum(count_prime_partner))
# model
model3_2 <- brm(count_target | trunc(lb=1) ~ count_prime_own + count_prime_partner +
                  (1 | term_group)+ (1|group_speaker)+ (1|key) + (1|term),
                data = syntax_rule_sum, family = poisson(),
chains = 4, cores = 4, backend = 'cmdstanr', threads = threading(8))
summary(model3_2)

