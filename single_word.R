# Title     : TODO
# Objective : TODO
# Created by: amandagriffith
# Created on: 10/20/21

word_list_data = read.csv("task_word_sum_0929.csv", header = T, stringsAsFactors = F)
# start preprocessing
study_term <- c("F2018", "S2019", 'F2019-1')
word_list_data_study <- subset(word_list_data, term %in% study_term)
# word_list_data_study = word_list_data_study %>%
# mutate(term_group = paste0(term, sep = '_', Group ),
#        group_speaker = paste0(term_group, sep = '_', Speaker ))
# factorize the random effect variables
word_list_data_study$word = factor(word_list_data_study$word)
word_list_data_study$term_group = factor(word_list_data_study$term_group)
word_list_data_study$group_speaker = factor(word_list_data_study$group_speaker)

# model1 <- brm(count_target | trunc(lb=1) ~ count_prime_own + count_prime_partner +
#                (1 | group)+ (1|group_speaker)+ (1|word) + (1|group_turn_id) + (1|term),
#     data = word_list_data_study, family = poisson())
model2 <- brm(count_target | trunc(lb = 1) ~ count_prime_own +
  count_prime_partner +
  (1 | term_group),
              data = word_list_data_study, family = poisson())

summary(model2)