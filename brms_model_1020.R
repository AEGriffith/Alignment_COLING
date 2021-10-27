
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

# checking convergence and fit
summary(model1_1)
# newly added code
plot(model1_1)
pp = brms::pp_check(model1_1)
pp + theme_bw()
## Plotting model predictions
brms::marginal_effects(model1_1)
# get fitted values

# option 1: 
# newdata = data.frame(corpus = levels(word_list_data_study$corpus)) # Xiaoyi: not sure how to modify this for our data, maybe use 
# fit = fitted(model1_1, newdata = newdata, re_formula = NA) * 100  # convert to %

# option 2: 
## compute expected predictions
newdata = data.frame(word_list_data_study))
fitted_values <- fitted(model1_1)
head(fitted_values)
## plot expected predictions against actual response
dat <- as.data.frame(cbind(Y = standata(model1_1)$Y, fitted_values))
ggplot(dat) + geom_point(aes(x = Estimate, y = Y))


colnames(fit) = c('fit', 'se', 'lwr', 'upr') # this may not work? 
df_plot = cbind(newdata, fitted_values)
df_plot

ggplot(df_plot, aes(x = corpus, y = fit)) +
  geom_violin(data=obs, aes(x=corpus, y=real), alpha=0.5, color="gray70", fill='gray95') +
  geom_jitter(data=obs, aes(x=corpus, y=real), alpha=0.3, position = position_jitter(width = 0.07)) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), position=position_dodge(), size=1, width=.5) +
  geom_point(shape=21, size=4, fill='red') +
  geom_hline(yintercept=50, linetype=2) +
  xlab("") +
  ylab('Probability of responding REAL, %') +
  ylim(0,100) +
  theme_bw () +
  theme(panel.grid = element_blank())




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

# checking convergence and fit
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

