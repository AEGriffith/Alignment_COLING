library(brms)
library(dplyr)
library(cmdstanr)
library(rstan)
rstan_options(auto_write = TRUE)


# load model
model <- readRDS("all_words_full.rds")

# load data
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


print(distinct(data.frame(word_list_data_study$term_group)))

## compute expected predictions
newdata1 <- subset(word_list_data_study, word_list_data_study$term_group %in% c("F2018_G6","F2019-1_G6"))
newdata15 <- subset(word_list_data_study, word_list_data_study$term_group %in% "F2018_G1")
newdata2 <- subset(word_list_data_study, word_list_data_study$term_group %in% c("S2019_G8", "S2019_G18","S2019_G25"))
newdata3 <- subset(word_list_data_study, word_list_data_study$term_group %in% c("F2018_G5", "F2018_G14","F2019-1_G9"))
newdata4 <- subset(word_list_data_study, word_list_data_study$term_group %in% c("F2018_G15", "F2018_G4", "F2018_G9"))
newdata5 <- subset(word_list_data_study, word_list_data_study$term_group %in% c("S2019_G43", "F2019-1_G4","F2019-1_G1"))
newdata6 <- subset(word_list_data_study, word_list_data_study$term_group %in% c("F2018_G22", "F2018_G10","S2019_G41"))
newdata7 <- subset(word_list_data_study, word_list_data_study$term_group %in% c("F2019-1_G10", "S2019_G2","S2019_G19"))
newdata8 <- subset(word_list_data_study, word_list_data_study$term_group %in% c("S2019_G4", "S2019_G29","F2019-1_G5", "F2019-1_G7"))
word_list_data = NULL
word_list_data_study = NULL
gc()
fit1 <- fitted(model, newdata = newdata1, allow_new_levels = TRUE)
saveRDS(fit1, "fit1.rds")
rm(fit1)
gc()
fit15 <- fitted(model, newdata = newdata15, allow_new_levels = TRUE)
saveRDS(fit15, "fit15.rds")
rm(fit15)
gc()
fit2 <- fitted(model, newdata = newdata2, allow_new_levels = TRUE)
saveRDS(fit2, "fit2.rds")
rm(fit2)
gc()
fit3<- fitted(model, newdata = newdata3, allow_new_levels = TRUE)
saveRDS(fit3, "fit3.rds")
rm(fit3)
gc()
fit4 <- fitted(model, newdata = newdata4, allow_new_levels = TRUE)
saveRDS(fit4, "fit4.rds")
rm(fit4)
gc()
fit5 <- fitted(model, newdata = newdata5, allow_new_levels = TRUE)
saveRDS(fit5, "fit5.rds")
rm(fit5)
gc()
fit6 <- fitted(model, newdata = newdata6, allow_new_levels = TRUE)
saveRDS(fit6, "fit6.rds")
rm(fit6)
gc()
fit7 <- fitted(model, newdata = newdata7, allow_new_levels = TRUE)
saveRDS(fit7, "fit7.rds")
rm(fit7)
gc()
fit8 <- fitted(model, newdata = newdata8, allow_new_levels = TRUE)
saveRDS(fit8, "fit8.rds")
rm(fit8)
gc()
fit1= readRDS("fit1.rds")
fit15 = readRDS("fit15.rds")
fit2 = readRDS("fit2.rds")
fit3 = readRDS("fit3.rds")
fit4 = readRDS("fit4.rds")
fit5 = readRDS("fit5.rds")
fit6 = readRDS("fit6.rds")
fit7 = readRDS("fit7.rds")
fit8 = readRDS("fit8.rds")
fitted_values = rbind(fit1, fit15, fit2, fit3, fit4, fit5, fit6, fit7, fit8)
head(fitted_values)
saveRDS(fitted_values, "fitted_values.rds")
fitted_values <- readRDS("fitted_values.rds")
summary(fitted_values)

summary(model1_1)
## plot expected predictions against actual response
dat <- as.data.frame(cbind(Y = standata(model)$Y, fitted_values))
ggplot(dat) + geom_point(aes(x = word_list_data_study$count_target, y = Estimate))