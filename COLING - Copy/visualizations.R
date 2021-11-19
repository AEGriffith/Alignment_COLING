

# model filename
file = "model2_1.rds"

# load model
model<- readRDS(file)

# model summary
summary(model)

plot(model, ask = FALSE)
pp = brms::pp_check(model, ntrys = 35, type = 'ecdf_overlay')
pp + theme_bw()
fitted_values <- fitted(model)
dat <- as.data.frame(cbind(Y = standata(model)$Y, fitted_values))
fit <- data.frame(fitted_values)
ggplot(dat) + geom_point(aes(x = word_list_data_study$count_target, y = fit$Estimate))


colnames(fit) = c('fit', 'se', 'lwr', 'upr') # this may not work?
newdata = data.frame(word_list_data_study)
df_plot = cbind(newdata, fit)
df_plot

ggplot(df_plot, aes(x = fit, y = count_target)) +
  geom_violin(data=df_plot, aes(x=fit, y=count_target), alpha=0.5, color="gray70", fill='gray95') +
  geom_jitter(data=df_plot, aes(x=fit, y=count_target), alpha=0.3, position = position_jitter(width = 0.07)) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), position=position_dodge(), size=1, width=.5) +
  geom_point(shape=21, size=4, fill='red') +
  geom_hline(yintercept=50, linetype=2) +
  xlab("") +
  ylab('Probability of responding REAL, %') +
  # ylim(0,) +
  theme_bw () +
  theme(panel.grid = element_blank())

