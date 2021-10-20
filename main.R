# Title     : TODO
# Objective : TODO
# Created by: amandagriffith
# Created on: 10/9/21

# library(lme4)
#load libraries
# library(tidyverse)
# library(anytime)
# library(jsonlite)
# library(DT)
# library(chron)
# library(lubridate)
# library(DBI)
#  library("plyr")
library(dplyr)
# library(RMariaDB)
# library(janitor)
# library(reticulate)
# library(rlist)
# library(gtools)
# library(plotly)
# library(ggfortify)
# library(survival)
# library(openxlsx)
# library(RColorBrewer)
# library(reshape2)
# library("readxl")
# library(ggplot2)

# make a fake data table
counts <- c(5, 4, 4, 5, 6, 6, 5, 5, 5, 6, 6, 4, 4, 4, 4, 6, 7, 5, 4, 5, 5, 5, 4, 6, 10, 5, 5, 4, 5, 4, 6, 5)
prime <- c(rep('A', 16), rep('B', 16))
task <- c(rep('1', 8), rep('2', 8), rep('1', 8), rep('2', 8))
subject <- paste0("S", 1:32)

data <- tibble(counts, prime, task, subject)

# install package
# install.packages('brms', dependencies = TRUE)
library(brms)

m3 <- brm(formula = counts ~ 1 + prime + task + (1 | subject), data = data, family = 'gaussian')
summary(m3)
