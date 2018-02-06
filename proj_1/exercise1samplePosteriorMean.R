# Exercise C.3.b
setwd("/home/shomea/g/ginama/V2018/git/Tma4300-Kode/proj_1")
source("sampleP.R")

students <- 35
draws <- 100000

daysInSeason <- matrix(c(92, 92, 91, 90))
p <- matrix(c(92 / 365, 92 / 365, 91 / 365, 92 / 365))

means <- computePosteriorMeans(p, students, draws, daysInSeason)
means_sorted <- sort(means)
truehist(means_sorted[2500:97500])