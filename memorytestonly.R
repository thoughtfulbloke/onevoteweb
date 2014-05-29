
national <- 42.5
labour <- 31
green <- 14.5
nzfirst <- 6
maori <- 1
mana <- 1
act <- 0.5
united <- 0.5
conservative <- 0.5
internet <- 1.5
sample <- 787
input <- data.frame(national, labour, green, nzfirst, maori, mana, act, united, conservative, internet, sample)

#library(lineprof)
#library(lineprof)
source("voters.R")
#prof <- lineprof(simulated_election(input))
#shine(prof)
simulated_election(input)


