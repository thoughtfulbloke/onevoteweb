
election_from_poll <- function(poll, num_polled, turnout = 2000000) {
  
  # if P is the true proportions, then the poll is multinomial(num_polled, P)
  # if we assume a Dirichlet(alpha) prior, the posterior is also dirichlet
  # due to conjugacy, with paramater alpha + votes_in_poll.
  
  votes_in_poll <- round(poll * num_polled) # the best we can do as we don't know the weighting
  prior <- rep(1, length(votes_in_poll))    # equivalent to adding a vote for each party
  
  prop <- rdirichlet(1, votes_in_poll + prior)
  return(round(turnout * prop))
}

# allocate_seats(votes, electorates)
#
# Allocates seats in an MMP parliament using the Sainte Laguë
# system as employed in New Zealand.
#
# votes        the number of votes in the election per-party.
# electorates  the number of electorates won per-party.
#

allocate_seats <- function(votes, electorates) {
  total_seats <- 120
  
  # exclude parties that don't make the threshold
  exclude <- votes / sum(votes) < 0.05 & !electorates
  votes[exclude] <- 0;
  
  # figure out total number of votes via Sainte Laguë
  divisors <- seq(1, by=2, length.out=total_seats)
  
  r <- rep(1:length(votes), length(divisors))
  d <- expand.grid(votes, divisors)
  o <- order(-round(d[,1] / d[,2]))
  
  seats <- rep(0, length(votes))
  t <- tabulate(r[o[1:total_seats]])
  seats[1:length(t)] <- t
  return(pmax(seats, electorates))
}

decide_winner <- function(seats, sides) {
  # now decide party allegiance, assuming NZF is king-maker
  nseats <- sum(seats[sides == "n"])
  lseats <- sum(seats[sides == "l"])
  wseats <- sum(seats[sides == "w"])
  
  if (nseats > (lseats + wseats))  {
    victory <- "national_led"
  } else if (lseats > (nseats + wseats))  {
    victory <- "labour_led"
  }  else if ((lseats == nseats) & (wseats == 0)){
    victory <- "hung"
  } else { victory <- "nzf_decides"}
  return(victory)
}

simulated_election <- function(polldata){
    require(gtools)
  
  
  party <- read.table(
    header = TRUE,
    row.names = "Party",
    stringsAsFactors = FALSE,
    fill = TRUE,
    comment = "",
    text = "

Party           Side  Electorate  Support
ACT             n        1              0
Conservative    n        0              0
Green           l        0              0
Labour          l       22              0
Mana            l        1              0
Maori           n        3              0
National        n       42              0
NZ_First        w        0              0
United_Future   n        1              0
Internet        l        0              0
")


sample_size <- polldata$sample[1]
party["National","Support"] <- polldata$national[1]
party["Labour","Support"] <- polldata$labour[1]
party["Green","Support"] <- polldata$green[1]
party["NZ_First","Support"] <- polldata$nzfirst[1]
party["Maori","Support"] <- polldata$maori[1]
party["Mana","Support"] <- polldata$mana[1]
party["ACT","Support"] <- polldata$act[1]
party["United_Future","Support"] <- polldata$united[1]
party["Conservative","Support"] <- polldata$conservative[1]
party["Internet","Support"] <- polldata$internet[1]


many_elections <- 1000
outcomes <- rep("", many_elections)

for (i in 1:many_elections)
{
  
  
  votes <- election_from_poll(party$Support, sample_size)
  seats <- allocate_seats(votes, party$Electorate)
  outcomes[i] <- decide_winner(seats, party$Side)
}
print("Results for many elections")
print(prop.table(table(outcomes)))

}
