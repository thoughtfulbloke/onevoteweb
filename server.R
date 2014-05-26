library(shiny)
require(gtools)

simulated_election <- function(polldata){
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
  
  result = c("national_led", "labour_led", "hung", "nzf_decides")
  count = c(0,0,0,0)
  
  outcomes <- data.frame(result, count)
  row.names(outcomes) <- outcomes$result
  
  many_elections <- 1000
  turnout = 2000000
  for (i in 1:many_elections)
  {
    # if P is the true proportions, then the poll is multinomial(num_polled, P)
    # if we assume a Dirichlet(alpha) prior, the posterior is also dirichlet
    # due to conjugacy, with paramater alpha + votes_in_poll.
    votes_in_poll <- round(party$Support * sample_size) # the best we can do as we don't know the weighting
    prior <- rep(1, length(votes_in_poll))    # equivalent to adding a vote for each party
    prop <- rdirichlet(1, votes_in_poll + prior)
    votes <- (round(turnout * prop))
    
    total_seats <- 120
    
    # exclude parties that don't make the threshold
    exclude <- votes / sum(votes) < 0.05 & !party$Electorate
    votes[exclude] <- 0;
    
    # figure out total number of votes via Sainte Laguë
    divisors <- seq(1, by=2, length.out=total_seats)
    divisors_rep <- rep(divisors, length.out=(length(divisors) * length(votes)))
    divisors_rep <- sort(divisors_rep)
    votes_rep <- rep(votes, length.out=(length(divisors) * length(votes)))
    r <- rep(1:length(votes), length(divisors))
    o <- order(-round(votes_rep / divisors_rep))
    seats <- rep(0, length(votes))
    t <- tabulate(r[o[1:total_seats]])
    seats[1:length(t)] <- t
    seats <- (pmax(seats, party$Electorate))
    
    nseats <- sum(seats[party$Side == "n"])
    lseats <- sum(seats[party$Side == "l"])
    wseats <- sum(seats[party$Side == "w"])
    
    if (nseats > (lseats + wseats))  {
      outcomes["national_led","count"] <- outcomes["national_led","count"] + 1
    } else if (lseats > (nseats + wseats))  {
      victory <- "labour_led"
      outcomes["labour_led","count"] <- outcomes["labour_led","count"] + 1
    }  else if ((lseats == nseats) & (wseats == 0)){
      victory <- "hung"
      outcomes["hung","count"] <- outcomes["hung","count"] + 1
    } else {outcomes["nzf_decides","count"] <- outcomes["nzf_decides","count"] + 1}
    
  }
  return(outcomes)
}


shinyServer(
  function(input, output) {
    output$distPlot <- renderPlot({
      outcomes <- simulated_election(input)
      freq <- 100 * outcomes$count / sum (outcomes$count)
      axis_text <- c( "National led\nw/o NZF", "Labour led\nw/o NZF", "Hung Parliament", "Up to NZF")
      colourscheme <- c("#0000FFFF","#FF0000FF","#FF00FFFF", "#000000FF")
      barplot(freq, names.arg=axis_text, main="If the poll is accurate,\nthe election would be", cex.names=0.6, col=colourscheme, ylab="Results Percentage")
    })
  }
)
