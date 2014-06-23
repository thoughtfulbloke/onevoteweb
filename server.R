#onevoteweb is the web app of One Vot, Two Vote, Red Vote, Blue Vote https://github.com/thoughtfulbloke/OneVoteTwoVoteRedVoteBlueVote

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

Party           Allies  Electorate  Support
ACT             n        1              0
Conservative    n        0              0
Green           l        0              0
Labour          l       22              0
Mana            l        1              0
Maori           n        3              0
National        n       42              0
NZ_First        u        0              0
United_Future   n        1              0
")
  
sample_size <- polldata$sample[1]
party["National","Support"] <- polldata$national_support[1]
party["Labour","Support"] <- polldata$labour_support[1]
party["Green","Support"] <- polldata$green_support[1]
party["NZ_First","Support"] <- polldata$nzfirst_support[1]
party["Maori","Support"] <- polldata$maori_support[1]
party["Mana_Internet","Support"] <- polldata$manainternet_support[1]
party["ACT","Support"] <- polldata$act_support[1]
party["United_Future","Support"] <- polldata$united_support[1]
party["Conservative","Support"] <- polldata$conservative_support[1]

party["National","Electorate"] <- polldata$national_electorate[1]
party["Labour","Electorate"] <- polldata$labour_electorate[1]
party["Green","Electorate"] <- polldata$green_electorate[1]
party["NZ_First","Electorate"] <- polldata$nzfirst_electorate[1]
party["Maori","Electorate"] <- polldata$maori_electorate[1]
party["Mana_Internet","Electorate"] <- polldata$manainternet_electorate[1]
party["ACT","Electorate"] <- polldata$act_electorate[1]
party["United_Future","Electorate"] <- polldata$united_electorate[1]
party["Conservative","Electorate"] <- polldata$conservative_electorate[1]

party["National","Allies"] <- polldata$national_allies[[1]]
party["Labour","Allies"] <- polldata$labour_allies[1]
party["Green","Allies"] <- polldata$green_allies[1]
party["NZ_First","Allies"] <- polldata$nzfirst_allies[1]
party["Maori","Allies"] <- polldata$maori_allies[1]
party["Mana_Internet","Allies"] <- polldata$manainternet_allies[1]
party["ACT","Allies"] <- polldata$act_allies[1]
party["United_Future","Allies"] <- polldata$united_allies[1]
party["Conservative","Allies"] <- polldata$conservative_allies[1]
  
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
    nseats <- sum(seats[party$Allies == "n"])
    lseats <- sum(seats[party$Allies == "l"])
    wseats <- sum(seats[party$Allies == "u"])
    
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

prepdata <- function(polldata){
  party <- read.table(
    header = TRUE,
    row.names = "Party",
    stringsAsFactors = FALSE,
    fill = TRUE,
    comment = "",
    text = "

Party           Colour   Allies  Electorate  Support
ACT             #FFCB05  n        1              0
Conservative    #00AEEF  n        0              0
Green           #098137  l        0              0
Labour          #FF0000  l       22              0
Mana_Internet   #770808  l        1              0
Maori           #EF4A42  n        3              0
National        #00529F  n       42              0
NZ_First        #000000  u        0              0
United_Future   #501557  n        1              0
")

party["National","Support"] <- polldata$national_support[1]
party["Labour","Support"] <- polldata$labour_support[1]
party["Green","Support"] <- polldata$green_support[1]
party["NZ_First","Support"] <- polldata$nzfirst_support[1]
party["Maori","Support"] <- polldata$maori_support[1]
party["Mana_Internet","Support"] <- polldata$manainternet_support[1]
party["ACT","Support"] <- polldata$act_support[1]
party["United_Future","Support"] <- polldata$united_support[1]
party["Conservative","Support"] <- polldata$conservative_support[1]

party["National","Electorate"] <- polldata$national_electorate[1]
party["Labour","Electorate"] <- polldata$labour_electorate[1]
party["Green","Electorate"] <- polldata$green_electorate[1]
party["NZ_First","Electorate"] <- polldata$nzfirst_electorate[1]
party["Maori","Electorate"] <- polldata$maori_electorate[1]
party["Mana_Internet","Electorate"] <- polldata$manainternet_electorate[1]
party["ACT","Electorate"] <- polldata$act_electorate[1]
party["United_Future","Electorate"] <- polldata$united_electorate[1]
party["Conservative","Electorate"] <- polldata$conservative_electorate[1]

party["National","Allies"] <- polldata$national_allies[[1]]
party["Labour","Allies"] <- polldata$labour_allies[1]
party["Green","Allies"] <- polldata$green_allies[1]
party["NZ_First","Allies"] <- polldata$nzfirst_allies[1]
party["Maori","Allies"] <- polldata$maori_allies[1]
party["Mana_Internet","Allies"] <- polldata$manainternet_allies[1]
party["ACT","Allies"] <- polldata$act_allies[1]
party["United_Future","Allies"] <- polldata$united_allies[1]
party["Conservative","Allies"] <- polldata$conservative_allies[1]

return(party)
}

shinyServer(
  function(input, output) {
    output$outplot <- renderPlot({
      outcomes <- simulated_election(input)
      freq <- 100 * outcomes$count / sum (outcomes$count)
      axis_text <- c( "National led\nw/o Undecideds", "Labour led\nw/o Undecideds", "Hung Parliament", "Up to Undecideds")
      colourscheme <- c("#0000FFFF","#FF0000FF","#FF00FFFF", "#000000FF")
      barplot(freq, names.arg=axis_text, main="If the poll is accurate,\nthe election would be", cex.names=0.9, col=colourscheme, ylab="Results Percentage")
    })
    output$inplot <- renderPlot({
      parties <- prepdata(input)
      barplot(parties$Support, names.arg=row.names(parties), main="Individual results", cex.names=0.9, col=parties$Colour, ylab="Support")
    })
  }
)
