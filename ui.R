library(shiny)


shinyUI(fluidPage(
  # Application title
  titlePanel("One Vote, Two Vote. Red Vote, Blue Vote. A New Zealand MMP simulator"),
    
  sidebarLayout(
    sidebarPanel(
      h3('Enter party support percentage:'),
      p('defaults: Roy Morgan Apr 21'),
      numericInput('national', 'National', 42.5, min = 0, max = 100, step = 0.5),
      numericInput('labour', 'Labour', 31, min = 0, max = 100, step = 0.5),
      numericInput('green', 'Green Party', 14.5, min = 0, max = 100, step = 0.5),
      numericInput('nzfirst', 'NZ First', 6, min = 0, max = 100, step = 0.5),
      numericInput('maori', 'Maori Party', 1, min = 0, max = 100, step = 0.5),
      numericInput('mana', 'Mana Party', 1, min = 0, max = 100, step = 0.5),
      numericInput('act', 'ACT NZ', 0.5, min = 0, max = 100, step = 0.5),
      numericInput('united', 'United Future', 0.5, min = 0, max = 100, step = 0.5),
      numericInput('conservative', 'Conservative', 0.5, min = 0, max = 100, step = 0.5),
      numericInput('internet', 'Internet Party', 1.5, min = 0, max = 100, step = 0.5),
      numericInput('sample', 'Sample Size', 787, min = 0, max = 100, step = 1),
      submitButton('See Outcomes')
    ),
    mainPanel(
      h4('The poll on the left gives the election result:'),
      plotOutput("distPlot")
    )
    )
  )
)