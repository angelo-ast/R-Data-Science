attach("environment.RData")
pageWithSidebar(
  headerPanel('Expected Goals Model for La Liga'),
  sidebarPanel(
    selectInput("X", "Home Team", team),
    selectInput("Y", "Away Team", team)
  ),
  mainPanel(
    h4("Offensive rating summary"),
    verbatimTextOutput("summary1"),
    h4("Defensive rating summary"),
    verbatimTextOutput("summary2"),
    h4("League statistics"),
    tableOutput("view1"),
    h4("Expected Goals Model"),
    tableOutput("view2"),
    h4("Probability of outcomes by Poisson distribution (percentages %)"),
    tableOutput("view3"),
    h4("Probability of home win, away win, draw and associated betting odds"),
    verbatimTextOutput("summary3"),
    h4("Probability (betting odds) of over/under goals scored"),
    verbatimTextOutput("summary4"),
    h4("Probability of home asian handicap -1.0 in percentages"),
    verbatimTextOutput("summary5"),
    h4("Probability of home asian handicap -0.75 in percentages"),
    verbatimTextOutput("summary6"),
    h4("Probability of home asian handicap -0.5 in percentages"),
    verbatimTextOutput("summary7"),
    h4("Probability of home asian handicap -0.25 in percentages"),
    verbatimTextOutput("summary8"),
    h4("Probability of home asian handicap 0.0 in percentages"),
    verbatimTextOutput("summary9"),
    h4("Probability of home asian handicap +0.25 in percentages"),
    verbatimTextOutput("summary10"),
    h4("Probability of home asian handicap +0.5 in percentages"),
    verbatimTextOutput("summary11"),
    h4("Probability of home asian handicap +0.75 in percentages"),
    verbatimTextOutput("summary12"),
    h4("Probability of home asian handicap +1.0 in percentages"),
    verbatimTextOutput("summary13")
  )
)