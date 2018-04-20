attach("environment.RData")
mat <- read.csv("data/EPL1516.csv", header = TRUE)
pageWithSidebar(
  headerPanel('English Premier League 2015-16'),
  sidebarPanel(
    selectInput("xcol", "X Variable", names(Xh)),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9),
    selectInput("var", "Plot Outcome", names(Xh))
  ),
  mainPanel(
    h4("Cluster plot"),
    plotOutput('plot1'),
    h4("Outcome versus input variable"),
    plotOutput('plot2'),
    h4("Outcome frequency"),
    plotOutput('plot3'),
    h4("Total Shots Ratio home team summary"),
    verbatimTextOutput("summary1"),
    h4("Total Shots Ratio away team summary"),
    verbatimTextOutput("summary2"),
    h4("Total Shots Ratio home team table"),
    tableOutput("view1"),
    h4("Total Shots Ratio away team table"),
    tableOutput("view2"),
    h4("Principal component analysis home team"),
    plotOutput('plot4'),
    h4("Principal component analysis away team"),
    plotOutput('plot5'),
    h4("Predictive models summary"),
    verbatimTextOutput("rezultat"),
    h4("Box and whisker plots to compare models"),
    plotOutput('rez1'),
    h4("Density plots of accuracy"),
    plotOutput('rez2'),
    h4("Dot plots of accuracy"),
    plotOutput('rez3'),
    h4("Pair-wise scatterplots of predictions to compare models"),
    plotOutput('rez4'),
    h4("xyplot plots to compare models"),
    plotOutput('rez5'),
    h4("Summarize p-values for pair-wise comparisons"),
    verbatimTextOutput("rez6")
  )
)