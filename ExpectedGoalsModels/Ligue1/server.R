library(formattable)

function(input, output, session) {
  
  #The part where the ratings of the teams playing this current season are computed
  
  t <- c("Amiens", "Angers SCO", "Bordeaux", "Caen", "Dijon", "Guingamp", "Lille",
         "Olympique Lyonnais", "Olympique Marseille", "Metz", "AS Monaco", "Montpellier", "Nantes", "Nice",
         "PSG", "Rennes", "Saint-Etienne", "Strasbourg", "Toulouse", "Troyes")
  team <- sort(t)
  mat <- read.csv("data/Ligue110.csv")
  #mat <- Ligue110
  home <- mat[,1:3]
  away <- mat[,c(1:2,4)]
  ha <- aggregate(home$home_score, by=list(home$home_team_id), FUN=mean)
  hd <- aggregate(home$away_score, by=list(home$home_team_id), FUN=mean)
  aa <- aggregate(away$away_score, by=list(away$away_team_id), FUN=mean)
  ad <- aggregate(away$home_score, by=list(away$away_team_id), FUN=mean)
  ham <- ha[-c(1, 3, 5, 6, 8, 9, 12:14, 16:18, 20, 23, 29, 32, 36),]
  hdm <- hd[-c(1, 3, 5, 6, 8, 9, 12:14, 16:18, 20, 23, 29, 32, 36),]
  aam <- aa[-c(1, 3, 5, 6, 8, 9, 12:14, 16:18, 20, 23, 29, 32, 36),]
  adm <- ad[-c(1, 3, 5, 6, 8, 9, 12:14, 16:18, 20, 23, 29, 32, 36),]
  arating <- (ha$x+aa$x)/2
  drating <- (hd$x+ad$x)/2
  aratm <- (arating[1]+arating[3]+arating[5]+arating[6]+arating[8]+arating[12]+arating[9]+arating[16]+arating[36])/9
  dratm <- (drating[1]+drating[3]+drating[5]+drating[6]+drating[8]+drating[12]+drating[9]+drating[16]+drating[36])/9
  aratingm <- (ham$x+aam$x)/2
  dratingm <- (hdm$x+adm$x)/2
  aratingnew <- c(aratm,aratingm[1:length(aratingm)])
  dratingnew <- c(dratm,dratingm[1:length(dratingm)])
  offm <- mean(aratingnew)
  defm <- mean(dratingnew)
  hadv <- sum(home[,1])/sum(home[,2])
  homev <- (offm+defm)*hadv/(1+hadv)
  awayv <- (offm+defm)*1/(1+hadv)
  
  #The function that obtains probabilities for specific outcomes using the Poisson distribution
  
  poison <- function() {
    g <- matrix(c(0),nrow = 11, ncol = 11)
    h.name <- input$X
    a.name <- input$Y
    pos1 <- which(team == h.name, arr.ind=TRUE)
    pos2 <- which(team == a.name, arr.ind=TRUE)
    xG.h <- homev*(aratingnew[pos1]/offm)*(dratingnew[pos2]/defm)
    xG.a <- awayv*(aratingnew[pos2]/offm)*(dratingnew[pos1]/defm)
    for(i in 1:11)  {
      for(j in 1:11) 
        g[i,j] <- dpois(i-1, lambda = xG.h, log = FALSE) * dpois(j-1, lambda = xG.a, log = FALSE) * 100
    }
    g <- t(g)
    return(g)
  }
  
  #Descriptive statistics of the League
  
  output$summary1 <- renderPrint({
    summary(aratingnew)
  })
  
  output$summary2 <- renderPrint({
    summary(dratingnew)
  })
  
  output$view1 <- renderTable({
    matr <- c(offm, defm, hadv, homev, awayv)
    matri <- matrix(matr, nrow = 5)
    rownames(matri) <- c("Offrating mean", "Defrating mean", "Home advantage", "Home value", "Away value")
    t(format(matri, digits = 4))
  })
  
  #Expected Goals Model
  
  output$view2 <- renderTable({
    h.name <- input$X
    a.name <- input$Y
    pos1 <- which(team == h.name, arr.ind=TRUE)
    pos2 <- which(team == a.name, arr.ind=TRUE)
    xG.h <- homev*(aratingnew[pos1]/offm)*(dratingnew[pos2]/defm)
    xG.a <- awayv*(aratingnew[pos2]/offm)*(dratingnew[pos1]/defm)
    sup <- xG.h - xG.a
    total <- xG.h + xG.a
    matr <- c(xG.h, xG.a, sup, total)
    matri <- matrix(matr, nrow = 4)
    rownames(matri) <- c("Home goals", "Away goals", "Sup", "Total")
    t(format(matri, digits = 3))
  })
  
  #Table of probabilities for all possible outcomes
  
  output$view3 <- renderTable({
    g <- poison()
    rownames(g) <- c("0","1","2","3","4","5","6","7","8","9","10")
    colnames(g) <- c("0","1","2","3","4","5","6","7","8","9","10")
    g
  },
  include.rownames = TRUE)
  
  #1x2 Betting, probability and odds
  
  output$summary3 <- renderPrint({
    g <- poison()
    homewin <- sum(g[upper.tri(g)])
    awaywin <- sum(g[lower.tri(g)])
    drawp <- sum(diag(g))
    oddsability <- c(homewin, awaywin, drawp)
    oddh <- 1/homewin * 100
    odda <- 1/awaywin * 100
    oddd <- 1/drawp * 100
    odds <- c(oddh, odda, oddd)
    mat <- t(rbind(oddsability,odds))
    mat <- formattable(mat, digits = 2, format = "f")
    rownames(mat) <- c("HW  ","AW  ","Dr  ")
    mat
  })
  
  #Over/Under betting, only odds
  
  output$summary4<- renderPrint({
    g <- poison()
    odds <- matrix(c(0),nrow = 2, ncol = 12)
    p05 <- g[1,1]
    odds[2,1] <- 1/p05 * 100 
    odds[1,1] <- 1/(100-p05) * 100 
    p1 <- g[1,1]
    draw1 <- g[1,2]+ g[2,1]
    s1 <- 100 - draw1
    p1 <- p1/s1*100
    odds[2,2] <- 1/p1*100
    odds[1,2] <- 1/(100-p1)*100
    p15 <- g[1,1] + g[2,1] + g[1,2]
    odds[2,4] <- 1/p15 * 100 
    odds[1,4] <- 1/(100-p15) * 100 
    odds[2,3] <- odds[2,2]/2 + odds[2,4]/2
    odds[1,3] <- odds[1,2]/2 + odds[1,4]/2
    p2 <- g[1,1] + g[2,1] + g[1,2]
    draw2 <- g[1,3] + g[3,1] + g[2,2]
    s2 <- 100-draw2
    p2 <- p2/s2*100
    odds[2,6] <- 1/p2 * 100 
    odds[1,6] <- 1/(100-p2) * 100
    odds[2,5] <- odds[2,4]/2 + odds[2,6]/2
    odds[1,5] <- odds[1,4]/2 + odds[1,6]/2
    p25 <- g[1,1] + g[2,1] + g[1,2] + g[1,3] + g[3,1] + g[2,2]
    odds[2,8] <- 1/p25 * 100
    odds[1,8] <- 1/(100-p25) * 100
    odds[2,7] <- odds[2,6]/2 + odds[2,8]/2
    odds[1,7] <- odds[1,6]/2 + odds[1,8]/2
    p3 <- g[1,1] + g[2,1] + g[1,2] + g[1,3] + g[3,1] + g[2,2]
    draw3 <- g[1,4] + g[4,1] + g[2,3] + g[3,2]
    s3 <- 100-draw3
    p3 <- p3/s3*100
    odds[2,10] <- 1/p3 * 100
    odds[1,10] <- 1/(100-p3) * 100
    odds[2,9] <- odds[2,8]/2 + odds[2,10]/2
    odds[1,9] <- odds[1,8]/2 + odds[1,10]/2
    p35 <- g[1,1] + g[2,1] + g[1,2] + g[1,3] + g[3,1] + g[2,2] + g[1,4] + g[4,1] + g[2,3] + g[3,2]
    odds[2,11] <- 1/p35 * 100
    odds[1,11] <- 1/(100-p35) * 100
    p45 <- g[1,1] + g[2,1] + g[1,2] + g[1,3] + g[3,1] + g[2,2] + g[1,4] + g[4,1] + g[2,3] + g[3,2] + g[5,1] + g[1,5] + g[3,3] + g[2,4] + g[4,2]
    odds[2,12] <- 1/p45 * 100
    odds[1,12] <- 1/(100-p45) * 100
    odds <- formattable(odds, digits = 2, format = "f")
    rownames(odds) <- c("Over   ", "Under   ")
    colnames(odds) <- c("0.5 goals  ","1 goal  ","1.25 goals  ","1.5 goals  ","1.75 goals  ","2 goals  ","2.25 goals  ","2.5 goals  ","2.75 goals  ","3 goals  ","3.5 goals  ","4.5 goals")
    odds
  })
  
  #Asian Handicaps odds
  
  output$summary5<- renderPrint({
    g <- poison()
    asian_1 <- 0
    for(i in 1:9) {
      for(j in (i+2):11) {
        asian_1 <- asian_1 + g[i,j]
      }}
    draw <- 0
    for(i in 1:10) {
      draw <- draw + g[i,i+1]
    }
    lose <- 100 - asian_1 - draw
    as_1 <- asian_1*100/(asian_1+lose)
    lo <- lose*100/(lose+asian_1)
    matr <- c(as_1, lo)
    matr <- matrix(matr, ncol = 2)
    matr <- formattable(matr, digits = 2, format = "f")
    colnames(matr) <- c("AH -1 Win", "Lose")
    rownames(matr) <- "odds"
    matr
  })
  
  output$summary6<- renderPrint({
    g <- poison()
    asian_075 <- 0
    for(i in 1:9) {
      for(j in (i+2):11) {
        asian_075 <- asian_075 + g[i,j]
      }}
    halfwin <- 0
    for(i in 1:10) {
      halfwin <- halfwin + g[i,i+1]
    }
    lose <- sum(g[lower.tri(g, diag = TRUE)])
    asian_075 <- asian_075+halfwin/2
    as_075 <- asian_075*100/(asian_075+lose)
    lo <- lose*100/(lose+asian_075)
    asian <- c(as_075, lo)
    matr <- matrix(asian, ncol = 2)
    matr <- formattable(matr, digits = 2, format = "f")
    colnames(matr) <- c("AH -0.75 Win", "Lose")
    rownames(matr) <- "odds"
    matr
  })
  
  output$summary7<- renderPrint({
    g <- poison()
    asian_05 <- sum(g[upper.tri(g)])
    asian <- c(asian_05, 100-asian_05)
    matr <- matrix(asian, ncol = 2)
    matr <- formattable(matr, digits = 2, format = "f")
    colnames(matr) <- c("AH -0.5 Win", "Lose")
    rownames(matr) <- "odds"
    matr
  })
  
  output$summary8<- renderPrint({
    g <- poison()
    asian_025 <- sum(g[upper.tri(g)])
    draw <- sum(diag(g))
    lose <- 100 - asian_025 - draw/2
    as_025 <- asian_025*100/(asian_025+lose)
    lo <- lose*100/(lose+asian_025)
    matr <- c(as_025, lo)
    matr <- matrix(matr, ncol = 2)
    matr <- formattable(matr, digits = 2, format = "f")
    colnames(matr) <- c("AH -0.25 Win", "Lose")
    rownames(matr) <- "odds"
    matr
  })
  
  output$summary9<- renderPrint({
    g <- poison()
    asian00 <- sum(g[upper.tri(g)])
    draw <- sum(diag(g))
    lose <- 100 - asian00 - draw
    as00 <- asian00*100/(asian00+lose)
    lo <- lose*100/(lose+asian00)
    matr <- c(as00, lo)
    matr <- matrix(matr, ncol = 2)
    matr <- formattable(matr, digits = 2, format = "f")
    colnames(matr) <- c("AH 0.0 Win", "Lose")
    rownames(matr) <- "odds"
    matr
  })
  
  output$summary10<- renderPrint({
    g <- poison()
    asian025 <- sum(g[upper.tri(g)])
    draw <- sum(diag(g))
    lose <- 100 - asian025 - draw
    asian025 <- asian025+draw/2
    as025 <- asian025*100/(asian025+lose)
    lo <- lose*100/(lose+asian025)
    matr <- c(as025, lo)
    matr <- matrix(matr, ncol = 2)
    matr <- formattable(matr, digits = 2, format = "f")
    colnames(matr) <- c("AH 0.25 Win", "Lose")
    rownames(matr) <- "odds"
    matr
  })
  
  output$summary11<- renderPrint({
    g <- poison()
    asian05 <- sum(g[upper.tri(g, diag = TRUE)])
    asian <- c(asian05, 100-asian05)
    matr <- matrix(asian, ncol = 2)
    matr <- formattable(matr, digits = 2, format = "f")
    colnames(matr) <- c("AH 0.5 Win", "Lose")
    rownames(matr) <- "odds"
    matr
  })
  
  output$summary12<- renderPrint({
    g <- poison()
    asian075 <- sum(g[upper.tri(g, diag = TRUE)])
    halflose <- 0
    for(i in 1:10) {
      halflose <- halflose + g[i+1,i]
    }
    lose <- 0
    for(i in 1:9) {
      for(j in (i+2):11) {
        lose <- lose + g[j,i]
      }}
    lose <- lose+halflose/2
    as075 <- asian075*100/(asian075+lose)
    lo <- lose*100/(lose+asian075)
    asian <- c(as075, lo)
    matr <- matrix(asian, ncol = 2)
    matr <- formattable(matr, digits = 2, format = "f")
    colnames(matr) <- c("AH 0.75 Win", "Lose")
    rownames(matr) <- "odds"
    matr
  })
  
  output$summary13<- renderPrint({
    g <- poison()
    asian1 <- sum(g[upper.tri(g, diag = TRUE)])
    draw <- 0
    for(i in 2:11) {
      draw <- draw + g[i,i-1]
    }
    lose <- 100 - asian1 - draw
    as1 <- asian1*100/(asian1+lose)
    lo <- lose*100/(asian1+lose)
    matr <- c(as1, lo)
    matr <- matrix(matr, ncol = 2)
    matr <- formattable(matr, digits = 2, format = "f")
    colnames(matr) <- c("AH +1 Win", "Lose")
    rownames(matr) <- "odds"
    matr
  })
}


































