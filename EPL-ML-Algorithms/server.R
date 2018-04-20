library(shiny)
library(Hmisc)
library(ggplot2)
library(caret)
library(doParallel)
library(randomForest)
library(rpart)
library(nnet)
library(kernlab)
library(klaR)

function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  mat <- read.csv("data/EPL1516.csv", header = TRUE)
  mat <- EPL1516
  y <- factor(mat$Result, labels = c("W", "L", "D"))
  Xf <- mat[, 3:ncol(mat)]
  Xh <- Xf[, 7:25]
  selectedData <- reactive({
    Xf[, c(input$xcol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  output$plot2 <- renderPlot({
    x.name <- input$var
    x <- Xf[, names(Xf) %in% x.name]
    pf <- prop.table(table(t(x), y), 1)[, c(1, 3, 2)]
    dtst <- data.frame(PctFreq = c(t(pf)), x = rep(as.numeric(rownames(pf)), 
                                                   each = ncol(pf)), Outcome = ordered(rep(1:3, nrow(pf)), labels = colnames(pf)))
    
    ggplot(dtst, aes(x = x, 
                     y = PctFreq, 
                     group = Outcome, 
                     fill = Outcome
            )) + 
      geom_area(position = "fill") + scale_x_continuous(x.name) + scale_y_continuous("Percentage frequency") + 
      scale_fill_manual(values = c("green", "blue", "red"))
  })
  
  output$plot3 <- renderPlot({
    AbsFreq <- table(y)
    PerFreq <- round(prop.table(AbsFreq) * 100, 1)
    Freq <- data.frame(PerFreq)
    ggplot(Freq, aes(x = "", fill = y, weight = Freq)) + geom_bar(width = 1) + 
      scale_fill_manual(values = c("green", "red", "blue")) + scale_y_continuous("Percentage frequency") + 
      scale_x_discrete(name = "")
  })
  
  output$summary1 <- renderPrint({
    TSRh=(Xf[,15]+Xf[,16])/(Xf[,15]+Xf[,16]+Xf[,34]+Xf[,35])
    summary(TSRh)
  })
  
  output$summary2 <- renderPrint({
    TSRa=(Xf[,35]+Xf[,34])/(Xf[,16]+Xf[,15]+Xf[,35]+Xf[,34])
    summary(TSRa)
  })
  
  output$view1 <- renderTable({
    m <- cbind.data.frame(Xf[,4],Xf[,5],TSRh, TSRa)
    mh <- aggregate(m[,3], by=list(m[,1]), FUN=mean)
    mh[order(mh[,2], decreasing = TRUE),]
  })
  
  output$view2 <- renderTable({
    m <- cbind.data.frame(Xf[,4],Xf[,5],TSRh, TSRa)
    ma <- aggregate(m[,4], by=list(m[,2]), FUN=mean)
    ma[order(ma[,2], decreasing = TRUE),]
  })
  
  output$plot4 <- renderPlot({
    Xh <- Xf[, 7:25]
    homePCA <- prcomp(Xh)
    plot(homePCA, type = "l")
  })
  
  output$plot5 <- renderPlot({
    Xa <- Xf[26:44]
    awayPCA <- prcomp(Xa)
    plot(awayPCA, type = "l")
  })
  
  output$rezultat <- renderPrint({
    Xc <- cbind(Xh[, 1:3], Xh[, 10], Xh[, 19], Xa[, 1:3], Xa[, 10], Xa[, 19], TSRh)
    dtset.ind <- data.frame(Xc, y)
    set.seed(987)
    idx <- sample(1:nrow(dtset.ind), 80)
    learn <- dtset.ind[-idx, ]
    test <- dtset.ind[idx, ]
    # Recursive Partitioning and Regression Trees
    set.seed(7)
    fit.cart <- train(y ~ ., data = learn, method = "rpart", trControl = control)
    y.rpart <- predict(fit.cart, newdata = test[, 1:11], type = "prob")
    # Linear Discriminant Analysis
    set.seed(7)
    fit.lda <- train(y ~ ., data = learn, method = "lda", trControl = control)
    y.lda <- predict(fit.lda, newdata = test[, 1:11], type = "prob")
    # Support Vector Machine
    set.seed(7)
    fit.svm <- train(y ~ ., data = learn, method = "svmRadial", trControl = control)
    y.svm <- predict(fit.svm, newdata = test[, 1:11], type = "prob")
    # k-Nearest Neighbours
    clus <- makeCluster(spec = 6, type = "PSOCK")
    registerDoParallel(clus)
    set.seed(7)
    fit.knn <- train(y ~ ., data = learn, method = "knn", trControl = control)
    stopCluster(clus)
    yhat.knn <- predict(fit.knn, newdata = test[, 1:11], type = "prob")
    # Random forest
    clus <- makeCluster(spec = 6, type = "PSOCK")
    registerDoParallel(clus)
    control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    set.seed(7)
    fit.rf <- train(y ~ ., data = learn, method = "rf", trControl = control)
    stopCluster(clus)
    y.rf <- predict(fit.rf, newdata = test[, 1:11], type = "prob")
    # Neural Network
    clus <- makeCluster(spec = 6, type = "PSOCK")
    registerDoParallel(clus)
    set.seed(7)
    fit.nnet <- train(y ~ ., data = learn, method = "nnet", trControl = control)
    stopCluster(clus)
    y.nnet <- predict(fit.nnet, newdata = test[, 1:11])
    # Model Selection
    results <- resamples(list(RF = fit.rf, CART = fit.cart, NN=fit.nnet, SVM = fit.svm, 
                            KNN = fit.knn, LDA = fit.lda))
    summary(results)
  })
  
  output$rez1 <- renderPlot({
    scales <- list(x = list(relation = "free"), y = list(relation = "free"))
    bwplot(results, scales = scales)
  })
  
  output$rez2 <- renderPlot({
    scales <- list(x = list(relation = "free"), y = list(relation = "free"))
    densityplot(results, scales = scales, pch = "|")
  })
  
  output$rez3 <- renderPlot({
    scales <- list(x = list(relation = "free"), y = list(relation = "free"))
    dotplot(results, scales = scales)
  })
  
  output$rez4 <- renderPlot({
    splom(results)
  })
  
  output$rez5 <- renderPlot({
    xyplot(results, models = c("LDA", "SVM"))
  })

  output$rez6 <- renderPrint({
    diffs <- diff(results)
    summary(diffs)
  })
}