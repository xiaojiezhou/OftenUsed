##################################################-
####http://topepo.github.io/caret/index.html
#################################################- 
rm(list=ls())
library(AppliedPredictiveModeling)
library(caret)
library(e1071)
library(ipred)



########Visualizations###########
str(iris)

featurePlot(x = iris[, 1:4],  # data matrix
            y = iris$Species, #indicator variable
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

featurePlot(x = iris[, 1:4],
            y = iris$Species,
            plot = "density",
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            adjust = 1.5,
            pch = "|",
            layout = c(4, 1),
            auto.key = list(columns = 3))

featurePlot(x = iris[, 1:4],
            y = iris$Species,
            plot = "box",
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),
            layout = c(4,1 ),
            auto.key = list(columns = 2))


library(mlbench)
data(BostonHousing)
regVar <- c("age", "lstat", "tax")
str(BostonHousing[, regVar])

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = BostonHousing[, regVar],
            y = BostonHousing$medv,
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 1))

### End:  Visualizations ###
########Pre-processing########
# install.packages("earth")
  library(earth)
  data(etitanic)

#--- Create dummy variables ---
  head(model.matrix(survived ~ ., data = etitanic))
  
  dummies <- dummyVars(survived ~ ., data = etitanic)
  dat.dummies = predict(dummies, newdata = etitanic)

#--- Identify zero or near zero variance predictors
  data(mdrr)
  data.frame(table(mdrrDescr$nR11))
  
  nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
  nzv[nzv$nzv,][1:10,]
  
  nzv <- nearZeroVar(mdrrDescr)
  filteredDescr <- mdrrDescr[, -nzv]
  dim(filteredDescr)
  
  table(mdrrDescr$nTB)/length(mdrrDescr$nTB)

#--- Find linear combo -- not very useful -- requires be to exact
  ltfrDesign <- matrix(0, nrow=6, ncol=6)
  ltfrDesign[,1] <- c(0.9, 1, 1, 1, 1, 1)
  ltfrDesign[,2] <- c(1, 1, 1, 0, 0, 0)
  ltfrDesign[,3] <- c(0, 0, 0, 1, 1, 1)
  ltfrDesign[,4] <- c(1, 0, 0, 1, 0, 0)
  ltfrDesign[,5] <- c(0, 1, 0, 0, 1, 0)
  ltfrDesign[,6] <- c(0, 0, 1, 0, 0, 0.9)
  
  comboInfo <- findLinearCombos(ltfrDesign)
  comboInfo
  ltfrDesign[, -comboInfo$remove]

#--- Center & scaling
  set.seed(96)
  inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)
  
  training <- filteredDescr[inTrain,]
  test <- filteredDescr[-inTrain,]
  trainMDRR <- mdrrClass[inTrain]
  testMDRR <- mdrrClass[-inTrain]
  
  preProcValues <- preProcess(training, method = c("center", "scale"))
  
  trainTransformed <- predict(preProcValues, training)
  testTransformed <- predict(preProcValues, test)

#--- Inputation using knnImpute, or bagImpute
  row <- sample (1:nrow (iris), 8)
  iris [row, 1] <- NA
  
  x <- iris [, 1:4]
  pp <- preProcess (x, method = c("bagImpute"))
  x.imputed <- predict (pp, newdata = x)
  
  stopifnot( all (!is.na (x.imputed)))
  stopifnot( length (x) == length (x.imputed))

#---Transform predictor
  library(AppliedPredictiveModeling)
  transparentTheme(trans = .4)
  plotSubset <- data.frame(scale(mdrrDescr[, c("nC", "X4v")]))
  xyplot(nC ~ X4v,
         data = plotSubset,
         groups = mdrrClass,
         auto.key = list(columns = 2))
  transformed <- spatialSign(plotSubset)
  transformed <- as.data.frame(transformed)
  
  xyplot(nC ~ X4v,
         data = transformed,
         groups = mdrrClass,
         auto.key = list(columns = 2))
  
  preProcValues2 <- preProcess(training, method = "BoxCox")
  trainBC <- predict(preProcValues2, training)
  testBC <- predict(preProcValues2, test)
  preProcValues2

#--- Put all together
  data(schedulingData)
  str(schedulingData)
  pp_no_nzv <- preProcess(schedulingData[, -8],
                          method = c("center", "scale", "YeoJohnson", "nzv"))
  
  transformed <- predict(pp_no_nzv, newdata = schedulingData[, -8])
  head(transformed)

#--- Class Distance Calculations  _it is not working!!!
  centroids <- classDist(trainBC, trainMDRR)
  distances <- predict(centroids, testBC)
  distances <- as.data.frame(distances)
  head(distances)
  summary(trainMDRR)
  xyplot(dist.Active ~ dist.Inactive,
         data = distances, groups = testMDRR,
         auto.key = list(columns = 2))
  
  
  trainSet <- sample(1:150, 100)
  distData <- classDist(iris[trainSet, 1:4], iris$Species[trainSet])
  newDist <- predict(distData,  iris[-trainSet, 1:4])
  splom(newDist, groups = iris$Species[-trainSet])
  
########Data Splitting###########
#---Simple Splitting Based on the Outcome: create balanced split 

  set.seed(3456)
  trainIndex <- createDataPartition(iris$Species, p = .8,
                                    list = FALSE,
                                    times = 1)
  head(trainIndex)
  training = iris[trainIndex,]
  table(training$Species)

  
#--- Splitting Based on Predictors: maxDissim, minDiss, sumDiss
  library(mlbench)
  data(BostonHousing)
  
  testing <- scale(BostonHousing[, c("age", "nox")])
  set.seed(5)
  
  ## A random sample of 5 data points
  startSet <- sample(1:dim(testing)[1], 5)
  samplePool <- testing[-startSet,]
  start <- testing[startSet,]
  newSamp <- maxDissim(start, samplePool, n = 20)
  head(newSamp)
  
########Variable Importance###########

  data(mdrr)
  filterVarImp(mdrrDescr[, 1:5], mdrrClass)
  
  data(BloodBrain)
  filterVarImp(bbbDescr[, 1:5], logBBB, nonpara = FALSE)
  apply(bbbDescr[, 1:5], 2,
        function(x, y) summary(lm(y~x))$coefficients[2,3],
        y = logBBB)
  filterVarImp(bbbDescr[, 1:5], logBBB, nonpara = TRUE)
  
  gbmImp <- varImp(gbmFit3, scale = FALSE)
  gbmImp
  
  RocImp <- filterVarImp(x = training[, -ncol(training)], y = training$Class)
  head(RocImp, n=12)
  
  RocImp2 <- varImp(svmFit, scale = FALSE)  #roc curve for each predictor 
  RocImp2
  
  plot(gbmImp, top = 20)
  
########Measureing model performance###########
  #-- functions:  postResample, sensitivity, specificity, posPredValue,  
  #--             negPredValue, confusionMatrix, mnLogLoss,
  #--             multiClassSummary
  
  testPred <- predict(gbmFit3, testing)
  postResample(testPred, testing$Class)
  sensitivity(testPred, testing$Class)
  confusionMatrix(testPred, testing$Class)
  confusionMatrix(gbmFit3)
  
  test_results <- predict(gbmFit3, testing, type = "prob")
  test_results$obs <- testing$Class
  head(test_results)
  mnLogLoss(test_results, lev = levels(test_results$obs))
  
  test_results$pred <- predict(gbmFit3, testing)
  multiClassSummary(test_results, lev = levels(test_results$obs))
  
  #--- Evaluating class Probabilities
  set.seed(2)
  trainingSim <- twoClassSim(1000) #sim two class dataset
  evalSim     <- twoClassSim(1000)
  testingSim  <- twoClassSim(1000)
  
  ctrl <- trainControl(method = "cv",
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)
  
  set.seed(1045)
  fdaModel <- train(Class ~ ., data = trainingSim,
                    method = "fda",
                    metric = "ROC",
                    tuneLength = 20,
                    trControl = ctrl)
  set.seed(1045)
  ldaModel <- train(Class ~ ., data = trainingSim,
                    method = "lda",
                    metric = "ROC",
                    trControl = ctrl)
  
  set.seed(1045)
  c5Model <- train(Class ~ ., data = trainingSim,
                   method = "C5.0",
                   metric = "ROC",
                   tuneLength = 10,
                   trControl = ctrl)
  
  ## A summary of the resampling results:
  getTrainPerf(fdaModel)
  
  getTrainPerf(ldaModel)
  
  getTrainPerf(c5Model)
  
  evalResults <- data.frame(Class = evalSim$Class)
  evalResults$FDA <- predict(fdaModel, evalSim, type = "prob")[,"Class1"]
  evalResults$LDA <- predict(ldaModel, evalSim, type = "prob")[,"Class1"]
  evalResults$C5.0 <- predict(c5Model, evalSim, type = "prob")[,"Class1"]
  head(evalResults)
  
  trellis.par.set(caretTheme())
  liftData <- lift(Class ~ FDA + LDA + C5.0, data = evalResults)
  plot(liftData, values = 60, auto.key = list(columns = 3,
                                              lines = TRUE,
                                              points = FALSE))
  #evaluate if prob prediction is consistent with event rate in data
  trellis.par.set(caretTheme())
  calData <- calibration(Class ~ FDA + LDA + C5.0,
                         data = evalResults,
                         cuts = 13)
  plot(calData, type = "l", auto.key = list(columns = 3,
                                            lines = TRUE,
                                            points = FALSE))
  
  
  
########Model Training and Tuing###########
  library(mlbench)
  data(Sonar)
  str(Sonar[, 1:10])
  
#--- Basic paramater tuning  
  set.seed(998)
  inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
  training <- Sonar[ inTraining,]
  testing  <- Sonar[-inTraining,]
  
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)
  set.seed(825)
  gbmFit1 <- train(Class ~ ., data = training,
                   method = "gbm",
                   trControl = fitControl,
                   ## This last option is actually one
                   ## for gbm() that passes through
                   verbose = FALSE)
  gbmFit1

#--- Alternate tuning grids
  gbmGrid <-  expand.grid(interaction.depth = c( 2, 3),
                          n.trees = (1:10)*100,
                          shrinkage = 0.1,
                          n.minobsinnode = 20)
  
  nrow(gbmGrid)
  
  set.seed(825)
  gbmFit2 <- train(Class ~ ., data = training,
                   method = "gbm",
                   trControl = fitControl,
                   verbose = FALSE,
                   ## Now specify the exact models 
                   ## to evaluate:
                   tuneGrid = gbmGrid)
  gbmFit2
#--- Plotting the Resampling Profile  
  trellis.par.set(caretTheme())
  plot(gbmFit2) 
  plot(gbmFit2, metric = "Kappa")
  ggplot(gbmFit2)
  update(gbmFit2, param = list(n.trees=750, interaction.depth=2, 
                               shrinkage=0.1, n.minobsinnode=20))
#--- Alternative Performance Metrics
  head(twoClassSummary)
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             ## Estimate class probabilities
                             classProbs = TRUE,
                             ## Evaluate performance using 
                             ## the following function
                             summaryFunction = twoClassSummary)
  
  set.seed(825)
  gbmFit3 <- train(Class ~ ., data = training,
                   method = "gbm",
                   trControl = fitControl,
                   verbose = FALSE,
                   tuneGrid = gbmGrid,
                   ## Specify which metric to optimize
                   metric = "ROC")
  gbmFit3
  ggplot(gbmFit3)
  
#---Choosing final model
  whichTwoPct <- tolerance(gbmFit3$results, metric = "ROC",
                           tol = 2, maximize = TRUE)
  cat("best model within 2 pct of best:\n")
  gbmFit3$results[whichTwoPct,1:6]

  OneSE  = oneSE(gbmFit3$results, metric = "ROC", num=5,
                           maximize = TRUE)
  cat("best model within 1SE of best:\n")
  gbmFit3$results[OneSE,1:6]
 
   best(gbmFit3$results, metric = "ROC", maximize = TRUE)
  
#--- Extract predictions and class probability
   predict(gbmFit3, newdata = head(testing))
   predict(gbmFit3, newdata = head(testing), type = "prob")
   
#--- Comparing resample distribution
   #-- within-model
   densityplot(gbmFit3)
   
   #---Between models
   set.seed(825)
   svmFit <- train(Class ~ ., data = training,
                   method = "svmRadial",
                   trControl = fitControl,
                   preProc = c("center", "scale"),
                   tuneLength = 8,
                   metric = "ROC")
   svmFit
   
   set.seed(825)
   rdaFit <- train(Class ~ ., data = training,
                   method = "rda",
                   trControl = fitControl,
                   tuneLength = 4,
                   metric = "ROC")
   rdaFit
   
   resamps <- resamples(list(GBM = gbmFit3,
                             SVM = svmFit,
                             RDA = rdaFit))
   resamps$values
   summary(resamps)
   bwplot(resamps, layout = c(3, 1))
   dotplot(resamps, metric = "ROC")
   xyplot(resamps, what = "BlandAltman")
   splom(resamps)
   
   difValues <- diff(resamps)
   difValues
   summary(difValues)
   
   #--- Fitting model without parameter tuning
   fitControl <- trainControl(method = "none", classProbs = TRUE)
   
   set.seed(825)
   gbmFit4 <- train(Class ~ ., data = training,
                    method = "gbm",
                    trControl = fitControl,
                    verbose = FALSE,
                    ## Only a single model can be passed to the
                    ## function when no resampling is used:
                    tuneGrid = data.frame(interaction.depth = 4,
                                          n.trees = 100,
                                          shrinkage = .1,
                                          n.minobsinnode = 20),
                    metric = "ROC")
   gbmFit4
   predict(gbmFit4, newdata = head(testing))
   predict(gbmFit4, newdata = head(testing), type = "prob")

   
########Parallel Processing###########
   library(doMC)
   registerDoMC(cores = 4)
   ## All subsequent models are then run in parallel
   model <- train(Class ~ ., data = training, method = "rf")
   
   gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                           n.trees = (1:15)*100,
                           shrinkage = 0.1,
                           n.minobsinnode = 20)
   
   fitControl <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 5,
                              ## Estimate class probabilities
                              classProbs = TRUE,
                              ## Evaluate performance using 
                              ## the following function
                              summaryFunction = twoClassSummary,
                              allowParallel = TRUE)
   set.seed(825)
   pre.tm=proc.time()
   gbmFit3 <- train(Class ~ ., data = training,
                    method = "gbm",
                    trControl = fitControl,
                    verbose = FALSE,
                    tuneGrid = gbmGrid,
                    ## Specify which metric to optimize
                    metric = "ROC")
   proc.time()-pre.tm
   
   gbmFit3
   ggplot(gbmFit3)
   
########Using your own model: SVMs with Laplacian Kernels###########
   #---http://topepo.github.io/caret/custom_models.html
   #--- Fit Kernlan's ksvm
   lpSVM <- list(type = "Classification",
                 library = "kernlab",
                 loop = NULL)
   prm <- data.frame(parameter = c("C", "sigma"),
                     class = rep("numeric", 2),
                     label = c("Cost", "Sigma"))
   lpSVM$parameters <- prm
   svmGrid <- function(x, y, len = NULL, search = "grid") {
     library(kernlab)
     ## This produces low, middle and high values for sigma 
     ## (i.e. a vector with 3 elements). 
     sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
     ## To use grid search:
     if(search == "grid") {
       out <- expand.grid(sigma = mean(as.vector(sigmas[-2])),
                          C = 2 ^((1:len) - 3))
     } else {
       ## For random search, define ranges for the parameters then
       ## generate random values for them
       rng <- extendrange(log(sigmas), f = .75)
       out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])),
                         C = 2^runif(len, min = -5, max = 8))
     }
     out
   }
   lpSVM$grid <- svmGrid
   svmFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
     ksvm(x = as.matrix(x), y = y,
          kernel = rbfdot,
          kpar = list(sigma = param$sigma),
          C = param$C,
          prob.model = classProbs,
          ...)
   }
   lpSVM$fit <- svmFit
   svmPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
     predict(modelFit, newdata)
   lpSVM$predict <- svmPred
   svmProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
     predict(modelFit, newdata, type="probabilities")
   lpSVM$prob <- svmProb
   svmSort <- function(x) x[order(x$C),]
   lpSVM$sort <- svmSort
   lpSVM$levels <- function(x) lev(x)
   
   library(mlbench)
   data(Sonar)
   
   library(caret)
   set.seed(998)
   inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
   training <- Sonar[ inTraining,]
   testing  <- Sonar[-inTraining,]
   
   fitControl <- trainControl(method = "repeatedcv",
                              ## 10-fold CV...
                              number = 10,
                              ## repeated ten times
                              repeats = 5)
   
   set.seed(825)
   Laplacian <- train(Class ~ ., data = training,
                      method = lpSVM,
                      preProc = c("center", "scale"),
                      tuneLength = 8,
                      trControl = fitControl)
   print(Laplacian, digits = 3)
   
   trellis.par.set(caretTheme())
   plot(Laplacian,  scales = list(x = list(log = 2)))
   plot(Laplacian)
   
########Using your own model: LogitBoost###########
########Using your own model: nonstandard Formulas###########
   library(mboost)
   library(TH.data)
   library(party)
   data("bodyfat", package = "TH.data")
   mod <- mboost(DEXfat ~ btree(age) + bols(waistcirc) + bbs(hipcirc),
                 data = bodyfat)
   mod
   
   modelInfo <- list(label = "Model-based Gradient Boosting",
                     library = "mboost",
                     type = "Regression",
                     parameters = data.frame(parameter = "parameter",
                                             class = "character",
                                             label = "parameter"),
                     grid = function(x, y, len = NULL, search = "grid")
                       data.frame(parameter = "none"),
                     loop = NULL,
                     fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                       ## mboost requires a data frame with predictors and response
                       dat <- if(is.data.frame(x)) x else as.data.frame(x)
                       dat$DEXfat <- y
                       mod <- mboost(DEXfat ~ btree(age) + bols(waistcirc) + bbs(hipcirc),
                                     data = dat)
                     },
                     predict = function(modelFit, newdata, submodels = NULL) {
                       if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                       ## By default a matrix is returned; we convert it to a vector
                       predict(modelFit, newdata)[,1]
                     },
                     prob = NULL,
                     predictors = function(x, ...) {
                       unique(as.vector(variable.names(x)))
                     },
                     tags = c("Ensemble Model", "Boosting", "Implicit Feature Selection"),
                     levels = NULL,
                     sort = function(x) x)
   
   set.seed(307)
   mboost_resamp <- train(DEXfat ~ age + waistcirc + hipcirc,
                          data = bodyfat,
                          method = modelInfo,
                          trControl = trainControl(method = "repeatedcv",
                                                   repeats = 5))
   mboost_resamp
########Using your own model: PLS feature extraction ###########
   data(tecator)
   
   set.seed(930)
   
   ## We will model the protein content data
   trainMeats <- createDataPartition(endpoints[,3], p = 3/4)
   absorpTrain  <- absorp[trainMeats[[1]], ]
   proteinTrain <- endpoints[trainMeats[[1]], 3]
   absorpTest   <- absorp[-trainMeats[[1]], ]
   proteinTest  <- endpoints[-trainMeats[[1]], 3]
   
   pls_rf <- list(label = "PLS-RF",
                  library = c("pls", "randomForest"),
                  type = "Regression",
                  ## Tune over both parameters at the same time
                  parameters = data.frame(parameter = c('ncomp', 'mtry'),
                                          class = c("numeric", 'numeric'),
                                          label = c('#Components',
                                                    '#Randomly Selected Predictors')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      grid <- expand.grid(ncomp = seq(1, min(ncol(x) - 1, len), by = 1),
                                          mtry = 1:len)
                    } else {
                      grid <- expand.grid(ncomp = sample(1:ncol(x), size = len),
                                          mtry = sample(1:ncol(x), size = len))
                    }
                    ## We can't have mtry > ncomp
                    grid <- subset(grid, mtry <= ncomp)
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    ## First fit the pls model, generate the training set scores,
                    ## then attach what is needed to the random forest object to 
                    ## be used later
                    
                    ## plsr only has a formula interface so create one data frame
                    dat <- if(!is.data.frame(x)) x <- as.data.frame(x)
                    dat$y <- y
                    pre <- plsr(y~ ., data = dat, ncomp = param$ncomp)
                    scores <- predict(pre, x, type = "scores")
                    colnames(scores) <- paste("score", 1:param$ncomp, sep = "")
                    mod <- randomForest(scores, y, mtry = param$mtry, ...)
                    mod$projection <- pre$projection
                    mod
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    ## Now apply the same scaling to the new samples
                    scores <- as.matrix(newdata)  %*% modelFit$projection
                    colnames(scores) <- paste("score", 1:ncol(scores), sep = "")
                    scores <- as.data.frame(scores)
                    ## Predict the random forest model
                    predict(modelFit, scores)
                  },
                  prob = NULL,
                  varImp = NULL,
                  predictors = function(x, ...) rownames(x$projection),
                  levels = function(x) x$obsLevels,
                  sort = function(x) x[order(x[,1]),])
   
   meatCtrl <- trainControl(method = "repeatedcv", repeats = 5)
   
   ## These will take a while for these data
   set.seed(184)
   plsrf <- train(absorpTrain, proteinTrain,
                  method = pls_rf,
                  preProc = c("center", "scale"),
                  tuneLength = 10,
                  ntree = 1000,
                  trControl = meatCtrl)
   ggplot(plsrf, plotType = "level")
   
   set.seed(184)
   rfOnly <- train(absorpTrain, proteinTrain,
                   method = "rf",
                   tuneLength = 10,
                   ntree = 1000,
                   trControl = meatCtrl)
   getTrainPerf(rfOnly)
   
   set.seed(184)
   plsOnly <- train(absorpTrain, proteinTrain,
                    method = "pls",
                    tuneLength = 20,
                    preProc = c("center", "scale"),
                    trControl = meatCtrl)
   getTrainPerf(plsOnly)
   postResample(predict(plsrf, absorpTest), proteinTest)
   postResample(predict(rfOnly, absorpTest), proteinTest)
   postResample(predict(plsOnly, absorpTest), proteinTest)
   
   
   
#########Optimizaing probability threshold for class imbalance##########
   library(caret)
   library(pROC)
   
   set.seed(442)
   trainingSet <- twoClassSim(n = 500, intercept = -16)
   testingSet  <- twoClassSim(n = 500, intercept = -16)
   table(trainingSet$Class)
   
   set.seed(949)
   mod0 <- train(Class ~ ., data = trainingSet,
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = data.frame(mtry = 3),
                 ntree = 1000,
                 trControl = trainControl(method = "repeatedcv",
                                          repeats = 5,
                                          classProbs = TRUE,
                                          summaryFunction = twoClassSummary))
   getTrainPerf(mod0)
   
   roc0 <- roc(testingSet$Class,
               predict(mod0, testingSet, type = "prob")[,1],
               levels = rev(levels(testingSet$Class)))
   roc0
   plot(roc0, print.thres = c(.5), type = "S",
        print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
        print.thres.cex = .8,
        legacy.axes = TRUE)
   
   ## use sequential parameters (parms not require refit() to produce predictions
   ## Get the model code for the original random forest method:
   
   thresh_code <- getModelInfo("rf", regex = FALSE)[[1]]
   thresh_code$type <- c("Classification")
   ## Add the threshold as another tuning parameter
   thresh_code$parameters <- data.frame(parameter = c("mtry", "threshold"),
                                        class = c("numeric", "numeric"),
                                        label = c("#Randomly Selected Predictors",
                                                  "Probability Cutoff"))
   ## The default tuning grid code:
   thresh_code$grid <- function(x, y, len = NULL, search = "grid") {
     p <- ncol(x)
     if(search == "grid") {
       grid <- expand.grid(mtry = floor(sqrt(p)),
                           threshold = seq(.01, .99, length = len))
     } else {
       grid <- expand.grid(mtry = sample(1:p, size = len),
                           threshold = runif(1, 0, size = len))
     }
     grid
   }
   
   ## Here we fit a single random forest model (with a fixed mtry)
   ## and loop over the threshold values to get predictions from the same
   ## randomForest model.
   thresh_code$loop = function(grid) {
     library(plyr)
     loop <- ddply(grid, c("mtry"),
                   function(x) c(threshold = max(x$threshold)))
     submodels <- vector(mode = "list", length = nrow(loop))
     for(i in seq(along = loop$threshold)) {
       index <- which(grid$mtry == loop$mtry[i])
       cuts <- grid[index, "threshold"]
       submodels[[i]] <- data.frame(threshold = cuts[cuts != loop$threshold[i]])
     }
     list(loop = loop, submodels = submodels)
   }
   
   ## Fit the model independent of the threshold parameter
   thresh_code$fit = function(x, y, wts, param, lev, last, classProbs, ...) {
     if(length(levels(y)) != 2)
       stop("This works only for 2-class problems")
     randomForest(x, y, mtry = param$mtry, ...)
   }
   
   ## Now get a probability prediction and use different thresholds to
   ## get the predicted class
   thresh_code$predict = function(modelFit, newdata, submodels = NULL) {
     class1Prob <- predict(modelFit,
                           newdata,
                           type = "prob")[, modelFit$obsLevels[1]]
     ## Raise the threshold for class #1 and a higher level of
     ## evidence is needed to call it class 1 so it should 
     ## decrease sensitivity and increase specificity
     out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                   modelFit$obsLevels[1],
                   modelFit$obsLevels[2])
     if(!is.null(submodels)) {
       tmp2 <- out
       out <- vector(mode = "list", length = length(submodels$threshold))
       out[[1]] <- tmp2
       for(i in seq(along = submodels$threshold)) {
         out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                              modelFit$obsLevels[1],
                              modelFit$obsLevels[2])
       }
     }
     out
   }
   
   ## The probabilities are always the same but we have to create
   ## mulitple versions of the probs to evaluate the data across
   ## thresholds
   thresh_code$prob = function(modelFit, newdata, submodels = NULL) {
     out <- as.data.frame(predict(modelFit, newdata, type = "prob"))
     if(!is.null(submodels)) {
       probs <- out
       out <- vector(mode = "list", length = length(submodels$threshold)+1)
       out <- lapply(out, function(x) probs)
     }
     out
   }
   
   
   fourStats <- function (data, lev = levels(data$obs), model = NULL) {
     ## This code will get use the area under the ROC curve and the
     ## sensitivity and specificity values using the current candidate
     ## value of the probability threshold.
     out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
     
     ## The best possible model has sensitivity of 1 and specificity of 1. 
     ## How far are we from that value?
     coords <- matrix(c(1, 1, out["Spec"], out["Sens"]),
                      ncol = 2,
                      byrow = TRUE)
     colnames(coords) <- c("Spec", "Sens")
     rownames(coords) <- c("Best", "Current")
     c(out, Dist = dist(coords)[1])
   }
   
   set.seed(949)
   mod1 <- train(Class ~ ., data = trainingSet,
                 method = thresh_code,
                 ## Minimize the distance to the perfect model
                 metric = "Dist",
                 maximize = FALSE,
                 tuneLength = 20,
                 ntree = 1000,
                 trControl = trainControl(method = "repeatedcv",
                                          repeats = 5,
                                          classProbs = TRUE,
                                          summaryFunction = fourStats))
   
   mod1
   
   
   library(reshape2)
   metrics <- mod1$results[, c(2, 4:6)]
   metrics <- melt(metrics, id.vars = "threshold",
                   variable.name = "Resampled",
                   value.name = "Data")
   
   ggplot(metrics, aes(x = threshold, y = Data, color = Resampled)) +
     geom_line() +
     ylab("") + xlab("Probability Cutoff") +
     theme(legend.position = "top")
#########Randomly search hyperparameter##########
   ## use search="random" in trainControl
   library(mlbench)
   data(Sonar)
   
   library(caret)
   set.seed(998)
   inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
   training <- Sonar[ inTraining,]
   testing  <- Sonar[-inTraining,]
   
   fitControl <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 5,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              search = "random")
   
   set.seed(825)
   som_fit <- train(Class ~ ., data = training,
                    method = "xyf",
                    preProc = c("center", "scale"),
                    metric = "ROC",
                    tuneLength = 30,
                    trControl = fitControl)
   som_fit
   
   ggplot(som_fit) +
     geom_smooth(se = FALSE, span = .8, method = loess) +
     theme(legend.position = "top")
   
#########Adpative Resampling##########
   ## adaptively resample the tuning parameter grid in a way that concentrates 
   ## on values that are the in the neighborhood of the optimal settings. 
   ## paper:  http://arxiv.org/abs/1405.6974
   
   ## Implemented via trainControl

   library(QSARdata)
   data(Mutagen)
   
   set.seed(4567)
   inTraining <- createDataPartition(Mutagen_Outcome, p = .75, list = FALSE)
   training_x <- Mutagen_Dragon[ inTraining,]
   training_y <- Mutagen_Outcome[ inTraining]
   testing_x  <- Mutagen_Dragon[-inTraining,]
   testing_y  <- Mutagen_Outcome[-inTraining]
   
   ## Get rid of predictors that are very sparse
   nzv <- nearZeroVar(training_x)
   training_x <- training_x[, -nzv]
   testing_x  <-  testing_x[, -nzv]
   
   fitControl <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 5,
                              ## Estimate class probabilities
                              classProbs = TRUE,
                              ## Evaluate performance using 
                              ## the following function
                              summaryFunction = twoClassSummary)
   set.seed(825)
   svmFit <- train(x = training_x,
                   y = training_y,
                   method = "svmRadial",
                   trControl = fitControl,
                   preProc = c("center", "scale"),
                   tuneLength = 8,
                   metric = "ROC")
   
   fitControl2 <- trainControl(method = "adaptive_cv",
                               number = 10,
                               repeats = 5,
                               ## Estimate class probabilities
                               classProbs = TRUE,
                               ## Evaluate performance using 
                               ## the following function
                               summaryFunction = twoClassSummary,
                               ## Adaptive resampling information:
                               adaptive = list(min = 10,
                                               alpha = 0.05,
                                               method = "gls",
                                               complete = TRUE))
   set.seed(825)
   svmFit2 <- train(x = training_x,
                    y = training_y,
                    method = "svmRadial",
                    trControl = fitControl2,
                    preProc = c("center", "scale"),
                    tuneLength = 8,
                    metric = "ROC")
   
#########Feature Selection: RFE##########
   ## Built-in feature selection:  predictors returns vectors used in the final model
   ## Wrapper: RFE (recursive feature elemination): rfeControl 
   library(caret)
   library(mlbench)
   library(Hmisc)
   library(randomForest)
   
   #--- Simulation
   n <- 100
   p <- 40
   sigma <- 1
   set.seed(1)
   sim <- mlbench.friedman1(n, sd = sigma)
   colnames(sim$x) <- c(paste("real", 1:5, sep = ""),
                        paste("bogus", 1:5, sep = ""))
   bogus <- matrix(rnorm(n * p), nrow = n)
   colnames(bogus) <- paste("bogus", 5+(1:ncol(bogus)), sep = "")
   x <- cbind(sim$x, bogus)
   y <- sim$y
   
   normalization <- preProcess(x)
   x <- predict(normalization, x)
   x <- as.data.frame(x)
   subsets <- c(1:5, 10, 15, 20, 25)
   
   #---rfeControl
   set.seed(10)
   
   ctrl <- rfeControl(functions = lmFuncs,
                      method = "repeatedcv",
                      repeats = 5,
                      verbose = FALSE)
   
   lmProfile <- rfe(x, y,
                    sizes = subsets,
                    rfeControl = ctrl)
   
   lmProfile
   predictors(lmProfile)
   lmProfile$fit
   head(lmProfile$resample)
   plot(lmProfile, type = c("g", "o"))
   
   #--- Use FE for RandomForest (can be used to arbitrary)
   rfRFE <-  list(summary = defaultSummary,
                  fit = function(x, y, first, last, ...){
                    library(randomForest)
                    randomForest(x, y, importance = first, ...)
                  },
                  pred = function(object, x)  predict(object, x),
                  rank = function(object, x, y) {
                    vimp <- varImp(object)
                    vimp <- vimp[order(vimp$Overall,decreasing = TRUE),,drop = FALSE]
                    vimp$var <- rownames(vimp)
                    vimp
                  },
                  selectSize = pickSizeBest,
                  selectVar = pickVars)
   example <- data.frame(RMSE = c(3.215, 2.819, 2.414, 2.144,
                                  2.014, 1.997, 2.025, 1.987,
                                  1.971, 2.055, 1.935, 1.999,
                                  2.047, 2.002, 1.895, 2.018),
                         Variables = 1:16)
   ## Find the row with the absolute smallest RMSE
   smallest <- pickSizeBest(example, metric = "RMSE", maximize = FALSE)
   smallest
   ## Now one that is within 10% of the smallest
   within10Pct <- pickSizeTolerance(example, metric = "RMSE", tol = 10, maximize = FALSE)
   within10Pct
   minRMSE <- min(example$RMSE)
   example$Tolerance <- (example$RMSE - minRMSE)/minRMSE * 100
   
   ## Plot the profile and the subsets selected using two different criteria
   
   par(mfrow = c(2, 1), mar = c(3, 4, 1, 2))
   
   plot(example$Variables[-c(smallest, within10Pct)],
        example$RMSE[-c(smallest, within10Pct)],
        ylim = extendrange(example$RMSE),
        ylab = "RMSE", xlab = "Variables")
   
   points(example$Variables[smallest],
          example$RMSE[smallest], pch = 16, cex= 1.3)
   
   points(example$Variables[within10Pct],
          example$RMSE[within10Pct], pch = 17, cex= 1.3)
   
   with(example, plot(Variables, Tolerance))
   abline(h = 10, lty = 2, col = "darkgrey")
   
   #--selectVar
   ctrl$functions <- rfRFE
   ctrl$returnResamp <- "all"
   set.seed(10)
   rfProfile <- rfe(x, y, sizes = subsets, rfeControl = ctrl)
   rfProfile
   
   trellis.par.set(caretTheme())
   plot1 <- plot(rfProfile, type = c("g", "o"))
   plot2 <- plot(rfProfile, type = c("g", "o"), metric = "Rsquared")
   print(plot1, split=c(1,1,1,2), more=TRUE)
   print(plot2, split=c(1,2,1,2))
   
   trellis.par.set(theme1)
   plot1 <- xyplot(rfProfile,
                   type = c("g", "p", "smooth"),
                   ylab = "RMSE CV Estimates")
   plot2 <- densityplot(rfProfile,
                        subset = Variables < 5,
                        adjust = 1.25,
                        as.table = TRUE,
                        xlab = "RMSE CV Estimates",
                        pch = "|")
   print(plot1, split=c(1,1,1,2), more=TRUE)
   print(plot2, split=c(1,2,1,2))
   
   
#########Feature Selection: Univeraite Filters##########
   #---: sbf(predictors, outcome, sbfControl = sbfControl(), ...)
   #---: sbf(formula, data, sbfControl = sbfControl(), ...)
   #--- functions: score (anovaScores and gamScores. anovaScores), filter, fit, summary, pred
   filterCtrl <- sbfControl(functions = rfSBF,
                            method = "repeatedcv", repeats = 5)
   set.seed(10)
   rfWithFilter <- sbf(x, y, sbfControl = filterCtrl)
   rfWithFilter
   predictors(rfWithFilter)
   
   
#########Feature Selection: Genetic Algorithms##########
   obj <- gafs(x = predictors,
               y = outcome,
               iters = 100)
   
   ctrl <- gafsControl(functions = caretGA)
   obj <- gafs(x = predictors,
               y = outcome,
               iters = 100,
               gafsControl = ctrl,
               ## Now pass options to `train`
               
               method = "lm")
   #--- Example:
   library(mlbench)
   n <- 100
   p <- 40
   sigma <- 1
   set.seed(1)
   sim <- mlbench.friedman1(n, sd = sigma)
   colnames(sim$x) <- c(paste("real", 1:5, sep = ""),
                        paste("bogus", 1:5, sep = ""))
   bogus <- matrix(rnorm(n * p), nrow = n)
   colnames(bogus) <- paste("bogus", 5+(1:ncol(bogus)), sep = "")
   x <- cbind(sim$x, bogus)
   y <- sim$y
   normalization <- preProcess(x)
   x <- predict(normalization, x)
   x <- as.data.frame(x)
   
   ga_ctrl <- gafsControl(functions = rfGA,
                          method = "repeatedcv",
                          repeats = 5)
   
   ## Use the same random number seed as the RFE process
   ## so that the same CV folds are used for the external
   ## resampling. 
   set.seed(10)
   rf_ga <- gafs(x = x, y = y,
                 iters = 200,
                 gafsControl = ga_ctrl)
   rf_ga
   
   plot(rf_ga) + theme_bw()
   
   #-- Example
   library(desirability)
   rfGA2 <- rfGA
   rfGA2$fitness_intern <- function (object, x, y, maximize, p) {
     RMSE <- rfStats(object)[1]
     d_RMSE <- dMin(0, 4)
     d_Size <- dMin(1, p, 2)
     overall <- dOverall(d_RMSE, d_Size)
     D <- predict(overall, data.frame(RMSE, ncol(x)))
     c(D = D, RMSE = as.vector(RMSE))
   }
   ga_ctrl_d <- gafsControl(functions = rfGA2,
                            method = "repeatedcv",
                            repeats = 5,
                            metric = c(internal = "D", external = "RMSE"),
                            maximize = c(internal = TRUE, external = FALSE))
   
   set.seed(10)
   rf_ga_d <- gafs(x = x, y = y,
                   iters = 200,
                   gafsControl = ga_ctrl_d)
   
   rf_ga_d
   
#########Feature selection: Simulated Annealing##########
   #-- Basci syntax:
   obj <- safs(x = predictors,
               y = outcome,
               iters = 100)
   ctrl <- safsControl(functions = caretSA)
   obj <- safs(x = predictors,
               y = outcome,
               iters = 100,
               safsControl = ctrl,
               ## Now pass options to `train`
               method = "lm")
   #--- Example:
   sa_ctrl <- safsControl(functions = rfSA,
                          method = "repeatedcv",
                          repeats = 5,
                          improve = 50)
   
   set.seed(10)
   rf_sa <- safs(x = x, y = y,
                 iters = 500,
                 safsControl = sa_ctrl)
   rf_sa
   #--- Example:
   grid <- expand.grid(old = c(4, 3.5),
                       new = c(4.5, 4, 3.5) + 1,
                       iter = 1:40)
   grid <- subset(grid, old < new)
   
   grid$prob <- apply(grid, 1,
                      function(x)
                        safs_prob(new = x["new"],
                                  old= x["old"],
                                  iteration = x["iter"]))
   
   grid$Difference <- factor(grid$new - grid$old)
   grid$Group <- factor(paste("Current Value", grid$old))
   
   ggplot(grid, aes(x = iter, y = prob, color = Difference)) +
     geom_line() + facet_wrap(~Group) + theme_bw() +
     ylab("Probability") + xlab("Iteration")
   
   
   