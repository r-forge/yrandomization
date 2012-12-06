

random.y <- function(A) { 
  for (i in 1:nrow(A)) { A[,1] <- sample(A[,1]) } 
  outplut=list(A)
} 

random.mat <- function(A, repl=10) { 
  random.A <- replicate(repl, random.y(A))
  random.mat.dataframe = data.frame(random.A)
  .GlobalEnv[["random.mat.dataframe"]] <- random.mat.dataframe
} 


QY <- train(qsar.descriptors.cor, unlist(qsar.activity),
            "pls",
            tuneLength = 20,
            trControl = trainControl(
              method = "boot632", returnResamp = "all"))


Ytest <- function(A) { 
  for (i in 1:ncol(A)) { Q <- train(qsar.descriptors.cor, unlist(random.mat.dataframe[,i]),
                                    "pls",
                                    tuneLength = 20,
                                    trControl = trainControl(
                                      method = "boot632", returnResamp = "all"))
                         Qf <- Q$results[which.min(Q$results[,2] ), ]                      
                         outplut=list(Qf)
                         
                         .GlobalEnv[["Qf"]] <- Qf} 
} 



Yfinal <- function(Ytest, repl=10) { 
  random.A <- replicate(repl, random.y(A))
  random.mat.dataframe = data.frame(random.A)
  .GlobalEnv[["random.mat.dataframe"]] <- random.mat.dataframe
} 




DF = data.frame(outplut[1:col(A)])

DF = data.frame(outplut[1:col(A)])


tobs <- function(A) { 
  (replicate(1000, shuffle1(A), simplify = "array")) } 

Ycbind function(A) { 
  
  
  toba <- sample(datamatrix[,1])
  
  desc <- qsar.descriptors.cor
  
  plsFit<- caret::train(desc, unlist(qsar.activity), 
                        "pls",
                        tuneLength = 15,
                        trControl = fitControl)
  
  
  
  Q1 <- caret::train(desc, unlist(Y[10]), 
                     "pls",
                     tuneLength = 15,
                     trControl = fitControl)
  
  Q2 <- caret::train(desc, unlist(Y[9]), 
                     "pls",
                     tuneLength = 15,
                     trControl = fitControl)
  
  Ycv1 <- Q1$results[which.min(Q1$results[,2] ), ]
  Ycv2 <- Q2$results[which.min(Q2$results[,2] ), ]