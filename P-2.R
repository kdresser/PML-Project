
# P-2.R

# The training data are chunks of time-series measurement data.
# The testing data are point-wise samples.
# This project will attempt modeling/prediction from instaneous samples.

# *** Skip to below. ***

# First survey of raw training data.
training <- read.csv("pml-training.csv")
names(training)
nrow(training)
ncol(training)
head(training)

# First look at the raw data.
testing <- read.csv("pml-testing.csv")
names(testing)
nrow(testing)
ncol(testing)
head(testing)

table(training$classe)

testing[, 001]
testing[, 002]
testing[, 003]
testing[, 004]
testing[, 005]
testing[, 006]
testing[, 007]
testing[, 008]
testing[, 009]
testing[, 010]
testing[, 011]
testing[, 012]
testing[, 013]
testing[, 014]
testing[, 015]
testing[, 016]
testing[, 017]
testing[, 018]
testing[, 019]
testing[, 020]

training[1, 006]
training[1, 012]
training[1, 018]

# *** Start here. ***

# Reread training and testing (evaluation) datasets as T0, E0.
T0 <- read.table("pml-training.csv", header=TRUE, sep=",", quote="\"", as.is=TRUE)
T0$classe <- as.factor(T0$classe)   # Restore "classe" to be a factor.
E0 <- read.table("pml-testing.csv", header=TRUE, sep=",", quote="\"", as.is=TRUE)

Tnames <- names(T0)

# Survey field types.
sFTs <- function(X) {
  Xnames <- names(X)
  nC <- 0;  nI <- 0;  nN <- 0;  nU <- 0;
  suppressWarnings(sink())
  sink("sFTs.txt", type="output", split=TRUE)
  print(" ", quote=FALSE)
  for (i in 1:160) {
    if        (class(X[1, i]) == "character") {
      vFTs[i] <<- "C"
      print(sprintf("%3d : %s : %s", i, vFTs[i], Xnames[i]), quote=FALSE)
      nC <- nC + 1
    } else if (class(X[1, i]) == "integer"  ) {
      vFTs[i] <<- "I"
      print(sprintf("%3d : %s : %s", i, vFTs[i], Xnames[i]), quote=FALSE)    
      nI <- nI + 1
    } else if (class(X[1, i]) == "numeric"  ) {
      vFTs[i] <<- "N"
      print(sprintf("%3d : %s : %s", i, vFTs[i], Xnames[i]), quote=FALSE)    
      nN <- nN + 1
    } else {
      vFTs[i] <<- "U"
      print(sprintf("%3d : %s : %s", i, vFTs[i], Xnames[i]), quote=FALSE)    
      nU <- nU + 1
    }
  }
  print(" ", quote=FALSE)
  print(sprintf("C: %2d", nC), quote=FALSE)
  print(sprintf("I: %2d", nI), quote=FALSE)
  print(sprintf("N: %2d", nN), quote=FALSE)
  print(sprintf("?: %2d", nU), quote=FALSE)
  print(" ", quote=FALSE)
  sink()
}

# A vector of field types by field.
vFTs <- vector(mode="character", 160)
# Survey field types.
sFTs(T0)
sink()
vFTs

# Indices of "C" features.
Cs <- which(vFTs == "C")
Cs

# Indices of "I" features.
Is <- which(vFTs == "I")
Is

Cs_names <- names(data.frame(T0[c(Cs)]))
Cs_names
Is_names <- names(data.frame(T0[c(Is)]))
Is_names

# Survey "new window" field.
nwy <- sum(T0$new_window == "yes")
nwn <- sum(T0$new_window == "no")
nwy
nwn
nwy + nwn

# Survey Character Fields.
sCFs <- function(X, DIV0NUL=FALSE) {
  Xnames <- names(X)
  suppressWarnings(sink())
  sink("sCFs.txt", type="output", split=TRUE)
  # Look at only candidate feature fields.
  for (i in 8:159) {
    if (vFTs[i] == "C") {
      print(" ", quote=FALSE)
      print(sprintf("%3d : %s : %s", i, vFTs[i], Xnames[i]), quote=FALSE)
      nFNA <- 0;  nNUL <- 0;  nNUM <- 0;  nOTH <- 0;
      for (j in 1:nrow(X))   {
        f <- X[j, i]
        # DIV0 to ""?
        if (DIV0NUL & (f == "#DIV/0!")) {
          f <- ""
          X[j, i] <- f
          print("DIV0 -> NUL", quote=FALSE)
        }
        #
        if        (is.null(f)) {
          nNUL <- nNUL + 1    
        } else if (is.na(f)) {
          nFNA <- nFNA + 1
        } else if (f == "") {
          nNUL <- nNUL + 1    
        } else {
          n <- suppressWarnings(as.numeric(f))
          if (is.na(n)) {
            nOTH <- nOTH + 1
            print(sprintf("  %5d : %s", j, f), quote=FALSE)
          } else {
            nNUM <- nNUM + 1
          }
        }
      }
      print(sprintf("  FNA: %5d,  NUL: %5d,  NUM: %5d,  OTH: %5d", 
                    nFNA, nNUL, nNUM, nOTH), quote=FALSE)
    }
  }
  sink()
  if (DIV0NUL) {
    return(X)
  }
}  

# Survey the "#DIV0!" field values.  Convert DIV0's to "".
T1 <- sCFs(T0, DIV0NUL=TRUE)
sink()
T0[832, 12]
T1[832, 12]
sCFs(T1, DIV0NUL=FALSE)

# Convert C, I fields to numeric.
cvt2N <- function(X) {
  for (i in 8:159) {
    if (class(X[1, i]) != "numeric") {
      X[, i] <- suppressWarnings(as.numeric(X[, i]))
      if (class(X[1, i]) == "numeric") {
        vFTs[i] <<- "N"
      }
    }
  }
  return(X)
}  

# T: Convert C, I to numeric.
T2 <- cvt2N(T1)
str(T2)

# E: Convert C, I to numeric.
E2 <- cvt2N(E0)
str(E2)

# Survey NAs.
sNAs <- function(X) {
  Xnames <- names(X)
  suppressWarnings(sink())
  sink("sNAs.txt", type="output", split=TRUE)
  print(" ", quote=FALSE)
  for (i in 1:160) {
    n <- sum(is.na(X[, i]))
    vnNA[i] <<- n
    print(sprintf("%3d : %5d (%3.0f%%) : %s", i, vnNA[i], 100*vnNA[i]/nrow(X), Xnames[i]), quote=FALSE)
  }
  sink()
}

# A vector of NA counts by field.
vnNA <- vector(mode="integer", 160)
# Survey NAs.
sNAs(T2)
sink()

# Fields either have no NAs, or are nearly all (98%) NAs.
# Which fields have zero NAs? (These will be ones used for ML.)
znNA <- which(vnNA == 0)
znNA

# Summarise features.
sFs <- function(X) {
  Xnames <- names(X)
  suppressWarnings(sink())
  sink("sFs.txt", type="output", split=TRUE)
  for (i in 1:160) {
    if (class(X[1, i]) == "numeric") {
      print(" ", quote=FALSE)
      print(sprintf("%3d : %s...", i, Xnames[i]), quote=FALSE)
      print(summary(X[, i]), quote=FALSE)
    }
  }
  sink()
}

# Summarise features, watching for weirdness.
sFs(T2)
sink()

# Create a dataframe containing the class and only the 
# instantaneous measurement data.
fsDF <- function(X) {
  z <- which(vnNA == 0)
  z <- z[z >= 8 & z != 160]
  return(data.frame(X[, c(160, z)]))
}

# T: Subset training data fields.
T3 <- fsDF(T2)
str(T3)
dim(T3)
names(T3)

# E: Subset evaluation data fields.
E3 <- fsDF(E2)
str(E3)
dim(E3)
names(E3)

# Create the model building "training" and "testing" feature sets, using a 75/25 split.
library(caret)
set.seed(1234)
z <- createDataPartition(y=T3$classe, p=0.75, list=FALSE)
training <- T3[z, ]
testing <- T3[-z, ]
dim(training)
dim(testing)

# What does PCA think of the features?
pc <- prcomp(training[, -1])
summary(pc)
# PC1..PC9 explain 95% of variance.
# PC1..PC18 explain 99% of variance.

# Use caret preprocessing to select the top 95% (9 components, as suggested by "prcomp").
prePC <- preProcess(training[, -1], method="pca", pcaComp=9)
prePC
trainPC <- predict(prePC, training[, -1])
trainPC$classe <- training$classe
testPC <- predict(prePC, testing[, -1])
testPC$classe <- testing$classe
save(trainPC, file="trainPC.RData")
save(testPC, file="testPC.RData")

# Model #1.

# A random forest.
# Use the cross validation that's internal to the model generation.
library(caret)
set.seed(1234)
mfrf0 <- train(classe~., method="rf", data=trainPC)
save(mfrf0, file="mfrf0.RData")
load("mfrf0.RData")

###
### -> 0.933 estimated test accuracy.  
###    95% confidence interval: 0.923..0.942
###

# Check the RF model against the testPC (held out from the original training data).
cmrf0 <- confusionMatrix(testPC$classe, predict(mfrf0, testPC))
cmrf0

###
### -> 0.953 actual test accuracy.
###

# Use mfrf0 on the original 20-item test (evaluation) dataset.
E3PC <- predict(prePC, E3[, -1])
E3PCy <- as.character(predict(mfrf0, E3PC))
E3PCy
class(E3PCy)

pml_write_files = function(X){
  n = length(X)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    print(filename, quote=FALSE)
    write.table(X[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
  }
}
pml_write_files(E3PCy)

# Model #2.

# A default SVM, initially with default cost and gamma hyperparameters.
library(e1071)
set.seed(1234)
mfsvm0 <- svm(classe~., data=trainPC)
save(mfsvm0, file="mfsvm0.RData")
load("mfsvm0.RData")
# Snoop a little...
cmsvm0 <- confusionMatrix(testPC$classe, predict(mfsvm0, testPC))
cmsvm0
# -> 0.787 accuracy. 

# Use a grid-search for better hyperparameters.
# Do cross-validation within the trainPC dataset.
library(caret)
library(e1071)

# Measure the SVM modeling time vs training dataset size, to 
# determine how large a dataset to use during a hyperparameter
# grid search.

# trainPC / 1.
set.seed(1234)
T <- trainPC
dim(T)
ptm0 <- proc.time()
set.seed(1234)
mfsvm <- svm(classe~., data=T)
proc.time() - ptm0
# -> 62s

# trainPC / 2.
set.seed(1234)
z <- createDataPartition(y=T$classe, p=0.5, list=FALSE)
T <- T[z, ]
dim(T)
ptm0 <- proc.time()
set.seed(1234)
mfsvm <- svm(classe~., data=T)
proc.time() - ptm0
# -> 16s

# trainPC / 4.
set.seed(1234)
z <- createDataPartition(y=T$classe, p=0.5, list=FALSE)
T <- T[z, ]
dim(T)
ptm0 <- proc.time()
set.seed(1234)
mfsvm <- svm(classe~., data=T)
proc.time() - ptm0
# -> 4.9s

# trainPC / 8.
set.seed(1234)
z <- createDataPartition(y=T$classe, p=0.5, list=FALSE)
T <- T[z, ]
dim(T)
ptm0 <- proc.time()
set.seed(1234)
mfsvm <- svm(classe~., data=T)
proc.time() - ptm0
# -> 1.7s

# Use a 1/4 training dataset for the grid search.
# Create it 25% larger to accomodate 5-fold cross-validation by tune.svm.
set.seed(1234)
z <- createDataPartition(y=trainPC$classe, p=0.25*1.25, list=FALSE)
trainPCgs <- trainPC[z[, 1], ]
dim(trainPCgs)
# -> 4,564 in trainPCgs grid search training + cross-validation dataset.

# First, a coarse search of cost, holding gamma to the default used above.
tune.res <- tune(svm, classe~., data=trainPCgs, 
                 kernel="radial", 
                 ranges=list(cost=2^seq(-5, 10, len=10), gamma=c(0.1111)),
                 tunecontrol=tune.control(cross=5))
tune.res
# -> best cost 322.5

# Second, a finer search of cost and a coarse search of gamma.
tune.res <- tune(svm, classe~., data=trainPCgs, 
                 kernel="radial", 
                 ranges=list(cost=c(200, 300, 400), gamma=10^seq(-3, 3, len=5)),
                 tunecontrol=tune.control(cross=5))
tune.res
# -> cost 300, gamma 1

# Third, a final fine search.
tune.res <- tune(svm, classe~., data=trainPCgs, 
                 kernel="radial", 
                 ranges=list(cost=c(275, 300, 325), gamma=c(0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3)),
                 tunecontrol=tune.control(cross=5))
tune.res
# -> cost 275, gamma 1.3

# SVM test accuracy estimate.
# Use 4/5 of trainPC for training and the remainder for validation.
# Will not redo the hyperparameter search.
set.seed(1234)
z <- createDataPartition(y=trainPC$classe, p=0.8, list=FALSE)
names(trainPC)
trainPC2 <- trainPC[z, ]
validPC2 <- trainPC[-z, ]
dim(trainPC2)
dim(validPC2)
set.seed(1234)
mfsvm2 <- svm(classe~., data=trainPC2, cost=275, gamma=1.3)
mfsvm2
save(mfsvm2, file="mfsvm2.RData")
load("mfsvm2.RData")
cmsvm2 <- confusionMatrix(validPC2$classe, predict(mfsvm2, validPC2))
cmsvm2

###
### -> 0.975 estimated test accuracy
### 95% confidence interval: 0.968..0.980
###

# SVM actual test accuracy.
cmsvm <- confusionMatrix(testPC$classe, predict(mfsvm2, testPC))
cmsvm

###
### -> 0.970 actual test accuracy
###

### END.