# MB2 Project (training data distribution)
# Cornelia Zygar, 2582296

###################################################################
# This is a script for assessing the influence of the distribution#
# of training data on the accuracy of a supervised random forest  #
# classification within the RStoolbox superClass() function.      #
###################################################################

# loading packages
library("ggplot2")
library("rgdal")
library("RStoolbox")
library("here")

###################################################################
# Basic idea                                                      #
###################################################################

# superClass() function has parameter nSamples.
# from the documentation of superClass():
# nSamples: "Integer. Number of samples per land cover class."
# "Sample training coordinates. If trainData(and valData if present) are SpatialPolygons-
# DataFrames superClass will calculate the area per polygon and sample nSamples locations 
# per class within these polygons.  The number of samples per individual polygon scales
# with the polygon area, i.e. the bigger the polygon, the more samples."

# Prerequisite: constant number of training pixels (nSamples)
# The more Polygons within the shapefile, the more distributed the training data
# will be.

###################################################################
# functions                                                       #
###################################################################

# function needs to be applied on a list of SpatialPolygonDataframes
# @param trainingDataset list of SpatialPolygonDataframes
# @param img Rasterbrick or Stack
# @param validationData set of validation data (polygons)
# @param numSamples desired number of samples
# @return list containing different accuracy measures
distrAssessment <- function(trainingDataset, img, validationData, numSamples){
  result <- getValidation(
    superClass(
      img, 
      trainingDataset, 
      valData=validationData, 
      responseCol = "class", 
      predict=FALSE, 
      nSamples = numSamples
      ), 
    metrics="caret"
    )
 
  # classwise accuracies
  sensitivity <- result$byClass[,"Sensitivity"]
  specificity <- result$byClass[,"Specificity"]
  
  # overall accuracy
  overallAcc <- result$overall["Accuracy"]

  return (list(sensitivity, specificity, overallAcc))
}


# parameter feature: feature that should be plotted
# @param feature feature that should be plotted ("sensitivity"/ "specificity"/ "accuracy")
# @param resultList List that was returned by the distrAssessment() function
# @return line plot of accuracy values in relation to increasing degree of sampling distribution
plotDistrAssessment <- function(feature, resultList){
  
  # help function to select feature from list
  helpf <- function(x,n){
    return(x[[n]])
  }
  
  # defining list containing number of polygons 
  # hard coded..not optimal
  numList <- list(1,2,4,8,16)
  
  if (feature == "sensitivity"){
    data <- lapply(resultList, helpf, n=1)
    df <- do.call(rbind, Map(data.frame, nPolygons = numList, accuracy = data))
    df$class <- c(rep(1:3, times = length(numList)))
  } else if (feature == "specificity"){
    data <- lapply(resultList, helpf, n=2)
    df <- do.call(rbind, Map(data.frame, nPolygons = numList, accuracy = data))
    df$class <- c(rep(1:3, times = length(numList)))
  } else if (feature == "overall"){
    data <- lapply(resultList, helpf, n=3)
    df <- do.call(rbind, Map(data.frame, nPolygons = numList, accuracy = data))
  } else{
    print("please provide valid feature argument!")
    return (NULL)
  }

  # adjust ggplot depending on selected attribute
  if("class" %in% colnames(df)){
    plot <- ggplot(data = df, aes(x=nPolygons, y=accuracy, group = class, col=factor(class)))+
      geom_line()+
      geom_point(aes(x=nPolygons,y=accuracy))+
      ylab(feature)+
      xlab("number polygons")+
      labs(color="Class")
  } else{
    plot <- ggplot(data = df, aes(x=nPolygons, y=accuracy))+
      geom_line()+
      geom_point(aes(x=nPolygons,y=accuracy))+
      ylab(feature)+
      xlab("number polygons")
  }
  
  return (plot)
}

###################################################################
# loading data                                                    #
###################################################################

# loading List of SpatialPolygonDataframes
# every SpatialPolygonDataframe has the same polygons as the one added before plus the same number of polygons
# added at other locations of the area.
# goal: with increasing number, the samples are increasingly evenly distributed over the image.
trainingList <- list(
  readOGR(here("training_distrAssessment/training_bayern_1_small_1.shp")),
  readOGR(here("training_distrAssessment/training_bayern_1_small_2.shp")),
  readOGR(here("training_distrAssessment/training_bayern_1_small_4.shp")),
  readOGR(here("training_distrAssessment/training_bayern_1_small_8.shp")),
  readOGR(here("training_distrAssessment/training_bayern_1_small_16.shp"))
)

# validation data
valData <- readOGR(here("validation_data/validation_bayern_1_small.shp"))

# the image which will be classified
image <- brick(here("img_data/S2Stack_20190704_bayern_1_small.tif"))

###################################################################
# running the functions                                           #
###################################################################

# applying distrAssessment function over all elements of trainingList
# the higher the numSamples value, the longer the computation will take!
accuracyResult <- lapply(trainingList, distrAssessment, img = image, numSamples=200, validationData=valData)
accuracyResult

# plotting different accuracy measures contained in accuracyResult
# overall accuracy
overallResult <- plotDistrAssessment("overall", accuracyResult)
overallResult

# sensitivity
sensitivityResult <- plotDistrAssessment("sensitivity", accuracyResult)
sensitivityResult

# specificity
specificityResult <- plotDistrAssessment("specificity", accuracyResult)
specificityResult

# wrong argument
wrongArgResult <- plotDistrAssessment("nonExisting", accuracyResult)
wrongArgResult
