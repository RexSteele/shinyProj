source("gameVals.R")

#Available characters to generate
charSet = "ADEFGHIKLMNPQRSTVWY"

#Return single random char from charSet
randomChar <- function(){
  tempVal <- round(runif(1, 1, 19))
  return(substr(charSet, tempVal, tempVal))
}

#Returns vector of random chars chosen from allowable char set
createColumn <- function(){
  tempVec <- vector(mode= "list", length = "2")
  for(i in 0:length(tempVec)){
    tempVec[i] <- randomChar()
  }
  return(paste(tempVec, collapse = ""))
}

#Shuffles a single character, chosen at random
oneRandomShuffle <- function(oldCol){
  randomPos <- round(runif(1,1,2))
  tempChar <- randomChar()

  if(randomPos == 1){
    tempCol <- substr(oldCol, 2, 2)
    tempCol <- paste("<span style=\"color:red\">", tempChar, "</span>", tempCol, sep="")
  }else{
    tempCol <- substr(oldCol, 1, 1)
    tempCol <- paste(tempCol, "<span style=\"color:red\">", tempChar, "</span>", sep="")
  }

  return(tempCol)
}

#Determine binding site for column
calcBindingSite <- function(currentCol, bindTab){

  #String to hold result
  result <- ""

  #Subselect matrix selection matching sequence, get highest valued column match, randomly choose between ties
  result <- paste(result, colnames(bindTab)[max.col(bindTab[currentCol,], ties.method = "random")], sep = "", collapse = "")

  return(result)
}

#get adist for current binding site string compared to goal string
calcAdist <- function(goalArrPos){
  return(adist(getWholeCurrBind(), getGoalBind()[goalArrPos]))
}

#check provided string against goal
checkGoal <- function(){
  x <- length(getGoalBind())
  for(i in 1:x){
    if(identical(getWholeCurrBind(), getGoalBind()[i])){
      return(getGoalBind()[i])
    }
  }

  return(FALSE)
}
