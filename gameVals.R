#Allowable chars for goal sets
goalSet <- "GATC"

#Array to hold goal values
goalArr <- vector()

#Primary goal
goalPrime <- "GATTACCA"

#Move counter for player
moveCount <- 0
revertFlag <- TRUE

#type of move
moveType <- ""
oldMoveType <- ""

#Current/Old Bind sites
currBind <- rep(NA,8)
oldBind <- rep(NA,8)

#Current/Old play sets
oldPlay <- rep(NA,8)
currPlay  <- rep(NA,8)

#Return single random char from goalSet
randGoalChar <- function(){
  tempVal <- round(runif(1, 1, 4))
  return(substr(goalSet, tempVal, tempVal))
}

#Sets up goal array based on user input
createGoal <- function(keyAmount){
  goalArr <<- vector()
  moveCount <<- 0
  for(x in 1:keyAmount){
    if(x == 1){
      goalArr[1] <<- goalPrime
    }else{
      goalArr[x] <<- paste(randGoalChar(), randGoalChar(), randGoalChar(), randGoalChar(), randGoalChar(), randGoalChar(), randGoalChar(), randGoalChar(), collapse = "", sep = "")
    }
  }
}

#get/set for movetype
getMoveType <- function(){
  return(moveType)
}
setMoveType <- function(newMove){
  moveType <<- newMove
}

#Getter/helper func for moveCount
getMoveCount <- function(){
  return(moveCount)
}
addMoveCount <- function(){
  if(revertFlag){
    revertFlag <<- FALSE
  }
  moveCount <<- moveCount + 1
}
subMoveCount <- function(){
  if(!revertFlag){
    moveCount <<- moveCount - 1
    revertFlag <<- TRUE
  }
}
# Getters goal Bind
getGoalArr <- function(){
  return(goalArr)
}


#Current Bind setters
setCurrBind <- function(newBind, bindLoc){
  currBind[bindLoc] <<- newBind
}
setWholeCurrBind <- function(newBindString){
  setCurrBind(substr(newBindString, 1, 1), 1)
  setCurrBind(substr(newBindString, 2, 2), 2)
  setCurrBind(substr(newBindString, 3, 3), 3)
  setCurrBind(substr(newBindString, 4, 4), 4)
  setCurrBind(substr(newBindString, 5, 5), 5)
  setCurrBind(substr(newBindString, 6, 6), 6)
  setCurrBind(substr(newBindString, 7, 7), 7)
  setCurrBind(substr(newBindString, 8, 8), 8)
}
# Current bind Getters
getBind <- function(bindLoc){
  return(currBind[bindLoc])
}
getWholeCurrBind <- function(){
  return(paste(currBind, sep = "", collapse = ""))
}


#Old Bind setters
setOldBind <- function(newBind, bindLoc){
  oldBind[bindLoc] <<- newBind
}
setWholeOldBind <- function(newBindString){
  setOldBind(substr(newBindString, 1, 1), 1)
  setOldBind(substr(newBindString, 2, 2), 2)
  setOldBind(substr(newBindString, 3, 3), 3)
  setOldBind(substr(newBindString, 4, 4), 4)
  setOldBind(substr(newBindString, 5, 5), 5)
  setOldBind(substr(newBindString, 6, 6), 6)
  setOldBind(substr(newBindString, 7, 7), 7)
  setOldBind(substr(newBindString, 8, 8), 8)
}
# Old bind Getters
getOldBind <- function(bindLoc){
  return(oldBind[bindLoc])
}
getWholeOldBind <- function(){
  return(paste(oldBind, sep = "", collapse = ""))
}


# Current play columns setters
setCurrPlay <- function(newPlay, playLoc){
  currPlay[playLoc] <<- newPlay
}
setWholeCurrPlay <- function(newPlayString){
  setCurrPlay(substr(newPlayString, 1, 2), 1)
  setCurrPlay(substr(newPlayString, 3, 4), 2)
  setCurrPlay(substr(newPlayString, 5, 6), 3)
  setCurrPlay(substr(newPlayString, 7, 8), 4)
  setCurrPlay(substr(newPlayString, 9, 10), 5)
  setCurrPlay(substr(newPlayString, 11, 12), 6)
  setCurrPlay(substr(newPlayString, 13, 14), 7)
  setCurrPlay(substr(newPlayString, 15, 16), 8)
}
# Current Play column getters
getCurrPlay <- function(playLoc){
  return(currPlay[playLoc])
}
getWholeCurrPlay <- function(){
  return(paste(currPlay, sep = "", collapse = ""))
}


# Old play columns setters
setOldPlay <- function(currPlay, playLoc){
  oldPlay[playLoc] <<- currPlay
}
setWholeOldPlay <- function(newPlayString){
  setOldPlay(substr(newPlayString, 1, 2), 1)
  setOldPlay(substr(newPlayString, 3, 4), 2)
  setOldPlay(substr(newPlayString, 5, 6), 3)
  setOldPlay(substr(newPlayString, 7, 8), 4)
  setOldPlay(substr(newPlayString, 9, 10), 5)
  setOldPlay(substr(newPlayString, 11, 12), 6)
  setOldPlay(substr(newPlayString, 13, 14), 7)
  setOldPlay(substr(newPlayString, 15, 16), 8)
}
# Old Play column getters
getOldPlay <- function(playLoc){
  return(oldPlay[playLoc])
}
getWholeOldPlay <- function(){
  return(paste(oldPlay, sep = "", collapse = ""))
}
