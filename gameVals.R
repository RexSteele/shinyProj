#Allowable chars for goal sets
goalSet = "GATC"

#Array to hold goal values
goalArr <- vector()

#Current win target
goalBind = "GATTACCA"

#Move counter for player
moveCount <- 0
revertFlag <- TRUE

#type of move
moveType = ""
oldMoveType = ""

#Current/Old Bind sites
currBind1 = ""
currBind2 = ""
currBind3 = ""
currBind4 = ""
currBind5 = ""
currBind6 = ""
currBind7 = ""
currBind8 = ""
oldBind1 = ""
oldBind2 = ""
oldBind3 = ""
oldBind4 = ""
oldBind5 = ""
oldBind6 = ""
oldBind7 = ""
oldBind8 = ""

#Current/Old play sets
oldPlay1 <- ""
oldPlay2 <- ""
oldPlay3 <- ""
oldPlay4 <- ""
oldPlay5 <- ""
oldPlay6 <- ""
oldPlay7 <- ""
oldPlay8 <- ""
currPlay1 <- ""
currPlay2 <- ""
currPlay3 <- ""
currPlay4 <- ""
currPlay5 <- ""
currPlay6 <- ""
currPlay7 <- ""
currPlay8 <- ""

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
      goalArr[1] <<- "GATTACCA"
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
getGoalBind <- function(){
  return(goalArr)
}

#Current Bind setters
setCurrBind1 <- function(newBind){
  currBind1 <<- newBind
}
setCurrBind2 <- function(newBind){
  currBind2 <<- newBind
}
setCurrBind3 <- function(newBind){
  currBind3 <<- newBind
}
setCurrBind4 <- function(newBind){
  currBind4 <<- newBind
}
setCurrBind5 <- function(newBind){
  currBind5 <<- newBind
}
setCurrBind6 <- function(newBind){
  currBind6 <<- newBind
}
setCurrBind7 <- function(newBind){
  currBind7 <<- newBind
}
setCurrBind8 <- function(newBind){
  currBind8 <<- newBind
}
setWholeCurrBind <- function(newBindString){
  setCurrBind1(substr(newBindString, 1, 1))
  setCurrBind2(substr(newBindString, 2, 2))
  setCurrBind3(substr(newBindString, 3, 3))
  setCurrBind4(substr(newBindString, 4, 4))
  setCurrBind5(substr(newBindString, 5, 5))
  setCurrBind6(substr(newBindString, 6, 6))
  setCurrBind7(substr(newBindString, 7, 7))
  setCurrBind8(substr(newBindString, 8, 8))
}

# Current bind Getters
getCurrBind1 <- function(){
  return(currBind1)
}
getCurrBind2 <- function(){
  return(currBind2)
}
getCurrBind3 <- function(){
  return(currBind3)
}
getCurrBind4 <- function(){
  return(currBind4)
}
getCurrBind5 <- function(){
  return(currBind5)
}
getCurrBind6 <- function(){
  return(currBind6)
}
getCurrBind7 <- function(){
  return(currBind7)
}
getCurrBind8 <- function(){
  return(currBind8)
}
getWholeCurrBind <- function(){
  return(paste(getCurrBind1(),getCurrBind2(), getCurrBind3(), getCurrBind4(), getCurrBind5(), getCurrBind6(), getCurrBind7(), getCurrBind8(), sep = "", collapse = ""))
}





#Old Bind setters
setOldBind1 <- function(newBind){
  oldBind1 <<- newBind
}
setOldBind2 <- function(newBind){
  oldBind2 <<- newBind
}
setOldBind3 <- function(newBind){
  oldBind3 <<- newBind
}
setOldBind4 <- function(newBind){
  oldBind4 <<- newBind
}
setOldBind5 <- function(newBind){
  oldBind5 <<- newBind
}
setOldBind6 <- function(newBind){
  oldBind6 <<- newBind
}
setOldBind7 <- function(newBind){
  oldBind7 <<- newBind
}
setOldBind8 <- function(newBind){
  oldBind8 <<- newBind
}
setWholeOldBind <- function(newBindString){
  setOldBind1(substr(newBindString, 1, 1))
  setOldBind2(substr(newBindString, 2, 2))
  setOldBind3(substr(newBindString, 3, 3))
  setOldBind4(substr(newBindString, 4, 4))
  setOldBind5(substr(newBindString, 5, 5))
  setOldBind6(substr(newBindString, 6, 6))
  setOldBind7(substr(newBindString, 7, 7))
  setOldBind8(substr(newBindString, 8, 8))
}

# Old bind Getters
getOldBind1 <- function(){
  return(oldBind1)
}
getOldBind2 <- function(){
  return(oldBind2)
}
getOldBind3 <- function(){
  return(oldBind3)
}
getOldBind4 <- function(){
  return(oldBind4)
}
getOldBind5 <- function(){
  return(oldBind5)
}
getOldBind6 <- function(){
  return(oldBind6)
}
getOldBind7 <- function(){
  return(oldBind7)
}
getOldBind8 <- function(){
  return(oldBind8)
}
getWholeOldBind <- function(){
  return(paste(getOldBind1(),getOldBind2(), getOldBind3(), getOldBind4(), getOldBind5(), getOldBind6(), getOldBind7(), getOldBind8(), sep = "", collapse = ""))
}




# Current Play column getters
getCurrPlay1 <- function(){
  return(currPlay1)
}
getCurrPlay2 <- function(){
  return(currPlay2)
}
getCurrPlay3 <- function(){
  return(currPlay3)
}
getCurrPlay4 <- function(){
  return(currPlay4)
}
getCurrPlay5 <- function(){
  return(currPlay5)
}
getCurrPlay6 <- function(){
  return(currPlay6)
}
getCurrPlay7 <- function(){
  return(currPlay7)
}
getCurrPlay8 <- function(){
  return(currPlay8)
}
getWholeCurrPlay <- function(){
  return(paste(getCurrPlay1(),getCurrPlay2(), getCurrPlay3(), getCurrPlay4(), getCurrPlay5(), getCurrPlay6(), getCurrPlay7(), getCurrPlay8(), sep = "", collapse = ""))
}

# Current play columns setters
setCurrPlay1 <- function(newPlay){
  currPlay1 <<- newPlay
}
setCurrPlay2 <- function(newPlay){
  currPlay2 <<- newPlay
}
setCurrPlay3 <- function(newPlay){
  currPlay3 <<- newPlay
}
setCurrPlay4 <- function(newPlay){
  currPlay4 <<- newPlay
}
setCurrPlay5 <- function(newPlay){
  currPlay5 <<- newPlay
}
setCurrPlay6 <- function(newPlay){
  currPlay6 <<- newPlay
}
setCurrPlay7 <- function(newPlay){
  currPlay7 <<- newPlay
}
setCurrPlay8 <- function(newPlay){
  currPlay8 <<- newPlay
}
setWholeCurrPlay <- function(newPlayString){
  setCurrPlay1(substr(newPlayString, 1, 2))
  setCurrPlay2(substr(newPlayString, 3, 4))
  setCurrPlay3(substr(newPlayString, 5, 6))
  setCurrPlay4(substr(newPlayString, 7, 8))
  setCurrPlay5(substr(newPlayString, 9, 10))
  setCurrPlay6(substr(newPlayString, 11, 12))
  setCurrPlay7(substr(newPlayString, 13, 14))
  setCurrPlay8(substr(newPlayString, 15, 16))
}



# Old Play column getters
getOldPlay1 <- function(){
  return(oldPlay1)
}
getOldPlay2 <- function(){
  return(oldPlay2)
}
getOldPlay3 <- function(){
  return(oldPlay3)
}
getOldPlay4 <- function(){
  return(oldPlay4)
}
getOldPlay5 <- function(){
  return(oldPlay5)
}
getOldPlay6 <- function(){
  return(oldPlay6)
}
getOldPlay7 <- function(){
  return(oldPlay7)
}
getOldPlay8 <- function(){
  return(oldPlay8)
}
getWholeOldPlay <- function(){
  return(paste(getOldPlay1(),getOldPlay2(), getOldPlay3(), getOldPlay4(), getOldPlay5(), getOldPlay6(), getOldPlay7(), getOldPlay8(), sep = "", collapse = ""))
}

# Old play columns setters
setOldPlay1 <- function(currPlay){
  oldPlay1 <<- currPlay
}
setOldPlay2 <- function(currPlay){
  oldPlay2 <<- currPlay
}
setOldPlay3 <- function(currPlay){
  oldPlay3 <<- currPlay
}
setOldPlay4 <- function(currPlay){
  oldPlay4 <<- currPlay
}
setOldPlay5 <- function(currPlay){
  oldPlay5 <<- currPlay
}
setOldPlay6 <- function(currPlay){
  oldPlay6 <<- currPlay
}
setOldPlay7 <- function(currPlay){
  oldPlay7 <<- currPlay
}
setOldPlay8 <- function(currPlay){
  oldPlay8 <<- currPlay
}
setWholeOldPlay <- function(newPlayString){
  setOldPlay1(substr(newPlayString, 1, 2))
  setOldPlay2(substr(newPlayString, 3, 4))
  setOldPlay3(substr(newPlayString, 5, 6))
  setOldPlay4(substr(newPlayString, 7, 8))
  setOldPlay5(substr(newPlayString, 9, 10))
  setOldPlay6(substr(newPlayString, 11, 12))
  setOldPlay7(substr(newPlayString, 13, 14))
  setOldPlay8(substr(newPlayString, 15, 16))
}
