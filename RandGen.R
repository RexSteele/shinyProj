

charSet = "ADEFGHIKLMNPQRSTVWXYZ"

getPlayer <- function(){
  return(playerVector)
}

getFinalGoal <- function(){
  return(finalVector)
}

randomChar <- function(){
  tempVal <- round(runif(1, 1, 21))
  return(substr(charSet, tempVal, tempVal))
}

#Returns vector of random chars chosen from allowable char set
createVector <- function(){
  tempVec <- vector(mode= "list", length = "15")
  for(i in 0:length(tempVec) + 1){
    tempVec[i] <- randomChar()
  }
  return(tempVec)
}

#Returns vector of random chars chosen from allowable char set
createColumn <- function(){
  tempVec <- vector(mode= "list", length = "2")
  for(i in 0:length(tempVec) + 1){
    tempVec[i] <- randomChar()
  }
  
  return(paste(tempVec, collapse = ""))
}

#Returns a newly generated vector, in essence shuffling all values
shuffleAll <- function(){
  playerVector <- createVector()
}

#Shuffles a single character, chosen at random
shuffleOneRandom <- function(oldVector){
  tempShuffle <- oldVector
  value <- round(runif(1,1,16))
  tempChar <- randomChar()
  tempShuffle[value] <- tempChar
  return(tempShuffle)
}

#Shuffles a single character, based on marker
shuffleOneSelect <- function(oldVector, marker){
  tempShuffle <- oldVector
  tempShuffle[marker] <- randomChar()
  return(tempShuffle)
}

playerVector <- createVector()

finalVector <- createVector()
