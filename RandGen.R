#Available characters to generate
charSet = "ADEFGHIKLMNPQRSTVWY"

#Current win target
goalBond = "GATTACA"

#Current bond sites
currentBond = ""

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

#Returns a newly generated vector, in essence shuffling all values
shuffleAll <- function(){
  playerVector <- createVector()
}

#Shuffles a single character, chosen at random
oneRandomShuffle <- function(oldCol){
  randomPos <- round(runif(1,1,2))
  tempChar <- randomChar()
  
  if(randomPos == 1){
    tempCol <- substr(oldCol, 2, 2)
    tempCol <- paste(tempChar, tempCol, sep="")
  }else{
    tempCol <- substr(oldCol, 1, 1)
    tempCol <- paste(tempCol, tempChar, sep="")
  }
  
  return(tempCol)
}

#Determine bonding site for column
getBondingSite <- function(c1, c2, c3, c4, c5, c6, c7, c8, bondTab){
  
  #String to hold result
  result <- ""
  
  for(x in c(c1,c2,c3,c4,c5,c6,c7,c8)){
    
    #Subselect matrix selection matching sequence, get highest valued column match, randomly choose between ties
    result <- paste(result, colnames(bondTab)[max.col(bondTab[x,], ties.method = "random")], sep = "", collapse = "")
  }
  
  #Save to global variable so it can be referenced by the aDist functionality (making seperate reactive call keeps breaking)
  currentBond <<- result
  
  #Add spacing to the return value
  return(sub("\\s+$", "", gsub('(.{2})', '\\1 ', result)))
}

#get adist for current bonding site string compared to goal string
getAdist <- function(){
  temp <- adist(currentBond, goalBond)
  print(temp)
  return(temp)
}
