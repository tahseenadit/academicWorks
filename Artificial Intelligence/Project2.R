# File:         Project2.r 
# Description:  solution for Project 2, Artificial Intelligence 2019, UU
# Author:       Md Tahseen Anam

# Install the package
# install.packages("WheresCroc_1.2.2.tar.gz", repos = NULL, type="source")

# Load the library
library("WheresCroc")

getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}

myFunction <- function(moveInfo, readings, positions, edges, probs) {
  
  # Get total number of waterholes in the system.
  numberOfWaterholes = nrow(probs$salinity)
  
  if("visitedWaterholes" %in% names(moveInfo$mem)){
    visitedWaterholes = moveInfo$mem$visitedWaterholes
    visitedWaterholes[moveInfo$moves[1]] = 1
    #visitedWaterholes[moveInfo$moves[2]] = 1
  }else{
    moveInfo$mem$visitedWaterholes = matrix(0, nrow = numberOfWaterholes, ncol = 1)
    visitedWaterholes = moveInfo$mem$visitedWaterholes
  }
  
  if("probOfWaterhole" %in% names(moveInfo$mem)){
    probOfWaterhole = moveInfo$mem$probOfWaterhole
  }else{
    moveInfo$mem$probOfWaterhole = matrix(1, nrow = numberOfWaterholes,ncol = 2)
    probOfWaterhole = moveInfo$mem$probOfWaterhole
  }
  
  if("prevProbOfWaterhole" %in% names(moveInfo$mem)){
    prevProbOfWaterhole = moveInfo$mem$probOfWaterhole
  }else{
    moveInfo$mem$prevProbOfWaterhole = matrix(1, nrow = numberOfWaterholes,ncol = 2)
    prevProbOfWaterhole = moveInfo$mem$prevProbOfWaterhole
  }
  
  if("transitionMatrixforCrocInPrevState" %in% names(moveInfo$mem)){
    transitionMatrixforCrocInPrevState = moveInfo$mem$transitionMatrixforCrocInPrevState
  }else{
    moveInfo$mem$transitionMatrixforCrocInPrevState = matrix(0, nrow = numberOfWaterholes, ncol = 2)
    transitionMatrixforCrocInPrevState = moveInfo$mem$transitionMatrixforCrocInPrevState
  }
  
  if("transitionMatrixforNoCrocInPrevState" %in% names(moveInfo$mem)){
    transitionMatrixforNoCrocInPrevState = moveInfo$mem$transitionMatrixforNoCrocInPrevState
  }else{
    moveInfo$mem$transitionMatrixforNoCrocInPrevState = matrix(0, nrow = numberOfWaterholes, ncol = 2)
    transitionMatrixforNoCrocInPrevState = moveInfo$mem$transitionMatrixforNoCrocInPrevState
  }
  
  # So, now we have our transition matrix for our neighbours. 
  if("emissionMatrix" %in% names(moveInfo$mem)){
    emissionMatrix = moveInfo$mem$emissionMatrix
  }else{
    moveInfo$mem$emissionMatrix = matrix(0, nrow = numberOfWaterholes, ncol = 2)
    emissionMatrix = moveInfo$mem$emissionMatrix
  }
  
  if("initial" %in% names(moveInfo$mem)){
    
    if (!is.na(positions[1]) && positions[1]<0){ 
      
      waterholeNumber = positions[1] * -1
      probOfWaterhole[positions[1],1] = 1
      probOfWaterhole[positions[1],2] = 0
      
      for(k in 1:numberOfWaterholes){
        
        if(k!=positions[1]){
          probOfWaterhole[positions[1],1] = 0
          probOfWaterhole[positions[1],2] = 1
        }
        
      }
      
    }else if (!is.na(positions[2]) && positions[2]<0){ 
      
      waterholeNumber = positions[2] * -1
      probOfWaterhole[positions[2],1] = 1
      probOfWaterhole[positions[2],2] = 0
      
      for(k in 1:numberOfWaterholes){
        
        if(k!=positions[2]){
          probOfWaterhole[positions[2],1] = 0
          probOfWaterhole[positions[2],2] = 1
        }
        
      }
      
    }else{
      
      for (i in 1:numberOfWaterholes) {
        
        if(visitedWaterholes[i] == 1 && probOfWaterhole[,1]!=0){
          
          probOfWaterhole[i,1] = 0
          probOfWaterhole[i,2] = 1
          
        }else if (!is.na(positions[1]) && i==positions[1]){ 
          
          probOfWaterhole[i,1] = 0
          probOfWaterhole[i,2] = 1
          
        }else if (!is.na(positions[2]) && i==positions[2]) {
          
          probOfWaterhole[i,1] = 0
          probOfWaterhole[i,2] = 1
          
        }else {
          
          probSalinity <- pnorm(readings[1]+0.5,probs$salinity[i,1],probs$salinity[i,2]) - pnorm(readings[1]-0.5,probs$salinity[i,1],probs$salinity[i,2])
          probPhosphate <- pnorm(readings[2]+0.5,probs$phosphate[i,1],probs$phosphate[i,2]) - pnorm(readings[2]-0.5,probs$phosphate[i,1],probs$phosphate[i,2])
          probNitrogen <- pnorm(readings[3]+0.5,probs$nitrogen[i,1],probs$nitrogen[i,2]) - pnorm(readings[3]-0.5,probs$nitrogen[i,1],probs$nitrogen[i,2])
          
          probability <- probSalinity*probPhosphate*probNitrogen
          
          emissionMatrix[i, 1] = probability
          emissionMatrix[i, 2] = 0.7 - probability
          
          neighbourWaterholes <- getOptions(i,edges)
          prob = 0
          for (j in 1:length(neighbourWaterholes)) {
            prob = prob + (1.0/length(getOptions(neighbourWaterholes[j],edges)))*prevProbOfWaterhole[neighbourWaterholes[j], 1]
            prob = prob + (1.0/length(getOptions(neighbourWaterholes[j],edges)))*prevProbOfWaterhole[neighbourWaterholes[j], 2]*0.33
          }
          
          probOfWaterhole[i, 1] = prob*emissionMatrix[i, 1]
          probOfWaterhole[i, 2] = 1 - probOfWaterhole[i, 1]
        }
      }
      totalProb = sum(probOfWaterhole[,1])
      probOfWaterhole[,1] = sapply(probOfWaterhole[,1], function(x) x/totalProb)
      totalProb = sum(probOfWaterhole[,2])
      probOfWaterhole[,2] = sapply(probOfWaterhole[,2], function(x) x/totalProb)
    }
    
  }else{
    moveInfo$mem$wronggoal = 0
    tourists = 0
    numberOfPossibleWaterholesForCroc = 40
    
    if(!is.na(positions[1]) && positions[1] > 0){
      tourists = tourists + 1
      if(!is.na(positions[2]) && positions[2] > 0){
        tourists = tourists + 1
        if(positions[1]==positions[2]){
          tourists = 1
        }
      }
    }else{
      if(!is.na(positions[2]) && positions[2] > 0){
        tourists = tourists + 1
      }
    }
    
    for (i in 1:numberOfWaterholes) {
      
      if (!is.na(positions[1]) && i==positions[1]){ 
        
        if (positions[1]<0) {
          
          probOfWaterhole[positions[1],1] = 1
          probOfWaterhole[positions[1],2] = 0
          
          for(k in 1:numberOfWaterholes){
            
            if(k!=positions[1]){
              probOfWaterhole[positions[1],1] = 0
              probOfWaterhole[positions[1],2] = 1
            }
            
          }
          break
        }else{
          probOfWaterhole[i,1] = 0
          probOfWaterhole[i,2] = 1
          #tourists = tourists + 1
        }
        
      }else if (!is.na(positions[2]) && i==positions[2]) {
        
        if (positions[2]<0) {
          
          probOfWaterhole[positions[2],1] = 1
          probOfWaterhole[positions[2],2] = 0
          
          for(k in 1:numberOfWaterholes){
            
            if(k!=positions[2]){
              probOfWaterhole[positions[2],1] = 0
              probOfWaterhole[positions[2],2] = 1
            }
            
          }
          break
        }else{
          probOfWaterhole[i,1] = 0
          probOfWaterhole[i,2] = 1
          #tourists = tourists + 1
        }
        
      }else {
        
        numberOfPossibleWaterholesForCroc = numberOfWaterholes - tourists
        # Calculate the probability.
        probOfCroc = 1/numberOfPossibleWaterholesForCroc
        probOfWaterhole[i,1] = probOfCroc
        probOfWaterhole[i,2] = 0.7 - probOfCroc
        
      }
    }
    
    prevProbOfWaterhole = probOfWaterhole
    
    for (i in 1:numberOfWaterholes) {
      
      probSalinity <- pnorm(readings[1]+0.5,probs$salinity[i,1],probs$salinity[i,2]) - pnorm(readings[1]-0.5,probs$salinity[i,1],probs$salinity[i,2])
      probPhosphate <- pnorm(readings[2]+0.5,probs$phosphate[i,1],probs$phosphate[i,2]) - pnorm(readings[2]-0.5,probs$phosphate[i,1],probs$phosphate[i,2])
      probNitrogen <- pnorm(readings[3]+0.5,probs$nitrogen[i,1],probs$nitrogen[i,2]) - pnorm(readings[3]-0.5,probs$nitrogen[i,1],probs$nitrogen[i,2])
      
      probability <- probSalinity*probPhosphate*probNitrogen
      
      emissionMatrix[i, 1] = probability
      emissionMatrix[i, 2] = 0.7 - probability
      
      neighbourWaterholes <- getOptions(i,edges)
      prob = 0
      for (j in 1:length(neighbourWaterholes)) {
        prob = prob + (1.0/length(getOptions(neighbourWaterholes[j],edges)))*prevProbOfWaterhole[neighbourWaterholes[j], 1]
        prob = prob + (1.0/length(getOptions(neighbourWaterholes[j],edges)))*prevProbOfWaterhole[neighbourWaterholes[j], 2]*0.33
     }
      
      probOfWaterhole[i, 1] = prob
      probOfWaterhole[i, 2] = 1 - probOfWaterhole[i, 1]
      
    }
    
    totalProb = sum(probOfWaterhole[,1])
    probOfWaterhole[,1] = sapply(probOfWaterhole[,1], function(x) x/totalProb)
    totalProb = sum(probOfWaterhole[,2])
    probOfWaterhole[,2] = sapply(probOfWaterhole[,2], function(x) x/totalProb)
  }
  
  maxNodeTrue = which.max(probOfWaterhole[,1])
  maxNode = which.max(probOfWaterhole[,1])
  neighbourWaterholes <- getOptions(moveInfo$mem$maxNode,edges)
  
  if(!is.na(positions[1]) && positions[1] < 0){
    maxNode = positions[1]*-1
  }
  else if (!is.na(positions[2]) && positions[2] < 0){
    maxNode = positions[2]*-1
  }
  
  if(maxNode %in% neighbourWaterholes){
    
  }else if(moveInfo$mem$wronggoal > 2){
    tourists = 0
    numberOfPossibleWaterholesForCroc = 40
    
    if(!is.na(positions[1]) && positions[1] > 0){
      tourists = tourists + 1
      if(!is.na(positions[2]) && positions[2] > 0){
        tourists = tourists + 1
        if(positions[1]==positions[2]){
          tourists = 1
        }
      }
    }else{
      if(!is.na(positions[2]) && positions[2] > 0){
        tourists = tourists + 1
      }
    }
    
    for (i in 1:numberOfWaterholes) {
      
      if (!is.na(positions[1]) && i==positions[1]){ 
        
        if (positions[1]<0) {
          
          probOfWaterhole[positions[1],1] = 1
          probOfWaterhole[positions[1],2] = 0
          
          for(k in 1:numberOfWaterholes){
            
            if(k!=positions[1]){
              probOfWaterhole[positions[1],1] = 0
              probOfWaterhole[positions[1],2] = 1
            }
            
          }
          break
        }else{
          probOfWaterhole[i,1] = 0
          probOfWaterhole[i,2] = 1
          #tourists = tourists + 1
        }
        
      }else if (!is.na(positions[2]) && i==positions[2]) {
        
        if (positions[2]<0) {
          
          probOfWaterhole[positions[2],1] = 1
          probOfWaterhole[positions[2],2] = 0
          
          for(k in 1:numberOfWaterholes){
            
            if(k!=positions[2]){
              probOfWaterhole[positions[2],1] = 0
              probOfWaterhole[positions[2],2] = 1
            }
            
          }
          break
        }else{
          probOfWaterhole[i,1] = 0
          probOfWaterhole[i,2] = 1
          #tourists = tourists + 1
        }
        
      }else {
        
        numberOfPossibleWaterholesForCroc = numberOfWaterholes - tourists
        # Calculate the probability.
        probOfCroc = 1/numberOfPossibleWaterholesForCroc
        probOfWaterhole[i,1] = probOfCroc
        probOfWaterhole[i,2] = 0.7 - probOfCroc
        
      }
    }
    
    for (i in 1:numberOfWaterholes) {
      
      probSalinity <- pnorm(readings[1]+0.5,probs$salinity[i,1],probs$salinity[i,2]) - pnorm(readings[1]-0.5,probs$salinity[i,1],probs$salinity[i,2])
      probPhosphate <- pnorm(readings[2]+0.5,probs$phosphate[i,1],probs$phosphate[i,2]) - pnorm(readings[2]-0.5,probs$phosphate[i,1],probs$phosphate[i,2])
      probNitrogen <- pnorm(readings[3]+0.5,probs$nitrogen[i,1],probs$nitrogen[i,2]) - pnorm(readings[3]-0.5,probs$nitrogen[i,1],probs$nitrogen[i,2])
      
      probability <- probSalinity*probPhosphate*probNitrogen
      
      emissionMatrix[i, 1] = probability
      emissionMatrix[i, 2] = 0.7 - probability
      
      neighbourWaterholes <- getOptions(i,edges)
      prob = 0
      for (j in 1:length(neighbourWaterholes)) {
        prob = prob + (1.0/length(getOptions(neighbourWaterholes[j],edges)))*prevProbOfWaterhole[neighbourWaterholes[j], 1]
        prob = prob + (1.0/length(getOptions(neighbourWaterholes[j],edges)))*prevProbOfWaterhole[neighbourWaterholes[j], 2]*0.33
      }
      
      probOfWaterhole[i, 1] = prob*emissionMatrix[i, 1]
      probOfWaterhole[i, 2] = 1 - probOfWaterhole[i, 1]
      
    }
    
    totalProb = sum(probOfWaterhole[,1])
    probOfWaterhole[,1] = sapply(probOfWaterhole[,1], function(x) x/totalProb)
    totalProb = sum(probOfWaterhole[,2])
    probOfWaterhole[,2] = sapply(probOfWaterhole[,2], function(x) x/totalProb)
    maxNode = which.max(probOfWaterhole[,1])
  }else{
    moveInfo$mem$wronggoal = moveInfo$mem$wronggoal + 1
  }
  
  moveInfo$mem$maxNode = maxNode
  
  currentNode = positions[3]
  prevNode = matrix(0, nrow = 40, ncol = 1)
  traversed = matrix(0, nrow = 40, ncol = 1)
  queue = matrix(0, nrow = 40, ncol = 1)
  queue[1] = positions[3]
  k = 2
  j = 1
  foundMax = 0
  
  
  while(foundMax == 0){
    currentNode = queue[j]
    neighbourWaterholes <- getOptions(queue[j],edges)
    j = j+1
    for (i in 1:length(neighbourWaterholes)) {
      
      if(neighbourWaterholes[i] != currentNode){
        if(abs(maxNode - neighbourWaterholes[i]) == 0){
          traversed[neighbourWaterholes[i]] = 1
          prevNode[neighbourWaterholes[i]] = currentNode
          queue[k] = neighbourWaterholes[i]
          k = k+1
          foundMax = 1
          break
        }
        
        if(traversed[neighbourWaterholes[i]] == 0){
          traversed[neighbourWaterholes[i]] = 1
          prevNode[neighbourWaterholes[i]] = currentNode
          queue[k] = neighbourWaterholes[i]
          k = k+1
        }
      }
    }
  }
  
  path = matrix(nrow = 40,ncol = 1)
  i = 1
  node = maxNode
  path[i] = maxNode
  i = i+1
  while(1){
    path[i] = prevNode[node]
    node = prevNode[node]
    if(node == positions[3])
      break
    i = i+1
  }
  
  i = i-1
  move1 = path[i]
  i = i-1
  move2 = path[i]
  
  if(positions[3] == maxNode){
    move1 = 0
    move2 = 0
  }else if(move1 == maxNode){
    move2 = 0
  }
  
  moveInfo$moves[1] = move1
  moveInfo$moves[2] = move2
  
  
  moveInfo$mem$visitedWaterholes = visitedWaterholes
  moveInfo$mem$probOfWaterhole = probOfWaterhole
  moveInfo$mem$transitionMatrixforCrocInPrevState = transitionMatrixforCrocInPrevState
  moveInfo$mem$transitionMatrixforNoCrocInPrevState = transitionMatrixforNoCrocInPrevState
  moveInfo$mem$emissionMatrix = emissionMatrix 
  
  moveInfo$mem$initial = TRUE
  
  return(moveInfo)
}
