# File:         demo.r 
# Description:  Naive demo-solution given at classroom session
#               for Project 1, Artificial Intelligence 2019, UU
# Author:       Fredrik Nilsson

# Install the package
# install.packages("DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")

# Load the library
library("DeliveryMan")
source_node = matrix(1,nrow=2)
# Matrix to keep track of g_cost
g_cost = matrix(nrow = 10,ncol = 10)
# Matrix to keep track of h_cost
h_cost = matrix(nrow = 10,ncol = 10)
# Matrix to keep track of f_cost
f_values = matrix(Inf,nrow = 10,ncol = 10)
# Matrix to keep track of visited nodes.
visited = matrix(0,nrow = 10,ncol = 10)
minimum_f = matrix(1,nrow=2)
load = 0
# Read documentation
# ?runDeliveryMan
# ?testDM

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  
  # initialize g_cost of the first node that is to be expanded.
  if("g_cost" %in% names(carInfo$mem)){
    carInfo$mem$g_cost = g_cost
  }else{
    carInfo$mem$g_cost = matrix(nrow = 10,ncol = 10)
    # Setting the cost of the first node that is to be expanded to 0
    carInfo$mem$g_cost[carInfo$x,carInfo$y] = 0
  }

  if("h_cost" %in% names(carInfo$mem)){
    carInfo$mem$h_cost = h_cost
  }else{
    carInfo$mem$h_cost = matrix(nrow = 10,ncol = 10)
  }
  
  if("f_values" %in% names(carInfo$mem)){
    carInfo$mem$f_values = f_values
    carInfo$mem$f_values[carInfo$x,carInfo$y] = carInfo$mem$g_cost[carInfo$x,carInfo$y] + carInfo$mem$h_cost[carInfo$x,carInfo$y]
  }else{
    carInfo$mem$f_values = matrix(Inf,nrow = 10,ncol = 10)
  }
  
  # marking the current node to be expanded.
  if("visited" %in% names(carInfo$mem)){
    carInfo$mem$visited = visited
  }else{
    carInfo$mem$visited = matrix(0,nrow = 10,ncol = 10)
  }
  
  # What is our goal?
  if(carInfo$load == 0) {
    
    carInfo$mem$goal <- nextPickup(trafficMatrix, 
                                   carInfo, 
                                   packageMatrix)
    if(load == 0){
      
      # setting parent node.
      source_node[1] <<- carInfo$x
      source_node[2] <<- carInfo$y
  
      carInfo$mem$g_cost = matrix(nrow = 10,ncol = 10)
      # Setting the cost of the first node that is to be expanded to 0
      carInfo$mem$g_cost[carInfo$x,carInfo$y] = 0
      
      carInfo$mem$g_cost = matrix(nrow = 10,ncol = 10)
      # Setting the cost of the first node that is to be expanded to 0
      carInfo$mem$g_cost[carInfo$x,carInfo$y] = 0
      
      carInfo$mem$visited = matrix(0,nrow = 10,ncol = 10)
      
      carInfo$mem$h_cost = matrix(nrow = 10,ncol = 10)
      # Setting the h cost of the first node that is to be expanded
      carInfo$mem$h_cost[carInfo$x,carInfo$y] = abs((carInfo$mem$goal[1] - carInfo$x)) + abs((carInfo$mem$goal[2] - carInfo$y))
      
      carInfo$mem$f_values = matrix(Inf,nrow = 10,ncol = 10)
      # Setting the f cost of the first node that is to be expanded
      carInfo$mem$f_values[carInfo$x,carInfo$y] = carInfo$mem$g_cost[carInfo$x,carInfo$y] + carInfo$mem$h_cost[carInfo$x,carInfo$y]
      
      load <<- 1
    }
  } else {
    
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
    
    if(load == 1){
      
      # setting parent node.
      source_node[1] <<- carInfo$x
      source_node[2] <<- carInfo$y
  
      carInfo$mem$visited = matrix(0,nrow = 10,ncol = 10)
      
      carInfo$mem$h_cost = matrix(nrow = 10,ncol = 10)
      # Setting the h cost of the first node that is to be expanded
      carInfo$mem$h_cost[carInfo$x,carInfo$y] = abs((carInfo$mem$goal[1] - carInfo$x)) + abs((carInfo$mem$goal[2] - carInfo$y))
      
      carInfo$mem$f_values = matrix(Inf,nrow = 10,ncol = 10)
      # Setting the f cost of the first node that is to be expanded
      carInfo$mem$f_values[carInfo$x,carInfo$y] = carInfo$mem$g_cost[carInfo$x,carInfo$y] + carInfo$mem$h_cost[carInfo$x,carInfo$y]
      
      load <<- 0
    }
  }
  
  # How do we get there?
  carInfo$nextMove <- nextMove(trafficMatrix,
                               carInfo,
                               packageMatrix)
  return(carInfo)
}

# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector = abs((packageMatrix[,1] - carInfo$x)) + abs((packageMatrix[,2] - carInfo$y))
  distanceVector[packageMatrix[,5] != 0] = Inf
  return(packageMatrix[which.min(distanceVector), c(1,2)])
}

# Find the move to get to carInfo$mem$goal
nextMove <- function(trafficMatrix, carInfo, packageMatrix) {
  
  if(carInfo$x == carInfo$mem$goal[1] && carInfo$y == carInfo$mem$goal[2]){
    g_cost <<- carInfo$mem$g_cost
    h_cost <<- carInfo$mem$h_cost
    visited <<- carInfo$mem$visited
    f_values <<- carInfo$mem$f_values
    # setting parent node.
    source_node[1] <<- carInfo$x
    source_node[2] <<- carInfo$y
    return(5)
  }
  
  currentx = carInfo$x
  currenty = carInfo$y
  
  neighbour_right = currentx+1
  neighbour_left = currentx-1
  neighbour_up = currenty+1
  neighbour_down = currenty-1
  
  r_f_value = 0
  l_f_value = 0
  u_f_value = 0
  d_f_value = 0
  
  f_vector = double()
  
  if(neighbour_right != 11 ){
    # Checking if we have already visited the neighbor node. If already visited, then we should not visit the neighbor node again.
    if(carInfo$mem$visited[neighbour_right,currenty] == 0){
      if(neighbour_right == 10){
        # Setting g cost of right neighbor which is the g cost to the expanded node plus the edge cost to the right neighbor. If there is no neighbor right than the edge cost is 0.
        r_g_value = carInfo$mem$g_cost[currentx,currenty] + 0
        # Setting h cost of right neighbor which is the manhattan distance between the neighbor and the goal node.
        r_h_value = max(trafficMatrix$hroads)*(abs((carInfo$mem$goal[1] - neighbour_right)) + abs((carInfo$mem$goal[2] - currenty)))
      }else{
        # Setting g cost of right neighbor which is the g cost to the expanded node plus the edge cost to the right neighbor
        r_g_value = trafficMatrix$hroads[neighbour_right,currenty] + carInfo$mem$g_cost[currentx,currenty]
        # Setting h cost of the right neighbor which is the manhattan distance between the right neighbor and the goal node.
        r_h_value = max(trafficMatrix$hroads)*(abs((carInfo$mem$goal[1] - neighbour_right)) + abs((carInfo$mem$goal[2] - currenty)))
      }
      
      # storing g cost of the right neighbor.
      carInfo$mem$g_cost[neighbour_right,currenty] = r_g_value
      # storing h cost of the right neighbor.
      carInfo$mem$h_cost[neighbour_right,currenty] = r_h_value
      # setting f value which is g cost plus heuristic
      r_f_value = r_g_value + r_h_value
      # storing f value of the right neighbor.
      carInfo$mem$f_values[neighbour_right,currenty] = r_f_value
      if(source_node[1] == neighbour_right && source_node[2] == currenty){
        #we dont need to add parent node to the neighbour because we dont want to go back.
        r_f_value = 0
      }else{
        f_vector = c(f_vector,r_f_value)
      }
    }
  }
  
  if(neighbour_left != 0){
    # Checking if we have already visited the neighbor node. If already visited, then we should not visit the neighbor node again.
    if(carInfo$mem$visited[neighbour_left,currenty] == 0){
      
      # Setting g cost of left neighbor which is the g cost to the expanded node plus the edge cost to the left neighbor
      l_g_value = trafficMatrix$hroads[neighbour_left,currenty] + carInfo$mem$g_cost[currentx,currenty]
      # Setting h cost of the left neighbor which is the manhattan distance between the left neighbor and the goal node.
      l_h_value = max(trafficMatrix$hroads)*(abs((carInfo$mem$goal[1] - neighbour_left)) + abs((carInfo$mem$goal[2] - currenty)))
      
      # storing g cost of the left neighbor.
      carInfo$mem$g_cost[neighbour_left,currenty] = l_g_value
      # storing h cost of the left neighbor.
      carInfo$mem$h_cost[neighbour_left,currenty] = l_h_value
      # setting f value which is g cost plus heuristic
      l_f_value = l_g_value + l_h_value
      # storing f value of the left neighbor.
      carInfo$mem$f_values[neighbour_left,currenty] = l_f_value
      if(source_node[1] == neighbour_left && source_node[2] == currenty){
        #we dont need to add parent node to the neighbour because we dont want to go back.
        l_f_value = 0
      }else{
        f_vector = c(f_vector,l_f_value)
      }
    }
  }
  
  if(neighbour_up != 11){
    if(carInfo$mem$visited[currentx,neighbour_up] == 0){
      if(neighbour_up == 10){
        # Setting g cost of up neighbor which is the g cost to the expanded node plus the edge cost to the up neighbor. If there is no neighbor up than the edge cost is 0.
        u_g_value = carInfo$mem$g_cost[currentx,currenty] + 0
        # Setting h cost of up neighbor which is the manhattan distance between the neighbor and the goal node.
        u_h_value = max(trafficMatrix$hroads)*(abs((carInfo$mem$goal[1] - currentx)) + abs((carInfo$mem$goal[2] - neighbour_up)))
      }else{
        # Setting g cost of up neighbor which is the g cost to the expanded node plus the edge cost to the up neighbor
        u_g_value = trafficMatrix$vroads[currentx,neighbour_up] + carInfo$mem$g_cost[currentx,currenty]
        # Setting h cost of the up neighbor which is the manhattan distance between the up neighbor and the goal node.
        u_h_value = max(trafficMatrix$hroads)*(abs((carInfo$mem$goal[1] - currentx)) + abs((carInfo$mem$goal[2] - neighbour_up)))
      }
      
      # storing g cost of the up neighbor.
      carInfo$mem$g_cost[currentx,neighbour_up] = u_g_value
      # storing h cost of the up neighbor.
      carInfo$mem$h_cost[currentx,neighbour_up] = u_h_value
      # setting f value which is g cost plus heuristic
      u_f_value = u_g_value + u_h_value
      # storing f value of the up neighbor.
      carInfo$mem$f_values[currentx,neighbour_up] = u_f_value
      if(source_node[1] == currentx && source_node[2] == neighbour_up){
        #we dont need to add parent node to the neighbour because we dont want to go back.
        u_f_value = 0
      }else{
        f_vector = c(f_vector,u_f_value)
      } 
    }
  }
  
  if(neighbour_down != 0){
    if(carInfo$mem$visited[currentx,neighbour_down] == 0){
      
      # Setting g cost of up neighbor which is the g cost to the expanded node plus the edge cost to the up neighbor
      d_g_value = trafficMatrix$vroads[currentx,neighbour_down] + carInfo$mem$g_cost[currentx,currenty]
      # Setting h cost of the up neighbor which is the manhattan distance between the up neighbor and the goal node.
      d_h_value = max(trafficMatrix$hroads)*(abs((carInfo$mem$goal[1] - currentx)) + abs((carInfo$mem$goal[2] - neighbour_down)))
      
      # storing g cost of the up neighbor.
      carInfo$mem$g_cost[currentx,neighbour_down] = d_g_value
      # storing h cost of the up neighbor.
      carInfo$mem$h_cost[currentx,neighbour_down] = d_h_value
      # setting f value which is g cost plus heuristic
      d_f_value = d_g_value + d_h_value
      # storing f value of the up neighbor.
      carInfo$mem$f_values[currentx,neighbour_down] = d_f_value
      if(source_node[1] == currentx && source_node[2] == neighbour_down){
        #we dont need to add parent node to the neighbour because we dont want to go back.
        d_f_value = 0
      }else{
        f_vector = c(f_vector,d_f_value)
      } 
    }
  }
  
  g_cost <<- carInfo$mem$g_cost
  h_cost <<- carInfo$mem$h_cost
  
  if(length(f_vector) == 0 ){
    # setting parent node.
    source_node[1] <<- carInfo$x
    source_node[2] <<- carInfo$y
    visited <<- carInfo$mem$visited
    f_values <<- carInfo$mem$f_values
    return(2)
  }else{
    min_f = min(f_vector)
  }
  # find out which frontier node has minimum f value
  if(min_f == r_f_value){
    minimum_f[1] <<- neighbour_right
    minimum_f[2] <<- currenty
  }else if(min_f == l_f_value){
    minimum_f[1] <<- neighbour_left
    minimum_f[2] <<- currenty
  }else if(min_f == u_f_value){
    minimum_f[1] <<- currentx
    minimum_f[2] <<- neighbour_up
  }else {
    minimum_f[1] <<- currentx
    minimum_f[2] <<- neighbour_down
  }
  
  # setting parent node.
  source_node[1] <<- carInfo$x
  source_node[2] <<- carInfo$y
  
  # If next node is goal node than go to that node.
  if(neighbour_right == carInfo$mem$goal[1] && carInfo$y == carInfo$mem$goal[2]){
    visited <<- carInfo$mem$visited
    f_values <<- carInfo$mem$f_values
    return(6)
  }else if(neighbour_left == carInfo$mem$goal[1] && carInfo$y == carInfo$mem$goal[2]){
    visited <<- carInfo$mem$visited
    f_values <<- carInfo$mem$f_values
    return(4)
  }else if(carInfo$x == carInfo$mem$goal[1] && neighbour_up == carInfo$mem$goal[2]){
    visited <<- carInfo$mem$visited
    f_values <<- carInfo$mem$f_values
    return(8)
  }else if(carInfo$x == carInfo$mem$goal[1] && neighbour_down == carInfo$mem$goal[2]){
    visited <<- carInfo$mem$visited
    f_values <<- carInfo$mem$f_values
    return(2)
  }
  
  # making other neighbors out of the frontier
  if(neighbour_right != 0 && minimum_f[1] == neighbour_right && minimum_f[2] == currenty){
    if(neighbour_left != 0){
      carInfo$mem$visited[neighbour_left,currenty] = 1
    }
    if(neighbour_up != 11){
      carInfo$mem$visited[currentx,neighbour_up] = 1
    }
    if(neighbour_down != 0){
      carInfo$mem$visited[currentx,neighbour_down] = 1
    }
    if(carInfo$mem$visited[neighbour_right,currenty] == 0){
      visited <<- carInfo$mem$visited
      f_values <<- carInfo$mem$f_values
      return(6)
    }
  }else if(neighbour_left != 0 && minimum_f[1] == neighbour_left && minimum_f[2] == currenty){
    if(neighbour_right != 11){
      carInfo$mem$visited[neighbour_right,currenty] = 1
    }
    if(neighbour_up != 11){
      carInfo$mem$visited[currentx,neighbour_up] = 1
    }
    if(neighbour_down != 0){
      carInfo$mem$visited[currentx,neighbour_down] = 1
    }
    if(carInfo$mem$visited[neighbour_left,currenty] == 0){
      visited <<- carInfo$mem$visited
      f_values <<- carInfo$mem$f_values
      return(4)
    }
  }else if(neighbour_up != 11 && minimum_f[1] == currentx && minimum_f[2] == neighbour_up){
    if(neighbour_right != 11){
      carInfo$mem$visited[neighbour_right,currenty] = 1
    }
    if(neighbour_left != 0){
      carInfo$mem$visited[neighbour_left,currenty] = 1
    }
    if(neighbour_down != 0){
      carInfo$mem$visited[currentx,neighbour_down] = 1
    }
    if(carInfo$mem$visited[currentx,neighbour_up] == 0){
      visited <<- carInfo$mem$visited
      f_values <<- carInfo$mem$f_values
      return(8)
    }
  }else if(neighbour_down != 0 && minimum_f[1] == currentx && minimum_f[2] == neighbour_down){
    if(neighbour_right != 11){
      carInfo$mem$visited[neighbour_right,currenty] = 1
    }
    if(neighbour_left != 0){
      carInfo$mem$visited[neighbour_left,currenty] = 1
    }
    if(neighbour_up != 11){
      carInfo$mem$visited[currentx,neighbour_up] = 1
    }
    if(carInfo$mem$visited[currentx,neighbour_down] == 0){
      visited <<- carInfo$mem$visited
      f_values <<- carInfo$mem$f_values
      return(2)
    }
  }else{
    visited <<- carInfo$mem$visited
    f_values <<- carInfo$mem$f_values
    return(5)
  }
}