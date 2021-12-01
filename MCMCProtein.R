rm(list = ls()) #used to clear local environment


generateChain <- function(length, direction)
{
  x <- matrix(1:2*length, nrow = length, ncol = 3)
  x[1,1] <- 0
  x[1,2] <- 0
  for (i in 2:length)
  {
    if (direction[i-1] == 'E')
    {
      x[i,1] <- x[i-1,1] + 1
      x[i,2] <- x[i-1,2]
    }
    if (direction[i-1] == 'W')
    {
      x[i,1] <- x[i-1,1] - 1
      x[i,2] <- x[i-1,2]
    }
    if (direction[i-1] == 'N')
    {
      x[i,1] <- x[i-1,1] 
      x[i,2] <- x[i-1,2] + 1
    }
    if (direction[i-1] == 'S')
    {
      x[i,1] <- x[i-1,1] 
      x[i,2] <- x[i-1,2] - 1
    }
  }
  return (x)
}

generateDirs <- function(length)
{
  set <- sample(4, length, replace = TRUE)
  directions <- sample(0, length, replace = TRUE)
  i <- 1
  while(i <= length)
  {
    if (set[i] == 1)
    {
      directions[i] <- 'N'
    }
    if (set[i] == 2)
    {
      directions[i] <- 'E'
    }
    if (set[i] == 3)
    {
      directions[i] <- 'S'
    }
    if (set[i] == 4)
    {
      directions[i] <- 'W'
    }
    # print (i)
    i <- i + 1
  }
  return (directions)
}

SAW <- function(length)
{
  x <- matrix(0, nrow = length * 2 + 1, ncol = length * 2 + 1)
  lastx <- length + 1
  lasty <- length + 1
  x[lastx,lasty] <- 1
  written <- 0
  stopper <- 0
  tempx <- lastx
  tempy <- lasty
  label <- 'N'
  labelset <- sample(0, length-1, replace = TRUE)
  i <- 2
  while (i <= length)
  {
    dir  <- sample(4, 1, replace = TRUE)
    # print(dir)
    if (dir == 1)
    {
      tempy <- lasty + 1
      label <- 'N'
    }
    else if (dir == 2)
    {
      tempx <- lastx + 1
      label <- 'E'
    }
    else if (dir == 3)
    {
      tempy <- lasty - 1
      label <- 'S'
    }
    else if (dir == 4)
    {
      tempx <- lastx - 1
      label <- 'W'
    }
    if (x[tempx,tempy] == 0)
    {
      x[tempx,tempy] <- 1
      lastx <- tempx
      lasty <- tempy
      stopper <- 0
      labelset[i-1] <- label
      i <- i + 1
    }
    else 
    {
      tempx <- lastx
      tempy <- lasty
      stopper <- stopper + 1
      if (stopper == 100)
      {
        return(FALSE)
      }
    }
    
  }
  return(labelset)
}



checkValid <- function(direction, length)
{
  if (length == 1)
  {
    return(TRUE)
  }
  x <- matrix(0, nrow = length * 2 + 1, ncol = length * 2 + 1)
  lastx <- length + 1
  lasty <- length + 1
  x[lastx,lasty] <- 1
  coords <- matrix(1:2*length, nrow = length, ncol = 2)
  coords[1,1] <- lastx
  coords[1,2] <- lasty
  for (i in 2:length)
  {
    if (direction[i-1] == 'E')
    {
      lastx <- lastx + 1
    }
    if (direction[i-1] == 'W')
    {
      lastx <- lastx - 1
    }
    if (direction[i-1] == 'N')
    {
      lasty <- lasty + 1
    }
    if (direction[i-1] == 'S')
    {
      lasty <- lasty - 1
    }
    x[lastx,lasty] <- i
    coords[i,1] <- lastx
    coords[i,2] <- lasty
  }
  
  lastx <- length + 1
  lasty <- length + 1
  for (i in 1:length) 
  {
    if(x[coords[i,1],coords[i,2]] != i)
    {
      return(FALSE)
    }
  }
  return(TRUE)
}





checkValidFast <- function(direction, length)
{
  a <- 1:length
  b <- 2:length
  x <- matrix(1:2*length, nrow = length, ncol = 2)
  x[1,1] <- 0
  x[1,2] <- 0
  for (i in b)
  {
    if (direction[i-1] == 'E')
    {
      x[i,1] <- x[i-1,1] + 1
      x[i,2] <- x[i-1,2]
    }
    if (direction[i-1] == 'W')
    {
      x[i,1] <- x[i-1,1] - 1
      x[i,2] <- x[i-1,2]
    }
    if (direction[i-1] == 'N')
    {
      x[i,1] <- x[i-1,1] 
      x[i,2] <- x[i-1,2] + 1
    }
    if (direction[i-1] == 'S')
    {
      x[i,1] <- x[i-1,1] 
      x[i,2] <- x[i-1,2] - 1
    }
  }
  
  hash <- matrix(0, nrow = length , ncol = length )
  for (i in a)
  {
    d <- floor(distance(x[i,1], x[i,2], 0, 0)) + 1
    j <- 1
    while(hash[d,j] != 0)
    {
      j <- j + 1
    }
    hash[d, j] <- i
  }
  
  for (i in a) #n^2 time fix if slow
  {
    d <- floor(distance(x[i,1], x[i,2], 0, 0)) + 1
    j <- 1
    while(hash[d,j] != 0)
    {
      k <-hash[d,j]
      if(k != i && x[i,1] == x[k,1] && x[i,2] == x[k,2])
      {
        return(FALSE)
      }
      j <- j + 1
    }
  }
  return(TRUE)
}

squdist <- function(x1, y1, x2, y2)
{
  x2x1 <- x2 - x1
  x2x1 <- x2x1 * x2x1
  y2y1 <- y2 - y1
  y2y1 <- y2y1 * y2y1
  ret <- x2x1 + y2y1
  return(ret)
}

distance <- function(x1, y1, x2, y2)
{
  x2x1 <- x2 - x1
  x2x1 <- x2x1 * x2x1
  y2y1 <- y2 - y1
  y2y1 <- y2y1 * y2y1
  ret <- x2x1 + y2y1
  ret <- sqrt(ret)
  return(ret)
}

Energy <- function(coords, length)
{
  en <- 0
  for (i in 1:length) #n^2 time fix if slow
  {
    for(j in 1:i)
    {
      
      if (coords[i,3] == 0 && coords[j,3] == 0 && i != j && i != j + 1)
      {
        dist <- distance(coords[i,1], coords[i,2], coords[j,1], coords[j,2])
        if (dist == 1)
        {
          en <- en - 1
        }
      }
      
    }
  }
  return(en)
}

pivot <- function(oldDir, length)
{
  index <- sample(length - 1, 1, replace = TRUE)
  lower <- index - 5
  if (lower < 2)
  {
    lower <- 2
  }
  for (i in (lower):(index))
  {
    # print(i)
    if (oldDir[i] == 'N')
    {
      oldDir[i] <- 'E'
    }
    if (oldDir[i] == 'E')
    {
      oldDir[i] <- 'N'
    }
    if (oldDir[i] == 'S')
    {
      oldDir[i] <- 'W'
    }
    if (oldDir[i] == 'W')
    {
      oldDir[i] <- 'S'
    }
  }
  return(oldDir)
}

mutateOneDir <- function(oldDir, length)
{
  index <- sample(length - 1, 1, replace = TRUE)
  newWay <- sample(4, 1, replace = TRUE)
  if (newWay == 1)
  {
    newWay <- 'N'
  }
  if (newWay == 2)
  {
    newWay <- 'E'
  }
  if (newWay == 3)
  {
    newWay <- 'S'
  }
  if (newWay == 4)
  {
    newWay <-  'W'
  }
  oldDir[index] <- newWay
  return(oldDir)
}

snakeMove <- function(oldDir, length)
{
  index <- sample(length - 1, 1, replace = TRUE)
  lower <- index - 5
  if (lower < 2)
  {
    lower <- 2
  }
  for (i in lower:length-1)
  {
    # print(i)
    if (oldDir[i] == 'N')
    {
      oldDir[i] <- 'E'
    }
    if (oldDir[i] == 'E')
    {
      oldDir[i] <- 'N'
    }
    if (oldDir[i] == 'S')
    {
      oldDir[i] <- 'W'
    }
    if (oldDir[i] == 'W')
    {
      oldDir[i] <- 'S'
    }
  }
  return(oldDir)
}


#CRUDE MONTE CARLO -- RANDOMLY SAMPLE VALID STATES BLINDLY
bestenergy <- 0
length <- 50
bestx <- 0
directions <- SAW(length)
while(directions == FALSE)
{
  directions <- SAW(length)
}
x <- generateChain(length, directions)
for(i in 1:length)
{
  x[i,3] <- HP[i]
}
for (i in 1:10000)
{
  print(i)
  directions <- SAW(length)
  while(directions == FALSE)
  {
    directions <- SAW(length)
  }
  
  x <- generateChain(length, directions)
  for(i in 1:length)
  {
    x[i,3] <- HP[i]
  }
  energy <- Energy(x,length)
  if (energy < bestenergy)
  {
    bestenergy <- energy
    bestx <- x
  }
}

MCMC <- function (burnIn = 1000, itr = 10000, length = 50, pivot = TRUE, snake = TRUE, prob = .25)
{
  directions <- SAW(length)
  while(directions == FALSE)
  {
    directions <- SAW(length)
  }
  x <- generateChain(length, directions)
  for(i in 1:length)
  {
    x[i,3] <- HP[i]
  }
  bestdir <- 0
  bestenergy <- 0
  bestx <- 0
  for (i in 1:burnIn)
  {
    print(i)
    directions <- SAW(length)
    while(directions == FALSE)
    {
      directions <- SAW(length)
    }
    
    x <- generateChain(length, directions)
    for(i in 1:length)
    {
      x[i,3] <- HP[i]
    }
    energy <- Energy(x, length)
    if (energy < bestenergy)
    {
      bestenergy <- energy
      bestdir <- directions
      bestx <- x
    }
  }
  directions <- bestdir
  energies <- bestenergy
  thisenergy <- bestenergy
  for (i in 1:itr)
  {
    print(i)
    newdirections <- snakeMove(directions, length) #1%
    if (checkValidFast(newdirections, length) && snake == TRUE)
    {
      x <- generateChain(length, newdirections)
      for(i in 1:length)
      {
        x[i,3] <- HP[i]
      }
      energy <- Energy(x, length)
      rand <- runif(1)
      if (energy < thisenergy || rand < prob)
      {
        directions <- newdirections
        thisenergy <- energy
      }
    }
    newdirections <- pivot(directions, length) #8%
    if (checkValidFast(newdirections, length) && pivot == TRUE)
    {
      x <- generateChain(length, newdirections)
      for(i in 1:length)
      {
        x[i,3] <- HP[i]
      }
      energy <- Energy(x, length)
      rand <- runif(1)
      if (energy < thisenergy || rand < prob)
      {
        directions <- newdirections
        thisenergy <- energy
      }
    }
    newdirections <- mutateOneDir(directions, length) #50%
    if (checkValidFast(newdirections, length))
    {
      x <- generateChain(length, newdirections)
      for(i in 1:length)
      {
        x[i,3] <- HP[i]
      }
      energy <- Energy(x, length)
      rand <- runif(1)
      if (energy < thisenergy || rand < prob)
      {
        directions <- newdirections
        thisenergy <- energy
      }
    }
    
    
    x <- generateChain(length, directions)
    for(i in 1:length)
    {
      x[i,3] <- HP[i]
    }
    energy <- Energy(x, length)
    if (energy < bestenergy)
    {
      bestenergy <- energy
      bestdir <- directions
      bestx <- x
    }
    energies <- c(energies, Energy(x, length))
  }
  plot(bestx[,1] , bestx[,2])
  lines(bestx[,1], bestx[,2])
  print(bestenergy)
  return(energies)
}






#------------------------MARKOV CHAIN MONTE CARLO--------------------------
#------------------------------
#Charge sequence. 0 is nonpolar (H) and decreases energy when paired. 1 is polar (P). 
HP <- c(0,0,1,0,1,0,1,0,1,0,0,0,0,1,0,1,1,1,0,1,1,1,0,1,1,1,1,0,1,1,1,0,1,1,1,0,1,0,0,0,0,1,0,1,0,1,0,1,0,0)
#-----------------------------

energylist <- vector()
for (i in 1:100)
{
  energylist <- c(energylist, MCMC(burnIn = 1000, itr = 1000, length = 50, pivot = TRUE, snake = TRUE)) # lots of shorter chains
}
mean(energylist) 
sd(energylist)
hist(energylist, main = "MCMC, Short Chains")

energylist <- MCMC(burnIn = 10000, itr = 10000, length = 50, pivot = FALSE, snake = FALSE, prob = .1)  #one big chain
mean(energylist) 
sd(energylist)
hist(energylist, main = "MCMC, Long Chain")





