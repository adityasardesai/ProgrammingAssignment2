## create special matrix
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve){
    m <<- solve(x)
  }
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## check if inverse is already cahced else create inverse and return
cacheSolve <-  function(x,...){
  m <- x$getInverse()
  if(!is.null(m)){
    message("Getting Cached data")
    return(m) 
  }
  data <- x$get()
  m <- solve(data,..)
  x$setInverse(m)
  m
}