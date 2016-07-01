## The functions below are used to cache the inverse of a matrix.
## This first function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x= matrix()){
  m <-NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get<- function()x
  setInverse <- function(solve) m <<- solve
  getInverse<- function() m
  list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by the above function. If the inverse is
## alread calculated then it will retrieve the inverse from the cache. 

cacheSolve <- function(x,...){
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}

