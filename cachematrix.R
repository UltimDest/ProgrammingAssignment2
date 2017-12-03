##This is a submission for ProgrammingAssignment2

## Functions makeCacheMatrix and cacheSolve  
## are able to cache potentially time-consuming computation of returning
## an inverse of a matrix if contents of a supplied vector is unchanging

## Matrices supplied are assumed inversible (and square)

## The makeCacheMatrix is a function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
  
}

## Sample values for input
## >x <-matrix(ncol=2,nrow=2,c(4,2,7,6))
## >m = makeCacheMatrix(x)
## >sample1 <- cacheSolve(m)
## >sample1
##     [,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4

##cacheSolve(m)
##getting cached data
##     [,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4
