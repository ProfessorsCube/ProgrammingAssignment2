## The functions below aim to cache potentially time-consuming computations, i.e. Matrix inversion. 
## That is when we need the values again it can look up the cache rather than recompute. 
## Cache the inverse of matrix.  

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  # starts with creating a placeholder for a future value
  mc <- NULL
  
  # defines a function to set the vector, x, to a new vector, y, and resets the value mc, to NULL
  set <- function(y) {
    x <<- y
    mc <<- NULL
  }
  
  # returns the vector, x
  get <- function() x
  
  #sets the function to cache the inverse of a matrix
  setinverse <- function(inverse) mc <<- inverse
  
  # returns the inverse, mc
  getinverse <- function() mc
 
  # returns the 'special vector' containing all of the functions just defined
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (matrix no change), 
## then the cachesolve to retrieve inverse from cache.


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  cs <- x$getinverse()
 
  ## # if the inverse has already been calculated
  if(!is.null(cs)) {
  
  ## # print msg; get data from the cache and skips the computation.   
    message("getting cached data")
    return(cs)
  }
 
  ## Else calculate the inverse 
  data <- x$get()
  cs <- solve(data, ...)
  x$setinverse(cs)
  cs
}

