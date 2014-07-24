## Functions to create "special matrix" and return its inverse. This "special matrix" is able to cache its inverse

## Create the special "matrix" from a matrix "x" (this function assumes that "x" is invertible)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) { # sets the value of the matrix
    x <<- y
    inv <<- NULL # If the matrix is changed, cached inverse should be updated afterwards
  }
  get <- function() x # get the value of the matrix
  
  setinverse <- function(inverse) inv <<- inverse # stores the inverse of "x" in the cache
  getinverse <- function() inv # gets the value of the inverse of "x" in the cache
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of "x" 
## Note that the input "x" is a "special matrix" created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() # Get the current cached value of the inverse
  
  if(!is.null(inv)) { # If the current cached value is not null, then return the inverse
    message("getting cached inverse")
    return(inv)
  }
  
  # Otherwise, inverse has to calculated and returned
  message("calculating inverse (and caching)")
  data <- x$get()         # Get matrix data
  inv <- solve(data, ...) # Calculate inverse
  x$setinverse(inv)       # Cache inverse of "x"
  inv
}
