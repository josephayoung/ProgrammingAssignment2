## These two functions generate a new "matrix" object and 
## introduce a cached inverse resultset to avoid the costly
## computation of a matrix inverse.

## Creates a set of lazy getter/setter functions for a matrix with
## a cacheable inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the matrix. If the inverse has already
## been calculated, the cached result will be retrieved.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  i
}
