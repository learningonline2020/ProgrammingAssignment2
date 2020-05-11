## Functions that cache inverse of a matrix

## This function creates a special matrix to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute inverse of a matrix
## Use cache if inverse values already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## get cache if calculation already done
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## computing inverse the matrix
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
