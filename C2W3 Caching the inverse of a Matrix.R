## Caching the inverse of a Matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

  inv <- NULL

## Setting the matrix
  set <- function( matrix ) {
    m <<- matrix
    inv <<- NULL
  }

## Returning the matrix
  get <- function() {
    m
  }
  
## Inverse of the matrix (settting)
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
## Inverse of the matrix (getting)
  getInverse <- function() {
    inv
  }
  
## List of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
## Getting the matrix
  data <- x$get()
  
## Calculating the inverse
  m <- solve(data) %*% data
  
## Setting the inverse
  x$setInverse(m)
  
## Returning the matrix
  m
}