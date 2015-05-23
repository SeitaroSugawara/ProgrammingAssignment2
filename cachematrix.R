## Put comments here that give an overall description of what your
## functions do
## Caching the Inverse of a Matrix

## Write a short comment describing this function
## Making a cache Matrix
makeCacheMatrix <- function(x = matrix()) {
  ## initialize matrix variable
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get        <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Caching the Inverse of a Matrix if it doesn't exist
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
