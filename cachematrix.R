## This function generates the inverse of the given matrix x

makeCacheMatrix <- function(x = matrix()) {
    p <- NULL
  set <- function(y) {
          x <<- y
          p <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) p <<- inverse
  getinverse <- function() p
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function checks if the inverse of given matrix is already available in the cache

cacheSolve <- function(x, ...) {
  p <- x$getinverse()
  if (!is.null(p)) {
          message("getting cached data")
          return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setinverse(p)
  p
}
