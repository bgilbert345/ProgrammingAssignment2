## This script can be used to avoid recalculating
## matrix inverses in loops; the value of the 
# inverse may be retrieved from a cache

## Creates a list of functions to set matrix
## value, get matrix value, set matrix inverse
## value, and get matrix inverse value

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## If the inverse (s) has already been calculated
## it is retrieved from the cache and returned
## immediately. Otherwise, it is computed and
## stored in the cache

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
