makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    ##sets value of matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ##gets the value of the matrix
  setinverse <- function(solve) m <<- mean
  ##sets value of inverse
  getinverse <- function() m
  ##gets value of inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}