## These functions are used to cache the inverse of a matrix so
## that it does not have to be calculated each time it is needed

##creates matrix for caching inverse into

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ##retrieves input matrix
  setinverse <- function(solve) inv <<- solve  ##sets value of cached inverse
  getinverse <- function() inv  ##retrieves cached inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## retrieves or solves the inverse of a matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {  ##displays inverse if already cached
    message("getting cached data")
    return(inv)
  }
  data <- x$get()  ##calculates inverse if not already cached
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
