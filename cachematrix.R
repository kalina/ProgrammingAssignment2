# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  cacheInverse <- function(inverse){
    m <<- inverse 
  }
  getInverse <- function(){
    m
  }
  list(set = set, get = get,
       cacheInverse = cacheInverse,
       getInverse = getInverse)
}


# cacheSolve - returns the inverse of a matrix
# looks at the cache first.  
# If value hasn't been calculated, calls solve and caches the result.

cacheSolve <- function(x, ...) {
  inverse = x$getInverse()
  
  if(!is.null(inverse)){
    message ("Retrieved from cache")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  inverse
}
