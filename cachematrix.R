## Returns the inverse of a matrix.  
## Uses a cache so the inverse will be calculated once.

## makeCacheMatrix - function containing a list of functions to 
## cache/retrieve an inverse of a matrix

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


## cacheSolve - returns the inverse of a matrix
## looks at the cache first.  If value hasn't been calculated, calls solve and caches the result.

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
