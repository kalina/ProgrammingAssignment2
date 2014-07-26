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


## cacheSolve - takes a matrix and returns the inverse
## looks at the cache first.  If value hasn't been calculated, calls solve and caches the result.

cacheSolve <- function(x, ...) {
  inverse = x$getInverse()
  
  if(!is.null(inverse)){
    return(inverse)
  }
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  inverse
}
