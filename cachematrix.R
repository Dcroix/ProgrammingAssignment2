## The functions presented below creates a special matrix with the capability to have its
## inverse cached and have it retrieved if such has been previously processed / calculated.
## Retrieval of cached data rather than having to recompute it again promotes effeciency.

## This function creates a special "matrix" object that is capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<- y
    inv <-- NULL
  }
  get <- function()x
  setinverse<- function(inverse) inv <<- inverse
  getinverse<- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function presented above. In case the aforementioned has already been computed
## with the matrix unchanged, then the cachesolve function gets the inverse from the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message ("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
        ## It returns a matrix that is the inverse of 'x'

