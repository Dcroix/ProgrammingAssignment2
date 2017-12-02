## The functions presented below creates a special matrix with the capability to have its
## inverse cached and have it retrieved if such has been previously processed / calculated.
## Retrieval of cached data rather than having to recompute it again promotes effeciency.

## This function creates a special "matrix" object that is capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                                            #sets "inv" to NULL
  set <- function(y){                                                    #defines a function that:
    x<<- y                                                               #sets the vector from x to y
    inv <<- NULL                                                         #sets "inv" to NULL
  }
  get <- function()x                                                     #returns the vector, x
  setinverse<- function(inverse) inv <<- inverse                         #sets the inverse represented by "inv" to inverse
  getinverse<- function() inv                                            #returns the inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)   #returns the special matrix with all of the recently defined functions
}

## This function computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function presented above. In case the aforementioned has already been computed
## with the matrix unchanged, then the cachesolve function gets the inverse from the cache. 

cacheSolve <- function(x, ...) {                                         #gets a variable with value from the makeCacheMatrix() function
  inv <- x$getinverse()                                                  #gives value to inv, a value of the makeCacheMatrix() function 
  if(!is.null(inv)){                                                     #reviews the value of inv whether it is null or not, if it is not null then it gets the value of inv
    message ("getting cached data")
    return(inv)
  }
  data <- x$get()                                                        #if inv is NULL, then it assigns new data to the specified variable
  inv <- solve(data)                                                     #assigns inverse of new data to inv
  x$setinverse(inv)                                                      #stores it as cache           
  inv                                                                    #prints inv
}
        ## It returns a matrix that is the inverse of 'x'

