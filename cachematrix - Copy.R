## Put comments here that give an overall description of what your
## functions do


# This function creates a special "matrix" object that can calculate and cache its inverse.

  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # holds the initial value or NULL if nothing is initialized
   
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    # returns the stored matrix
    get <- function() x
    setInverse <- function(inverse_cache) inv <<- inverse_cache
    # get the cached inverse value when the cacheSolve run for the first time
    #"inverse_cache" is a local variable to load the inverse - the inverse in calculate with Solve in cacheSolve 
    getInverse <- function() inv
    # Store the cached inverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  
  # "cacheSolve" function computes the inverse of the special "matrix" created by makeCacheMatrix function. 
  # if the "matrix" doesn't change it should retrieve the inverse from the cache instead of calculate it.
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    #send the inverse to makeCacheMatrix function
    x$setInverse(inv)
    # return the inverse 
    inv
  }


