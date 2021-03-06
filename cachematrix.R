## Functions makeCacheMatrix and cacheSolve create a list of 
## functions, and their defining environment, to get and set a matrix 
## and its inverse. Inverse is calculated only if it's not already stored in this environment

## makeCacheMatrix takes a matrix as an agument and returns a list of functions with the matrix 
## stored in their defining environment. 
## Functions in the returned list are:
## "set" and "get" to store and retrieve this matrix
## "setinverse" and "getinverse" to store and retrieve the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve function takes an object of the type returned by the makeCacheMatrix function
## (a list with the functions get, set, getinverse and setinverse)
## and returns the inverse of the matrix defined in those functions defining environment
## It checks if the inverse is already stored in the defining environment of the functions
## in the list passed and..
## If it is stored, it returns it. 
## If it isn't, the function calculates it, returns it, and saves it to that environment

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
      
}
