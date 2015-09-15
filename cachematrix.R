## File-Name:       cachematrix.R
## Date:            2015-09-15                                
## Author:          Ruben Maso
## Purpose:         Compute the inverse matrix is potentially time-consuming computations. The purpouse of the code is
##                  create an object to cache the inverse of a matrix and a function to return the inverse of the matrix,
##                  if the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##                  should retrieve the inverse from the cache, in other case, the compute the inverse and cache it.
## Examples:
##                  x <- makeCacheMatrix(matrix(runif(1000^2),1000))  # create a random 1000x1000 matrix
##                  cacheSolve(x) # return the inverse of x

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## This object contains the following functions:
##      set: set the value of the matrix
##      get: get the value of the matrix
##      setinverse: set the value of the inverse matrix
##      getinverse: get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        # Define the function to set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # Define the function to get the value of the matrix
        get <- function() x
        
        # Define the function to set the value of the inverse matrix
        setinverse <- function(inverse) i <<- inverse
        
        # Define the function to get the value of the inverse matrix
        getinverse <- function() i
        
        #return the list with the four functions 
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## check if inverse is cached. If True, return cached value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## else compute the inverse of the matrix
        m <- solve(x$get(), ...)
        ## cache the inverse matrix
        x$setinverse(m)
        m
}
