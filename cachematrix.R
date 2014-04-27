## Below are two functions that are used to cache the inverse of a given matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It takes a matrix object as input and produces a list of functions as output.

makeCacheMatrix <- function(x = matrix()) {
        ## create cached inverse matrix object and set to NULL
        cachedMatrix <- NULL
        ## create 'set' function to set values for the matrix
        set <- function(matrix) {  
                x <<- matrix
                cachedMatrix <<- NULL
        }
        ## create 'get' function to obtain the matrix  
        get <- function() x   
        ## create 'setInv' function to cache the inverse of the matrix
        setInv <- function(invMatrix) cachedMatrix <<- invMatrix
        ## create 'getInv' function to recover cached inverse matrix
        getInv <- function() cachedMatrix
        ## return list of the four functions created
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## the function makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Retrieve the value of inverse of matrix x
        cachedMatrix <- x$getInv()
        ## If the inverse is already in cache, return that inverse matrix...
        if(!is.null(cachedMatrix)) {
                message("getting cached data")
                return(cachedMatrix)
        }
        ## ... Otherwise, get matrix x
        data <- x$get()
        ## and calculate inverse with 'solve' function
        cachedMatrix <- solve(data)
        ## store in cache/return
        x$setInv(cachedMatrix)
        cachedMatrix
}