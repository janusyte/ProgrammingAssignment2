## These 2 functions helps to cache potentially time-consuming computations of inverse matrix. 


## This function creates a special "matrix", which returns a list containing a function to
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse matrix
##   get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        ## Assign the new matrix to x
        set <- function(y) {
        # Assigns y to the 'parent' function
                x <<- y     
        ## Delete the inverse matrix of previously stored matrix 
                matinv <<- NULL   
        }
        ## get the main matrix
        get <- function() x
        ## set the values of inverse matrix of 'x'
        setinv <- function(mm) matinv <<- mm
        ## get the inverse matrix of matrix 'x'
        getinv <- function() matinv
        ## Function returns a list of 4 functions 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function calculates the inverse matrix of "special" matrix 
## created by function makeCacheMatrix.
## If the inverse matrix was already calculated before, the function returns 
## the value from the cache and skips the computations.

cacheSolve <- function(x, ...) {
        ## get the inverse matrix value from function makeCacheMatrix
        mm <- x$getinv()
        ## check if the the inverse matrix was already calculated
        if(!is.null(mm)) {
        ## if yes, get the cached matrix data
                return(mm)
        }
        ## calculate inverse matrix if it was not calculated before
        ## get the data of the main matrix
        data <- x$get()
        ## calculate inverse matrix
        mm <- solve(data, ...)
        ## save inverse matrix to cache
        x$setinv(mm)
        ## Return a matrix that is the inverse of 'x'
        mm
}
