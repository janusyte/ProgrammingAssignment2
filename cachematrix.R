## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinv <- function(mm) matinv <<- mm
        getinv <- function() matinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        mm <- x$getinv()
        if(!is.null(mm)) {
                message("getting cached matrix data")
                return(mm)
        }
        data <- x$get()
        mm <- solve(data, ...)
        x$setinv(mm)
        ## Return a matrix that is the inverse of 'x'
        mm
}
