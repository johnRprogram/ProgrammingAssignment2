## Written for Coursera R programming Course
## This code uses two functions
## makeCacheMatrix creates/returns a list of 4 functions, where
## the argument passed to the function is a square invertible matrix

## This function takes x an invertible square matrix and
## returns a list of 4 named functions to the the parent environment
## solve calculates the inverse of the matrix x
## note of use of <<- which results in assignment of variables
## in the parent environment
## example: 
##      a <- matrix(1:4, nrow = 2, ncol = 2)
##      lista <- makeCacheMatrix(a)
makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invx <<- solve
        getinv <- function() invx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)     
}

## This function takes the list generated from makeCacheMatrix
## and first determines if the invx exists (is not NULL)
## if invx exists, then the existing value is returned, rather
## than recalculating the inverse
## example:
##      cacheSolve(lista) 
cacheSolve <- function(x, ...) {
        invx <- x$getinv()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data, ...)
        x$setinv(invx)
        invx
}
