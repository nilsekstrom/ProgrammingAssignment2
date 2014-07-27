## This script contains 2 functions, makeCacheMatrix and cacheSolve. These functions operate
## to calculate the inverse of a matrix and cache the answere.

##"makeCacheMatrix" is a function that creates a matrix object that in fact is a list that 
## operate to set the value of the matrix, get the value of the matrix, set the value of the 
## inverse matrix and get the value of the inverse matrix. It is also able to cache the value
## of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function "cacheSolve" calculates the inverse of the special matrix retrieved through
## the makeCacheMatrix function (using the "solve function") and caches the answere. If the 
## inverse has already been cached in the special matrix retrieved through the makeCacheMatrix 
## function, the cacheSolve will retrieve the inverse from there. Othe

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("retrieved from cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
