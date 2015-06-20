## Put comments here that give an overall description of what your
## functions do

#####################################################

## The "set" in makeCacheMatrix resets "x" and "m" in the enclosing environment, 
## which is why we use "x <<- y, m <<- NULL" in the "set" function instead of 
## "x <- y, m <- NULL".

## Similarly, the "setinverse" function resets "m" in the enclosing environment, 
## which is why we use "m <<- INVERSE" instead of "m <- INVERSE"

## The "get" function and "getinverse" function just return the matrix and inverse
## respectively.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(INVERSE) m <<- INVERSE
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#########################################################################


## For cacheSolve, if the inverse is previously calculated, then it just return
## the previous inverse with additional message "getting cached data"

## If not, then it calculates the inverse, and set the inverse using "setinverse".

cacheSolve <- function(x, ...) {
    INVERSE <- x$getinverse()
    if(!is.null(INVERSE)) {
        message("getting cached data")
        return(INVERSE)
    }
    data <- x$get()
    INVERSE <- solve(data, ...)
    x$setinverse(INVERSE)
    INVERSE
    ## Return a matrix that is the inverse of 'x'
}

