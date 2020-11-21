# Date: 21 Nov 2020
# Programming Assignment 2 (Coursera: JHU R Programming)
# This project contains two pairs of functions

# The first function creates an R object (a list) that caches
# 1 the value of a matrix; and
# 2 the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

# The second function uses the first function as its argument in order to
# solve the inverse of the matrix x. This is where the solve() function is executed

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
