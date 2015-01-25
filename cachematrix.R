## These two functions create a data structure that enables cache'ing of 
## an "expensive" operation: inverting a matrix. If the inverse has 
## already been calc'd and the matrix has not changed, then the cacheSolve 
## function will return the pre-calc'd answer. Otherwise, it will 
## perform the calculation.
## To do this, requires a data structure that functions as a "wrapper" 
## and include add'l information

## this function creates the data structure, or wrapper. it takes as 
## input a matrix, then gives back a list that includes the matrix
## plus additional information, including the cached value and associated 
## "methods" for 
## accessing and changing the underlying data

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y){
        x<<-y
        inv<<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv<<-inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
    
}


## cacheSolve takes as input a "CacheMatrix" as created in makeCacheMatrix
## the CacheMatrix is distinct from a matrix as it has the additional
## data and internal functions.
## cacheSolve will return the inverted matrix using the cached value, if 
## available, or a calculated inverse, if not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("using cached data")
        return(inverse)
    }
    inverse<-solve(x$get())
    x$setinverse(inverse)
    x$getinverse()
}
