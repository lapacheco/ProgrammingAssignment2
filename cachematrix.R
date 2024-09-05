## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## start by setting the inverse to null
    inv <- NULL
    ## create function that sets the matrix to new values
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## create function that gets the values of the matrix
    get <- function() x
    ## create the inverse using the argument into the inv variable
    setinv <- function(inverse) inv <<- inverse
    ## get the inverse
    getinv <- function() inv
    ## create the list of functions to call
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    ## if inverse already exists then call it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## store the new data
    data <- x$get()
    ## solve for the new inverse
    inv <- solve(data, ...)
    ## cache the new inverse
    x$setinv(inv)
    inv
}
