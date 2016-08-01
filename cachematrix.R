## makeCacheMatrix stores a matrix X in memory


makeCacheMatrix <- function(X = matrix()) {
    inverse <- NULL
    set <- function(Y) {
        X <<- Y
        inverse <<- NULL;
    }
    get <- function() return(X);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(X, ...) {
    inverse <- X$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- X$get()
    invserse <- solve(data, ...)
    X$setinv(inverse)
    return(inverse)
}
