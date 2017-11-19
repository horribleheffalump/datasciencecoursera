## The following functions define a cached matrix object, which can cache its inverse
## usage:
## A_cached <- makeCacheMatrix(A) ## create cached matrix
## cacheSolve(A_cached) ## calculate / get from cache inverse of A

## makeCacheMatrix expects a matrix as an input and constructs a cacheMatrix object, which has functions to get/set original matrix value and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}




## cacheSolve expects a cacheMatrix A as an input and returns inverse of A, which is only computed once at the first call after the matrix assignment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
