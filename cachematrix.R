## My functions are basically equal to the functions described in the vector
## mean example (I still didn't understand the code completely, but I understand
## more now than before).

## This function creates a special matrix that can store it's own inverse
## when applied at least one time in the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## This function see if the special matrix has any cache. If it has, then there
## is no computation and the inverse is returned. It computes the inverse other-
## wise, and return the result.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
