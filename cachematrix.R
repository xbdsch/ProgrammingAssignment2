## Compute the inversion of a matrix and cache the value. If the contents of a 
## matrix are not changing, look for cached inversion first and use the value.
## makeCacheMatrix create a list of functions for set and get cached values
## cacheSolve look for cached inversion first and use it if found, otherwise it
## calculates the inversion. 


## Create a list of functions for set and get cached values of the matrix and
## its inversion
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { ## set the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x ## get the value of the matrix
    setinv <- function(inversion) inv <<- inversion ## set the value of the matrix inversion
    getinv <- function() inv ## get the value of the matrix inversion
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Look for cached inversion first and use it if found, otherwise it
## calculates the inversion. 
cacheSolve <- function(x, ...) {
    inv <- x$getinv()  ## Check if cached value exists
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

## below is a test run
matrix_a <- matrix(c(2,0,0,2), 2,2)
a <- makeCacheMatrix(matrix_a)
b <- cacheSolve(a)
c <- cacheSolve(a)
