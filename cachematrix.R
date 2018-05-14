## The following functions allow user to get the inverse of their input matrix.
## However, if the inverse is already stored, it gets the inverse from the cache to save time and memory.

## makeCacheMatrix creates a "special matrix", but it just ultimately returns a list of functions when ran.
## When a new matrix is inputted, makeCacheMatrix reinitializes all the values needed in cacheSolve.

makeCacheMatrix <- function(mat = matrix()) {
    i <- NULL
    set <- function(y) {
        mat <<- y
        i <<- NULL
    }
    get <- function() {mat}
    setinv <- function(inv) {i <<- inv}
    getinv <- function() {i}
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve will get the inverse of the "special matrix" created in makeCacheMatrix.

cacheSolve <- function(mat, ...) {
    i <- mat$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- mat$get()
    i <- solve(data, ...)
    mat$setinv(i)
    i
}

## Try:
## x <- makeCacheMatrix(diag(5))
## cacheSolve(x)
## cacheSolve(x) will return the message "getting cached data" when ran the second time.