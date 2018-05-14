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