
# makeCacheMatrix: creates a special "vector", which is really a 
# list containing a function to: 
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. GEt the value of the inverse cached

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) i <<- inv
    get_inverse <- function() i
    list(set = set, get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse
    )
}


# cacheSolve: Function that calculate the inverse of the matrix, 
# cfreated by the above function. it first checks to see if the inverse 
# has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse and sets the value of the 
# inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$get_inverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$set_inverse(i)
    i
}
