
## Create a special matrix from a matrix x, allowing having its inverse matrix in cache

makeCacheMatrix <- function(x = numeric()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinvert <- function(invert) i <<- invert
        getinvert <- function() i
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}

## Return a matrix that is the inverse of 'x'
## calculate the inverse or get the cached matrix if already calculated

cacheSolve <- function(x, ...) {

        i <- x$getinvert()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinvert(i)
        i
}