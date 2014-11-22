## As inverting a matrix is a costly operation,
## we cache the result for future use 
## to avoid a waste of computational resource.

## Givem a matrix, this function returns an obiect
## which is ready to cache the result of the first call
## to its solve method and return the cached value for further calls
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function uses the object created by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
