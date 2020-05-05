## to create lexical looping functions
## functions create a cache matrix and get the cache by sloving the matrix

## creating a cache matrix

makeCacheMatrix <- function(x = matrix()) {
        ins <- NULL
        set <- function(y) {
                x <<- y
                ins <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) ins <<- solve
        getsolve <- function() ins
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## cache solving matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ins <- x$getsolve()
        if(!is.null(ins)) {
                message("getting inversed matrix")
                return(ins)
        }
        data <- x$get()
        ins <- solve(data, ...)
        x$setsolve(ins)
        ins
}
