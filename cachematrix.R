## This functions are designed to optimize the process of taking 
## the inverse of matrix by caching results of solved matrcices

## Function adds some "properties" to matrix 
## that allow to access the source matrix and also to set/get inverted matrix 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        get <- function() x
        getinv <- function() s
        setinv <- function(solve)  s <<- solve
        list(get = get, getinv = getinv, setinv = setinv)
}

## Function calculates the invertion of matrix x
## with caching of result
## so called repeatedly for the same object it returns result from the cache
## instead of computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if (!is.null(i)) {
                ##returning cached matrix
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
