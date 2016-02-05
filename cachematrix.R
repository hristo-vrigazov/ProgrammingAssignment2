## Find the inverse matrix by caching results
# Using the concept "memoization" from functional programming

# function that exposes the get, set, and getinverse and setinverse
# functions on the matrix for manipulating the cached result

makeCacheMatrix <- function(x = matrix()) {
    cachedResult <- NULL
    
    set <- function (y) {
        x <<- y
        cachedResult <<- NULL
    }
    
    get <- function () {
        x
    }
    
    setinverse <- function (solve) {
        cachedResult <<- solve
    }
    
    getinverse <- function () {
        cachedResult
    }
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## by consuming the api exposed by the function above,
## cacheSolve finds the inverse of a matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cachedResult <- x$getinverse()
    
    # if there exists a cachedResult, return it
    if (!is.null(cachedResult)) {
        return(cachedResult)
    }
    
    data <- x$get()
    cachedResult <- solve(data, ...)
    x$setinverse(cachedResult)
    cachedResult
}
