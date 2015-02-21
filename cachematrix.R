## The following functions facilitate the calculation of the inverse of a
## matrix in an efficient way, which involves caching the inverse once it gets
## calculated.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    # setters
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    setInverse <- function(inverse) { inv <<- inverse }
    
    # getters
    get <- function() { x }
    getInverse <- function() { inv }
    
    # return a list of the functions internal to makeCacheMatrix,
    # i.e. set, get, setmean, and getmean:
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## This function computes the inverse of the special "matrix" created by 
## the makeCacheMatrix function above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve function
## should retrieve the inverse from the cache and return it.
cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    
    # skip computing the inverse if already computed:
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, compute the matrix that is the inverse of 'x'...
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    
    # ...and return it:
    inv
    
}