## Matrix inversion is usually a costly computation and it makes sense to cache  
## the inverse of a matrix rather than compute it repeatedly. 
## The below pair of functions enable the caching of inverse of a matrix to avoid repetitive, 
## expensive computation


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv.m) m <<- inv.m
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve will retrieve the inverse from the cache - thus avoiding the expensive operation
## Assumption: the matrix passed as argument is an inversible matrix

cacheSolve <- function(x, ...) {
        ## Check if the inverse of the passed matrix already exists in cache
        inv <- x$getinverse()
        if(!is.null(inv)) {
                ## if yes, return from cache
                message("getting cached data")
                return(inv)
        }
        ## if the inverse does not exist in cache
        ## 01. compute the inverse
        ## 02. set the same in cache
        ## 03. return the inverse
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
