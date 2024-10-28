## The makeCacheMatrix function creates a special "matrix" object 
## that can store a matrix and cache its inverse. 
## It returns a list of functions to:

##      $set: set the matrix value.
##      $get: get the matrix value.
##      $setsolve: set the cached inverse.
##      $getsolve: get the cached inverse.

## This allows the cached result from solve() to be stored 
## outside the local environment scope , avoiding redundant calculations. 
## The returned list provides access to these functions and 
## retains their references in memory.


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


## The cacheSolve function computes the inverse of the "matrix" 
## object created by makeCacheMatrix. It first checks if the inverse 
## has already been calculated and cached outside the actual functions' scope.
## If so, it retrieves the cached inverse to avoid redundant computation. 
## If the inverse is not cached, cacheSolve calculates it, stores it in 
## the cache, and then returns it. This function optimizes performance 
## by caching the inverse for repeated access.

cacheSolve <- function(x, ...) {
            m <- x$getsolve()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setsolve(m)
            m
    }
