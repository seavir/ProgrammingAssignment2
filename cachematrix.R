## Caching the Inverse of a Matrix

## As matrix inversion is usually a costly computation,
## there may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly, at least
## when data frames involve Big Data matrices.

## makeCacheMatrix creates a list of functions that
## can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## set the value of the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the inverse of the matrix
        setInverse <- function(inverse) m <<-inverse
        getInverse <- function() m
        ## get the inverse of the matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}



## cacheSolve returns the inverse of the matrix 
## returned by makeCacheMatrix(). If the inverse has
## already been calculated, it will retrie it from the cache.

cacheSolve <- function(x, ...) {
        ## get the inverse of the matrix
        m <- x$getInverse()
        ## check if there is the matrix
        if ( ! is.null(m)) {
                print("getting cached data")
                return(m)
        }
        ## if not: get the inverse of the matrix
        m <- solve(x$get())
        ## set the inverse of the matrix 
        x$setInverse(m)
        m
}

