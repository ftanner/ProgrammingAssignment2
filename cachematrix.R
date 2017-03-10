## Author: Frank Tanner
## File: cachedmatrix.R
## Pedigree: forked from https://github.com/rdpeng/ProgrammingAssignment2
##
## Description: These functions manage a matrix cache so that expensive
## matrix operations do NOT need to be computed more than once.  The initial
## implementation only caches matrix inversion operations on invertable
## matricies with the "solve" function.
##

makeCacheMatrix <- function(cachedMatrix = matrix()) {
    # Creates a matrix cache object that caches the matrix x
    #
    # Args:
    #   x: the matrix to cache
    #
    # Returns:
    #  The cached matrix object.  Note, this is NOT a matrix
    #  but a separate object.
    #
    cachedInverse <- NULL
    setMatrix <- function(y) {
        cachedMatrix <<- y
        cachedInverse <<- NULL
    }
    getMatrix <- function() cachedMatrix
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = setMatrix, get = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    # Solves the inverse of a cached matrix.  Note that
    # cached matrix, x, is assumed to be invertable
    # via the solve function.  If not, an error will be thrown.
    # This only inverts the matrix if it has NOT already
    # been
    #
    # Args:
    #   x: the cached matrix object
    #
    # Returns:
    #  The cached matrix object.  Note, this is NOT a matrix
    #  but a separate object.
    #    
    xInverse <- x$getInverse()
    if(!is.null(xInverse)) {
        # message for debugging, commented out now
        #message("getting cached data")
        return(xInverse)
    }
    xMatrix <- x$get()
    xInverse <- solve(xMatrix, ...)
    x$setInverse(xInverse)
    xInverse
}
