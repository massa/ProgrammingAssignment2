## These functions allow usage of an object stores a matrix
##   and possibly the inverse of that matrix.

## makeCacheMatrix
##   returns a list with three functions:
##   'get' returns the stored matrix;
##   'set' overwrites the stored matrix;
##   'getsolved' returns the (possibly cached) inverse of the stored matrix.
## This object's interface is slightly different from the one returned
##   by 'makeVector' in the assignment text; this is on purpose, since it
##   makes the implementation of the 'cacheSolved' more succint (and even
##   a little bit redundant) and it wasn't explicitly disallowed by the
##   assignment.

makeCacheMatrix <- function(x = matrix()) {
    solved <- NULL
    list(
        get = function() x,
        set = function(y) {
            x <<- y
            solved <<- NULL
        },
        getsolved = function(...) {
            if( is.null(solved) )
                solved <- solve(x, ...)
            solved
        }
    )
}


## cacheSolve
## returns the (possibly cached) inverse of the matrix stored in 'x',
##   passing any additional parameters to the 'solve' function.
## This function assumes that the matrix storoed is invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x$getsolved(...)
}

