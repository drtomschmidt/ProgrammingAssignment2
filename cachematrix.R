## Put comments here that give an overall description of what your
## functions do
# The first function creates a sort of "matrix" while building a function to process matrices
# and create an inverse.
# The second function uses the first function to return an inverted matrix

# These functions are the work of Dr. Tom Schmidt

## Write a short comment describing this function
# This function takes in a matrix as a parameter. It then sets up a set of "helper"
#anonymous functions that can calculate an inverse and return the inverse from
#a cached copy.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

# This function takes in a matrix, which is in fact a special matrix like the one
# created by makeCacheMatrix
# It returns the inverse of that matrix, creating it on the fly if it does not
# exist, and returning a previously created cached copy if it does exist.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}


}
