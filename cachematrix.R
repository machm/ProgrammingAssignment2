## This function creates a matrix that will cache it's own inverse

makeCacheMatrix <- function(x = matrix()) {
        ## create properties for, and set, the inverse
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## create the matrix 
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}

## This function takes the "makeCacheMatrix" results and computes the 
## inverse. It will retrive the inverse if it has already been created.

cacheSolve <- function(x, ...) {
        ## Returns the inverse matrix of "x"
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
