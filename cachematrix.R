## Module facilitates creation of an matrix object that caches its own inverse.
## makeCacheMatrix creates the object.
## cacheSolve returns the inverse using cached data if availible.

# makeCacheMatrix creates a matrix object that caches its own inverse.
#
# The argument is a matrix assumed to be invertible.
# The function returns a list of functions
#   set - sets the matrix
#   get - returns the matrix
#   setinv - sets the cached inverse
#   getinv - returns the cached invers if availible NULL otherwise
makeCacheMatrix <- function(m = matrix()) {
    #Set the cached matrix to NULL by default
    inv <- NULL
    #Function to set matrix and removed cached inverse matrix
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    #Function that returns the matrix
    get <- function() m
    #Function to set the cached inverse matrix
    setinv <- function(x) inv <<- x
    #Function that returns the cached inverse matrix
    getinv <- function() inv
    #List of all functions to be returned
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# cacheSolve returns the matrix inverse for a matrix object created
# with makeCacheMatrix.
#
# The argument is the list of functions created by makeCacheMatrix
# The function returns the inverse of the matrix in makeCacheMatrix
cacheSolve <- function(x, ...) {
    #Get the cached inverse
    inv <- x$getinv()
    #If the cached inverse is availible return
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #Get the matrix and calculate the inverse
    m <- x$get()
    inv <- solve(m, ...)
    #Cache the inverse
    x$setinv(inv)
    #Return the inverse
    inv
}
