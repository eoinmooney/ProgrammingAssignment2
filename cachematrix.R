## This function makes a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inverse_x <<- NULL
}
get <- function() x
setinverse <- function(inverse) inverse_x <<- inverse
getinverse <- function() inverse_x
list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## The function below returns the inverse of a Matrix that was created with 
## the above makeCacheMatrix. If the inverse is in the cache it will return that
## else it will compute the inverse and return it. And cache the result

cacheSolve <- function(x, ...) {
inverse_x <- x$getinverse()
if (!is.null(inverse_x)) {
        message("getting inverse matrix from cache")
        return(inverse_x)
} else {
        inverse_x <- solve(x$get())
        x$setinverse(inverse_x)
        return(inverse_x)
}
}
