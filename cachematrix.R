## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        my_inverse <- NULL
        set <- function(y) {
                x <<- y
                my_inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) my_inverse <<- inverse
        getInverse <- function() my_inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        my_inverse <- x$getInverse()
        if (!is.null(my_inverse)) {
                message("getting cached data")
                return(my_inverse)
        }
        my_matrix <- x$get()
        my_inverse <- solve(my_matrix, ...)
        x$setInverse(my_inverse)
        my_inverse
}
