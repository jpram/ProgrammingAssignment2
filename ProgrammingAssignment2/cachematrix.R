## Matrix inversion is usually a costly computation. 
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

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


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

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
