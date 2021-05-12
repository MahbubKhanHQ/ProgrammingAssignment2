## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The inverse of the special "matrix" generated with the above function is 
## calculated with this function. It search to see if the matrix's inverse 
## has already been calculated. If this is the case, it retrieves the 
## matrix's inverse from the cache and skips the operation. If not, it uses 
## the "setinverse function" to determine the inverse of the matrix and store 
## the value of the inverse of the matrix in the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # when inverse is not calculated
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
