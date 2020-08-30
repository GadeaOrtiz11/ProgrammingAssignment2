## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
            set <- function(y) {
            x <<- y
            inv <<- NULL
            ## <<- operator used to assign a value to an object in an 
            ## environment that is different from the current environment.
    }
    get <- function() x
    set_inv <- function(inverse)  inv <<- inverse
    get_inv <- function() inv
    list(set = set, get = get,
         setmean = set_inv,
         getmean = get_inv)

}

## cacheSolve:
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
            if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
        inv
}
