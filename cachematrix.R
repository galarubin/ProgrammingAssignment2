## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #This will store the cached inverse
    
    #set the matrix and reset the cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #get the matrix
    get <- function() x
    
    #set the cached inverse
    setinverse <- function(inverse) inv <<- inverse
    
    #get the cached inverse
    getinverse <- function() inv
    
    #return a list with all methods
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # Check if inverse is already cached
    
    if(!is.null(inv)) {  # If cached inverse exists, return it
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, compute the inverse
    mat <- x$get()
    inv <- solve(mat, ...)  # Compute the inverse
    x$setinverse(inv)  # Cache the inverse
    inv  # Return the inverse
}
