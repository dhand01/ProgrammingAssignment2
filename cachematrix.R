## these functions are designed to create objects that can: 
## store a matrix
## store the inverse of a matrix
## solve the inverse of a matrix or return the cached version if available

## this function creates a special "matrix" object that can:
## get/set a matrix value
## get/set its inverse value

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get        <- function()    x
    setInverse <- function(inv) i <<- inv
    getInverse <- function()    i
    
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## this function computes the inverse of the matrix held in the special 
## CacheMatrix object. If the CacheMatrix object already has a cached value
## it is simply returned. Otherwise, the inverse is calculated and cached

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of x$get()
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
