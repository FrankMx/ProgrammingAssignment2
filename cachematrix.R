## This function returns a list of four functions creating the a special object "CacheMatrix" that 
## can get or set its value (the matrix itself), and can get or set its inverse matrix.

#The first time a special "CacheMatrix" is created executing the function makeCacheMatrix, 
# the inverse matrix I is defined as NULL.

makeCacheMatrix <- function(M = matrix()) {
    I <- NULL
    set <- function(y) 
    {
        M <<- y
        I <<- NULL
    }
    get <- function()
    {
        M  
    } 
    setInverse <- function(value)
    {
        I <<- value
    }
    getInverse <- function()
    {
        I
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function that returns the inverse matrix of 'x', with 'x' being a special 
## object "CacheMatrix" created as a result of the function makeCacheMatrix

cacheSolve <- function(x, ...) {
    
    I <- x$getInverse()
    if(!is.null(I)) {
        print("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setInverse(I)
    I
}
