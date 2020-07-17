## This function, makeCacheMatrix creates a special "matrix", which is really a list containing a 
# function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

## Write a short comment describing this function
# input: A matrix x
# output: A special object for cache

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


# The following function calculates the inverse of the special "matrix" created with makeCacheMatrix. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
# the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
# of the inverse in the cache via the setInverse function

## Write a short comment describing this function
# input: A special created object makeCacheMatrix applied on X
# Output: A inverse of matrix X

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
