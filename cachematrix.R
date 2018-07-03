## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special list of functions to set and get a value of a matrix and functions to set its inverse
## and get its inverse.
##
## Example:
## a <- makeCacheMatrix()
## b <- matrix(c(1,0, 0,1), nrow=2, ncol=2)
## a$set(b)

makeCacheMatrix <- function(x = matrix()) {
		inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_m <<- inverse
        getinverse <- function() inv_m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## This function calculates the inverse of the given matrix. If the value is already available,
## the cached version is used.
## Example:
## c <- cacheSolve(a)
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
