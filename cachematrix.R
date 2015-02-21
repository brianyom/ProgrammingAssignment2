## Put comments here that give an overall description of what your
## functions do
## The functions can cache the inverse of a matrix.

## Write a short comment describing this function
## makeCasheMatrix creates a list that can set and get the value of the matrix. Also, it can set and get the value of inverse of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		ivn <- NULL
	    set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function

## This function returns the inverse of the matrix. It checks if the inverse has already been computed or not. If it did, it will skip the computation. Other than that, it will compute the inverse and set the value.

cacheSolve <- function(x, ...) {
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
