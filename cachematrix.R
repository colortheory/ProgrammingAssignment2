## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## The following two functions cache the inverse of a matrix.

##### makeCacheMatrix() #####
## This function creates a special "matrix" object that can cache its inverse.
##
## Args ##
## x: the matrix object
##
## Returns ##
## A list of functions
#####

makeCacheMatrix <- function(x = matrix()) {
	
	m <- NULL
	
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x

	setinverse <- function(solve) m <<- solve

	getinverse <- function() m

	list(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

##### cacheSolve #####
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix not changed), then the cachesolve should retreive the inverse from the
## cache.
##
## Args ##
## x: the special "matrix" returned from makeCacheMatrix
##
## Returns ##
## The inverse matrix
#####

cacheSolve <- function(x, ...) {
	
	m <- x$getinverse()

	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	data <- x$get()

	m <- solve(data, ...)

	x$setinverse(m)

	m
}