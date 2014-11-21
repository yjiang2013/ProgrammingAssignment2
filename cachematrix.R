## Put comments here that give an overall description of what your
## functions do
## These two functions calculate and save the inverse matrix.

## Write a short comment describing this function
## makeCacheMatrix creates a list containing functions to 
## set the initial matrix
## get the initial matrix
## set the inverse matrix
## get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve calculates and returns the inverse of the special matrix 
## created by makeCacheMatrix. 
## It first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix and sets the value of the 
## inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
