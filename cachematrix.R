## Put comments here that give an overall description of what your
## functions do

## makeCachedMatrix is a function that creates a special matrix object, which is really
## a list containing functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(invs) inverse <<- invs
	getInverse <- function() inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## The cachedSolve computes the inverse of the special "matrix", created by makeCachedMatrix.
## First, it check if the inverse of the matrix has been computed. If so, it gets the inverse
## from the cache and return it. Otherwise, it computes the inverse of the matrix and set the
## inverse of the matrix in the cache via the setInverse function, and then return it.

cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if (!is.null(inverse)) {
		message("getting cached inversed matrix")
		return(inverse)
        ## Return a matrix that is the inverse of 'x'
	}
	matrix <- x$get()
	inverse <- solve(matrix, ...)
	x$setInverse(inverse)
	inverse
}
