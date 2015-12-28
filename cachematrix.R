##This pair of functions can cache the inverse of a matix, because matrix inversion is a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix creates a special "matrix", which stores a matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x<<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set,
		  get = get,
		  setinverse = setinverse,
		  getinverse = getinverse)
}


## cacheSolve function can computes a matrix's inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
