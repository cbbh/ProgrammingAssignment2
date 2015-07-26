## uses lexical scoping to cache the inverse of a square invertible matrix
## using a pair of functions

## creates a "matrix" object that can cache its inverse
## returns a list of four functions

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## computes the inverse of the "matrix" returned by makeCacheMatrix
## If inverse is already calculated, function returns to stored data
## If not, calculates using the Solve function 

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