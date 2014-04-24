## These functions stores a numeric matrix and cache's its inverse.

## This function returns a list of functions that 1) set the matrix 2) get the matrix
## 3) set the inverse of the matrix 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the matrix, but firsts checks to see if it has been solved.
## If it has been solved, it returns the cached matrix instead of calculating it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
