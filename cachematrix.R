##This function will makeCacheMatrix creates a list containing a function that will do four things.

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        
        ## sets the value of the matrix
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## gets the value of the matrix
        
        get <- function() x
        
        ## sets the value of the inverse
        
        setinverse <- function(inverse) m <<- inverse
        
        ## gets the value of the inverse
        
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function will return the inverse of matrix or use a cached solution if the inverse has already been solved in the current run

cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
        m <- x$getinverse ()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse (m)
        m
}
