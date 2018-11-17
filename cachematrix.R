
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        # initialize m
        m <- NULL
        
        # sets the input matrix x based on input given (y) and resets m
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # gets matrix x
        get <- function() x
        
        # sets m above to inverse (run by function below)
        setinverse <- function(inverse) { m <<- inverse }
        
        # retrieves inverse from m
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        # sets m to inverse
        m <- x$getinverse()
        
        # pulls inverse is m contains it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # gets matrix
        data <- x$get()
        
        # sets m to inverse
        m <- solve(data, ...)
        
        # from input matrix x, sets inverse to m
        x$setinverse(m)
        
        # inverse is returned
        m
        
}
