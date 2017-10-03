## I have created makeCacheMatrix to make a special "matrix" object, so that cacheSolve can take makeCacheMatrix as an object.
## Therefore, this will lead cacheSolve to retrieve the inverse.

## makeCacheMatrix below will take a matrix as an argument, and will return a list that can be used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse
}


## cacheSolve below will compute the inverse of what is returned by makeCacheMatrix above.

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
