#The makeCacheMatrix function sets the inverse matrix to Null.
#Sets x to matrix input by the user
#Gets the matrix details
#Sets and gets inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

#CacheSolve reads the matrix provided, checks if the inverse is already calcuated, if yes, 

#retrieves value from the cache, else calculates the inverse of the matrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}


