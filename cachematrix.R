#The makeCacheMatrix() function sets the inverse matrix(inv parameter) to Null. 
# Return a list with 4 functions as elements. (1) The set function: sets x to the matrix input by the user. 
# (2) The get function Gets the matrix details 
# (3) setinverse function : set the inv parameter with the matrix inverse.
# (4) getinverse : return the inv parameter 

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

# CacheSolve take as input a list of  "makeCacheMatrix" which includes the matrix provided, checks if the inverse is already calcuated,
#  if yes,  get  value from the cache with this message  "getting cached data", else calculates the inverse of the matrix.

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


