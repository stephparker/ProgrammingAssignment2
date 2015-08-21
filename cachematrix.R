## Creating a special matrix that can cache its inverse in order to save time
##1. set value of matrix
##2. get value of matrix
##3. set value of inverse of matrix
##4. get value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
            x <<- y   # <<- assigns value to an object in a different environment
            m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve)
                m <<- solve
        getmatrix <- function() 
                m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
          }


## Computes the inverse of the special matrix returned by 'makeCacheMatrix' above

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        #if the inverse has already been calculated:
        if(!is.null(m)) {
            message("getting cached data")  #get data from cache and skip computation
            return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)  #sets inverse in the cache
        m
}
