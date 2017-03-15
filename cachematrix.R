## For a given matrix, calculate the inverse matrix. 

## The function makeCacheMatrix create a special matrix that can calc his inverse.
## cacheSolve function calculate the inverse matrix that was created by the makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
        mxi <- NULL
        set <- function(y = matrix()) {
                x <<- y
                mxi <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mxi <<- solve
        getinverse <- function() mxi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        mxi <- x$getinverse()
        if(!is.null(mxi)) {
                message("getting cached data")
                return(mxi)
        }
        data <- x$get()
        mxi <- solve(data)
        x$setinverse(mxi)
        mxi
}