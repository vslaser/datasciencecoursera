## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function is storing the cache information. 
## In the first run this function will not come into play because there is no cache
## In the second run, the function will reutn the message and cached data

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## This function is computing the inverse and 
## if the inverse is already created, then it runs the makeCacheMatrix function
## Hence returning the cached data

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
