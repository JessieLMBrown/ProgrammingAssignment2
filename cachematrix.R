## Using the functions MakeCacheMatrix together with cacheSolve together we 
## can cache the inverse of a matrix. 

## The function makeCacheMatrix creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The function cacheSolve computes the inverse of the matrix returned by the 
## makeCacheMatrix function above. If the inverse has already been computed it 
## does not recompute it, it just returns the cached value.  

cacheSolve <- function(x, ...) {
       
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}


    
