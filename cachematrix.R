## Creates two functions to cache the inverse of matrix 'x' to reduce computation time

## makeCacheMatrix creates a cache and named list in the global environment. 

makeCacheMatrix <- function(x = matrix()) {
        CacheInv <- NULL        ##creates an empty matrix to store inverse         
        set <- function(y) {
                x <<- y
                CacheInv <<- NULL   ##creates an empty matrix to store inverse in different environment
        }
        get <- function() x
        setInv <- function(inverse) CacheInv <<- inverse
        getInv <- function() CacheInv
        list(set = set, get = get,      #stores functions and inverse matrix in a list
             setInv = setInv,
             getInv = getInv)
}

##cacheSolve inverts a matrix if it's inverse is not already stored in the cache
##and retreives the inverse is already cached.

cacheSolve <- function(x, ...) {
        
        invFunc <- x$getInv()       ##retreives getInv the named list
        if(!is.null(invFunc)) {      ##checks if the cache is not empty
                message("getting cached data")
                return(invFunc)   ## Returns the cached matrix and ends the function
        }
        data <- x$get()     ##if cache is empty, retrieve matrix 'x'
        invFunc <- solve(data, ...)  ##inverts matrix 'x'
        x$setInv(invFunc)  #sets the inverse of matrix 'x' into setInv
        invFunc    ##returns inverse of matrix 'x' 
}