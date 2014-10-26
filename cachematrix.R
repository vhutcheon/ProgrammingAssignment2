## Creates a "special" matrix whose inverse will be stored in cache.  
## 
## Creates a"special" matrix with a list of functions that operate on it to
## set it, retrieve it, set the cache, and retrieve the cache.
makeCacheMatrix <- function(x = matrix()) {
        ## creates a matrix of NAs to hold inverse.  No. of columns is no. of rows
        ## in "special" matrix and vice versa.
        m <- matrix(data = NA, nrow = dim(x)[2], ncol = dim(x)[1])
        ## set function creates "special" matrix x
        set <- function(y) {
                x <<- y
                ## clears cache when the "special" matrix is changed
                m <<- matrix(data = NA, nrow = dim(x)[2], ncol = dim(x)[1])
        }
        ## retrieves "special" matrix x
        get <- function() x
        ## calculates and caches inverse of matrix
        setinverse <- function(solve) m <<- solve
        ## retrieves matrix from cache
        getinverse <- function() m
        # list of functions that can operate on special matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes and caches the inverse of the "special" matrix 'x' if one is not cached.  
## Returns inverse from cache if available or from computation if not previously cached.
## Assumes matrix is invertible.
cacheSolve <- function(x, ...) {
        ## next statement gets the current value of the inverse matrix - will be matrix
        ## of NAs if inverse has not previously been cached or the value in cache otherwise
        m <- x$getinverse()
        ## the next statement checks to see if the inverse matrix m is not identical
        ## to a matrix of all NAs - i.e., if it was cached.  If cached, it returns the
        ## cached value.
        if(!identical(m, matrix(data = NA, nrow = dim(m)[1], ncol = dim(m)[2]))) {
                message("getting cached data")
                return(m)
        }
        ## only reach the following code if the inverse has not been cached previously
        ## First step is to retrieve the matrix and store in 'data'
        data <- x$get()
        ## calculates the inverse of 'data' and stores in 'm'
        m <- solve(data, ...)
        ## caches the inverse matrix 'm' using the setinverse function
        x$setinverse(m)
        ## returns the calculated inverse matrix
        m
}
