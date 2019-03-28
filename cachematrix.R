## Cache the inverse of a matrix


## Create an object that can cache the inverse of an input matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(newmatrix) {
        x <<- newmatrix ## set new matrix values (in parent environment)
        inverse <<- NULL ## clear cached inverse (in parent environment)
    }
    get <- function() x
    setinverse <- function(newinverse) inverse <<- newinverse ## cache new matrix inverse (in parent environment)
    getinverse <- function() inverse
    
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse) ## return list of named functions so cacheSolve can access them
}


## Retrieve the cached inverse of the input matrix from makeCacheMatrix if it exists,
## otherwise calculate the inverse of the input matrix, cache it, and return it

cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    
    ## if cached inverse exists return cached value & exit function
    if (!is.null(inverse)) {
        message("retrieving data from cache")
        return(inverse)
    }

    inverse <- solve(x$get(), ...) ## calculate matrix inverse (if not already cached)
    x$setinverse(inverse) ## cache calculated matrix inverse
    inverse
}
