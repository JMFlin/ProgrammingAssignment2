## makeCacheMatrix creates a matrix object that can cache its inverse
## cacheSolve solves the inverse matrix of makeCacheMatrix. 
## If the inverse has been calculated, then this function returns the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setmat <- function(solve) m <<- solve
    getmat <- function() m
    list(set = set, get = get, setmat = setmat, getmat = getmat)
}

cacheSolve <- function(x, ...) {
    
    m <- x$getmat()
    
    if(!is.null(m)){
        return(m)
    }
    
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmat(m)
    m
}
