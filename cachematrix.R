## makeCacheMatrix creates a matrix object that can cache its inverse.
## cacheSolve solves the inverse matrix of makeCacheMatrix. 
## If the inverse has been calculated, then cacheSolve function returns the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL # sets the value of m to NULL as a default
    
    set <- function(y){ #set the value of the matrix
        x <<- y # caches the input matrix
        m <<- NULL
    }
    
    get <- function(){
        x
    }    
    setmat <- function(solve){
        m <<- solve
    }
        
    getmat <- function(){
        m
    }
    list(set = set, get = get, setmat = setmat, getmat = getmat) # creates a list to store all the functions
}

# Calculates new input or if the inverse has already been calculated, then retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    m <- x$getmat()
    
    if(!is.null(m)){ # check to see if cacheSolve has been run before
        return(m)
    }
    
    matrix <- x$get()
    m <- solve(matrix, ...) # compute the value of the inverse of the input matrix
    x$setmat(m) # cache the inverse
    m # returns the inverse
}
        
