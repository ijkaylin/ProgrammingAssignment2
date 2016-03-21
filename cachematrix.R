#Because matrix inversion is a costly computation, we want to cache the inverse
# of the matrix rather than compute it repeatedly. The following two functions 
#will cache the inverse of the matrix

## creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse<- NULL
    set<- function(y) {
        x<<- y
        inverse<<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse<<- inv
    getInverse<- function() inverse
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


#cacheSolve(): computes the inverse of the special 'matrix' returned by the function 
#above and retrieved from the cache
# NOTE: solve(X) returns the inverse of a square matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse<- x$getInverse()
    if(!is.null(inverse)){
        message('Getting cached inverse matrix data')
        return(inverse)
    }
    mat<-x$get()
    inv<- solve(mat, ...)
    x$setInverse(inv)
    inv
}
