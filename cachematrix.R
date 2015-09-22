## This pair of functions can be used to efficiently compute the 
## inverse of a matrix.  Using these functions, the inverse of a
## particular matrix is cached when computed so that the inverse
## does not have to be recomputed unless the matrix changes.
## See the sample calling sequence at the bottom of this file.


## This function creates a vector or list of functions to do
## the following:  (1) set the value of the matrix, (2) get the 
## value of the matrix (3) set the inverse of the matrix (4) 
## get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(solve) s <<- solve
    
    getinv <- function() s
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## This function computes the inverse of the special "matrix"
## created with the function above.  It first checks to see if
## the inverse has already been computed, and if so that cached
## inverse is returned.  Otherwise, the inverse is computed and
## cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    s <- x$getinv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}


## sample calling sequence - note responses
## m <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
## cacheSolve(m)
## cacheSolve(m)
## m <- makeCacheMatrix(matrix(2:5, nrow=2, ncol=2))
## cacheSolve(m)





