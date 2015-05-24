## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
  
    ## Initialize the inverse property
    i <- NULL
  
    ## Function to set the matrix
    setMatrix <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
  
    ## Function to get the matrix
    getMatrix <- function() m
    
    ## Function to set the inverse of a matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
 
    ## Function to get the inverse of a matrix
    getInverse <- function() i

    ## Return a list of all the functions  
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,  
         getInverse = getInverse) 
}

## This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed),
## then `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Initialize the inverse of 'x' by checking of it already exists
    m <- x$getInverse()
    
    ## If it already exists, then return it
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }

    ## Get the matrix from 'x'
    data <- x$getMatrix()
    
    ## Calculate the inverse using matrix multiplication
    m <- solve(data)
    
    ## Set the inverse to 'x' and return it
    x$setInverse(m)
    m
}
