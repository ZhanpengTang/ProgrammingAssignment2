## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Setting the matrix
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    ## Setting the inverse of the matrix
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    ## Getting the inverse of the matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## matrix returned by `makeCacheMatrix` above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    ## Get the matrix from our object
    data <- x$get()
    ## Calculate the inverse using matrix multiplication
    m <- matrix(data, ...)
    ## Set the inverse to the object
    x$setmatrix(m)
    m
}

## Testing the function
m <- matrix(rnorm(1:9),3,3)
m
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
