## Functions that create special matrix object that caches given invertble matrix
## calculating its inverse, if it is not already cached
##   

## Description: Function creates special matrix object that can cache inverse of
## a non singular square matrix given along with input matrix
## Input Argument: Square matrix  
## Returns: None
## Assumption: Matrix given is a non sngular (detrminant is non-zero) and can be invertble


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(inv) m <<- inv
  getInverseMatrix <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

## Description: Function to calculate inverse of a matrix that can be invertible, 
## using the spcial matrix object created with makeCacheMatrix function.
## Check if inverse matrix exists in the geiven matrix object, if not present,
## computes inverse and caches it in the matrix object, returns the same. If present,
## returns the cached inverse matrix
## Arguement: Matrix object created with makeCacheMatrix
## Returns: Inverse of matrix with which matrix object is created
## ## Assumption: Matrix obk=ject contain non-singular matrix(detrminant is non-zero)
## and can be invertble
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(is.matrix(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
}
