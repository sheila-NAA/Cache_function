## The functions below are meant to create a square invertible cached matrix.


## This code will return a list that will set the matrix,
##get the matrix set the inverse of the matrix and get the inverse
##This reuslt will be the input for the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <-function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## this code below is designed to produce the inverse of the makeCacheMatrix() input

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data <- x$get()
  inv <- solve(mat.data,...)
  x$setinv(inv)
  return(inv)
}
