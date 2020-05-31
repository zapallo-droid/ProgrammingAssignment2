## This two functions calculate the inverse of a matrix and cache the result of the operation.

#Function 1 <- makeCacheMatrix <- This function take a invertible matrix as argument and make a "special" matrix
#(a list of values) to set and get the result of the inverse using the solve function and storage the result in
#other enviroment through the operator <<- 

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
get <- function() x
setinverse <- function(solve) I <<- solve
getinverse <- function() I
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


##Function 2 <- cacheSolve <- This function take the matrix and evaluate if the inverse is already calculated,
##in those cases when the inverse is in the cache return the value and in those cases where the inverse doesn't
##exists or when the matrix has change return a new value for the inverse matrix.

cacheSolve <- function(x, ...) {
  I <-x$getinverse()
  A <-x$get
  if(!is.null(I) && x == A) {
    message("getting cached data from makeCacheMatrix")
    return(I)
  }
  data <- x$get()
  I <- solve(data,...)
  x$setinverse(I)
  I
}
