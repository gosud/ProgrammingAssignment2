## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
invrs <- NULL
  
  #set the the matrix
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  
  #get the matrix
  get <- function() x
  
  #set the inverse
  setinverse <- function(inverse) invrs <<- inverse
  
  #get the inverse
  getinverse <- function() invrs
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #find out if the inverse has already been calculated
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cache data")
    return(invrs)
  }
  
  #if the matrix hasn't already been inverted, do so
  #get the original matrix
  data <- x$get()
  #calculate the inverse
  invrs <- solve(data, ...)
  
  #set the inverse
  x$setinverse(invrs)
  
  #return the inverse
  invrs
}
