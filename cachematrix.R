## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  invmat <- NULL
  set <- function(y){
    x <<- y
    invmat <<- NULL
  }
  # create the get matrix function
  get <- function() x
  
  #create the set inverse of the matrix function
  setinverse <- function(solve) invmat <<- solve
  getinverse <- function() invmat
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
  
  #get the inverse of the matrix
  inversemat <- x$getinverse()
  
  #check if the cached value exists
  if(!is.null(inversemat)){
    message("Getting Cached Inverse Matrix")
    return(inversemat)
  }
  
  data <- x$get()
  #if the cached value doesnt exist, do the inverse.
  inversemat <- solve(data, ...)
  x$setinverse(inversemat)
  inversemat
}
