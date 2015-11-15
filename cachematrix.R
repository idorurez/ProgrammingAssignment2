## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # create space
  mInverse <- NULL
  
  # set matrix
  set <- function(mnew) {
    x <<- mnew  
    mInverse <<- NULL
  }
  
  # get matrix
  get <- function() x
  
  # set inverse of matrix
  setinverse <- function(x) mInverse <<- x
  
  # get inverse of matrix
  getinverse <- function() mInverse
  
  # create output
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # get the inverse of the matrix
  mInverse <- x$getinverse()
  
  # check to see if it's null
  if (!is.null(mInverse)) {
    message("getting cached data") 
    return(mInverse)
  }
  
  # if it is null, get the matrix, then calculate the inverse
  m <- x$get()
  
  mInverse <- solve(m) 
  
  x$setinverse(mInverse)
  ## Return a matrix that is the inverse of 'x'
  
  mInverse

}
