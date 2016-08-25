## Matrix inversion can be a costly compution -- so caching may be of signficant benefit.
## The two functions below will 1) create a special matrix object that can cache its inverse, and 
## 2) either compute/retrieve the inversion depending on whether it has been calculated already.

## This function creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invMat <<- inverse
  getinverse <- function() invMat
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## This function computes the inverse of the special matrix above; unless it has already been created. 
## In the latter case, it will be retrieved from cache.

cacheSolve <- function(x, ...) {
  invMat <- x$getinverse()
  if(!is.null(invMat)) {
    message("Getting cached data!")
    return(invMat)
  }
  d <- x$get()
  invMat <- solve(d)
  x$setinverse(invMat)
  invMat
}