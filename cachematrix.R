## Caching the inverse of the matrix

## Function to create a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Function to compute the inverse of the special matrix object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinverse()
  if(!is.null(inv_mat))
  {
    message('getting cached data')
    return(inv_mat)
  }
  mat <- x$get()
  inv_mat <- solve(mat)
  x$setinverse(inv_mat)
  inv_mat
}