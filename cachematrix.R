## A pair of matrix that caches the inverse of a matrix

## 1st function creates matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## 2nd function computes the inverse of matrix returned by previous function 

cacheSolve <- function(x, ...) {
  
  m <= x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mtx <- x$get()
  m <- solve(mtx, ...)
  x$setinverse(m)
  m
 
}
