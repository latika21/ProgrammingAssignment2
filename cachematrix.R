## Put comments here that give an overall description of what your
## functions do
## The following functions cache the inverse of a matrix to save computational power were possible
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## Write a short comment describing this function
## Cachesolve calculates the inverse of the special matrix if it doesn't exsists. 
## If the inverse already exsist then retrieve it without computing
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
