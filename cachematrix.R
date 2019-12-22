## We create a function to create a "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse)i <<- inverse
  getinverse <- function() i
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve retrieves the inverse of the special "matrix" returned by makeCacheMatrix
##If the matrix's inverse has already been calculated then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}


