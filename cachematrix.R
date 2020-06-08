## Put comments here that give an overall description of what your
## functions do

## make CacheMatrix takes a matrix and returns a list of fucntions
## get, set, getinverse, setinverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inver) i <<- inver
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix with global caching
## it searches in the attributes of x and returns cached inverse of x if any
## otherwise it calculates the inverse of x with the built-in inverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data) 
  x$setinverse(i)
  i
}
