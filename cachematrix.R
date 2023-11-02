## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix - making a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setmatrix <- function(inv) i <<- inv
  getmatrix <- function() i
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function
#cacheSolve - function which takes a matrix as a variable and evaluates whether the inverse has been solved
  #if itthe inverse has been calculated, return a message, if not find the inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getmatrix()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setmatrix(i)
  i
}
