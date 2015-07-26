## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix: The input to this function is an invertible, square matrix. It will return a list of functions that will set
## the matrix, get the matrix, set the inverse, and get the inverse. This list will be used as the input to the function below, cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

##cacheSolve: The output of the above function will be used as the input to this function. This function will return 
## the inverse of the matrix that was orginially the input to the function above, makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
