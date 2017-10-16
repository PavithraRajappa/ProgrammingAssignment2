## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## This funciton creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { ## define the arg with default mode of matrix.
  inv <- NULL  ## inv=null, will keep hold the inverse matrix value
  set <- function(y) {  ## define the set function to assign new 
    x <<- y    ## value of matrix in parent environment
    inv <<- NULL  ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x ## define the get fucntion - returns value of the matrix argument
  setinv <- function(solveMatrix) inv <<- solveMatrix  ## assigns value of inv in parent environment
  getmat <- function() inv ## gets the value of inv where called
  list(set = set, get = get,
       setinv = setinv,
       getmat = getmat)  ## you need this in order to refer to the functions with the $ operator
}

## the below function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getmat()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
