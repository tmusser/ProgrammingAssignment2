## This function creates a special "matrix" object that can cache its inverse
## To use, send a matrix as an input argument:
## Usage example:
## m <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## Which will create an object which can be displayed as:
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix value
  inv <- NULL
  # set the value of the regular matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the regular matrix
  get <- function() x
  # set the value of the inverse matrix
  set_inverse <- function(inverse) inv <<- inverse
  # get the value of the inverse matrix
  get_inverse <- function() inv
  # return a list of the preceeding functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## This function takes a matrix as in the input and computes
## the inverse
## Usage example using our already created m matrix:
## > cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

cacheSolve <- function(x, ...) {
  # Check if inverse cache has been computed
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    # if created we directly return the inverse
    return(inv)
  }
  # Otherwise, get the matrix
  data <- x$get()
  # Compute the inverse
  inv <- solve(data, ...)
  # Cache the inverse of the matrix
  x$set_inverse(inv)
  # Return the newly computed result
  inv
}
