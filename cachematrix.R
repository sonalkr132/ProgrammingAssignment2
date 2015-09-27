# An efficent way to calculate inverse of matrix using cache of computed results

# Function used to cache inverse of matrix x
# returns a list containing set the matrix, get the matrix, get the inverse
# of the matrix and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Function to find the inverse of a given matrix.
# Only does the comptuation if inverse of given matrix was not
# found in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inv)
  inv
}
