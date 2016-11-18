# These functions allow you to cache the inverse of a matrix. This calculation is 
# computationally expensive, so it's nice to be able to pull the inverse from a 
# cache if it has already been calculated

# The makeCacheMatrix function returns a list of functions that sets the matrix,
# gets the matrix, sets the matrix inverse, and gets the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


# This function checks to see if the matrix inverse is stored in the cache, if not
# it calculates and returns the inverse. If the inverse is in the cache already,
# the function just returns the cached inverse

cacheSolve <- function(x, ...) {
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
