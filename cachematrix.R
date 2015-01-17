## Matrix with cached inverse
## ==========================
## The following two functions work together to provide an 
## extended matrix where the inverse is cached so it's no
## recalculated everytime it's needed.
##
## In order for this to be useful, the trade-off to be analysed
## depends on the ratio between matrix inversions / modifications to
## the "to be inverted" matrix (naturally, each modification flushes
## the cache of the previous inversion).

## Create an instance of the extended matrix, given a plain vanilla one.
makeCacheMatrix <- function(x = matrix()) {
  # Empty cache.
  inv <- NULL
  # Function to set the inner matrix.
  set <- function(y) {
    # Sets the inner matrix.
    x <<- y
    # Flushes the cache.
    inv <<- NULL
  }
  # Function to retrieve the inner matrix.
  get <- function() x
  # Function to set the cache.
  setinv <- function(invPar) inv <<- invPar
  # Function to retrieve the cache.
  getinv <- function() inv
  # Pack everything in a list.
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Helper function to compute the inverse (and populate the cache while doing so).
# Parameter x: extended matrix object created with makeCacheMatrix.
cacheSolve <- function(x, ...) {
  # Check the cache,
  inv <- x$getinv()
  if(!is.null(inv)) {
    # debug message
    message("getting cached data")
    return(inv)
  }
  # Since there's no cached data:
  # retrive the inner matrix ...
  data <- x$get()
  # compute the inverse ...
  inv <- solve(data)
  # populate the cache ...
  x$setinv(inv)
  # return the value for this call.
  inv
}
