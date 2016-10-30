# Functions for getting and setting the inverse of a (cached) matrix and
# also calculating the inverse if not cached.

# makeCacheMatrix creates cacheable matrix for suitable input
# into cacheSolve() function which sets and gets
# the cached values

makeCacheMatrix <- function(org.mat = matrix()) {

  inv.mat <- NULL

  set <- function(y) {
    org.mat <<- y
    inv.mat <<- NULL
  }

  # get and set the cached values of the matrix
  get <- function() org.mat
  # getting the inverse of the matrix using the internal solve-function
  set.inv <- function(solve) inv.mat <<- solve
  get.inv <- function() inv.mat

  list(
    set = set,
    get = get,
    set.inv = set.inv,
    get.inv = get.inv)

}


# cacheSolve computes the inverse of a matrix. Depending if the matrix is cached,
# cacheSolve just ooks up the value. If the matrix is not cached the inverse is
# calculated with the internal solve()-function.

cacheSolve <- function(cache.mat, ...) {
  inv.mat <- cache.mat$get.inv()
  # Check if cached matrix exists. If yes, function returns its value and ends
  if(!is.null(inv.mat)) {
    message("Getting the cached inverse matrix")
    return(inv.mat)
  }
  # If there is no cached matrix, one is created and stored as the current cached matrix
  # and is also returned.
  mat.to.inv <- cache.mat$get()
  inv.mat <- solve(mat.to.inv)
  cache.mat$set.inv(inv.mat)
  inv.mat

}
