# The first function, makeCacheMatrix creates a special “vector”, which is 
# really a list containing a function to
# 
#   1) set the value of the matrix
#   2) get the value of the matrix
#   3) set the value of the inverse
#   4) get the value of the inverse
#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL            #initializing the value of the inverse matrix 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x       #get the value of the matrix
  set_Inverse <- function(inverse) inv <<- inverse
  get_Inverse <- function() inv 
  list(set = set, get = get,             #output as a list of functions
       set_Inverse = set_Inverse, 
       get_Inverse = get_Inverse)
}

# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_Inverse()
  if(!is.null(inv)){    #Return inverse matrix if the value is not NULL
    message("getting cached data")
    return(inv)                     
  }
  mat <- x$get()          
  inv <- solve(mat,...)     #solve for inverse matrix
  x$set_Inverse(inv)
  inv
}