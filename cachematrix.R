# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mc<-NULL
  set<-function(y){
    x<<-y
    mc<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) mc<<- solve
  getmatrix<-function() mc
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  mc<-x$getmatrix()
  if(!is.null(mc)){
    message("getting cached data")
    return(mc)
  }
  matrix<-x$get()
  mc<-solve(matrix, ...)
  x$setmatrix(mc)
  mc
}
