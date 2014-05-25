## This function creates a matrix, then inverts the matrix and caches the result
## for future use

makeCacheMatrix <- function(x = matrix()) {
  # Clear cache memory  
        m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
        }
      get<-function() x
  # create a matrix and invert it
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
  }
}
