
## This funciton basically returns a list of functions
## for you to manipulate cached inverse of a matrix

makeCacheMatrix<- function(m=matrix()) {
  inv.m <- NULL
  setm <- function(y) {
    m <<- y
    inv.m <<- NULL    ## Initialize inverted matrix to NULL again
  }
  getm <- function() m
  setInvMatrix <- function(invM) inv.m <<- invM
  getInvMatrix <- function() inv.m
  list(setm = setm, getm = getm,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

## This function calculate inverse of a matrix
## Arguments:
## m - a matrix to be inversed (note I added this argument for flexibility)
## f - the list of functions returned from makeCacheMatrix function

cacheSolve <- function(m,f, ...) {
  ## first get the cached matrix
  data.m <- f$getm()
  
  ## check if the cached matrix has changed
  ## if not changed, check if the inverse already computed and cached
  if (identical(m, data.m)) {
    inv.m <- f$getInvMatrix()
    if(!is.null(inv.m)) {
      message("getting cached data")
      return(inv.m)
    }
  }
  ## EITHER the matrix is changed OR the inverse is not calculated yet in cache
  ## calculate the inverse of the new matrix m
  f$setm(m)   ## stores in the new matrix in cache, next f$getm() will return this
  inv.m <- solve(m, ...)
  f$setInvMatrix(inv.m) ## stores the computed inverse in cache
  inv.m
}
