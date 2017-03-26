## Function wrappers to aid in the calculation of a matrix, caching the matrix where possible.

## Returns a list of functions to get/set the matrix and inverse matrix. Set functions cache
## the given matricies.
makeCacheMatrix <- function(mat = matrix()) {

  inverseMat <- NULL
  setMat <- function(newMat) {
    mat <<- newMat
    inverseMat <<- NULL
  }
  
  getMat <- function() mat
  
  setInverseMat <- function(newInverseMat) {
    inverseMat <<- newInverseMat
  }
  
  getInverseMat <- function() inverseMat
  
  list(setMat = setMat,
       getMat = getMat,
       setInverseMat = setInverseMat,
       getInverseMat = getInverseMat)
}

## Takes a list of functions (created by makeCacheMatrix) and returns
## the 'solved' (inverse) matrix.
cacheSolve <- function(x, ...) {
  
  inverseMat <- x$getInverseMat()
  if(!is.null(inverseMat)) {
    message("getting cached data")
    return(inverseMat)
  }
  
  mat <- x$getMat()
  
  inverseMat <- solve(mat)
  x$setInverseMat(inverseMat)
  inverseMat
}