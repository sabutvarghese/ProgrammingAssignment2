makeCacheMatrix <- function(x = matrix()) {
  ##============================================================================
  ## Author: Sabu Varghese
  ## Date: 10/22/2014
  ## Function Arguments:
  ##     x - matrix
  ##
  ## Purpose: Cache the matrix passed as argument and returns a list that
  ## contains setters/getters for matrix and its inverse
  ##============================================================================

  matInverse <- NULL
  
  ## Set function for the new matrix. Assign the input matrix to x and reset the
  ## value of matInverse; note that when calling the set function from outside,
  ## variables will be in the enclosing environment, hence <<-
  set <- function(y) {
    x <<- y
	matInverse <<- NULL 
  }

  ## Get function; returns the value of matrix in the cache
  get <- function() { x }

  ## Assign inverse of the matrix to setMatInverse; note that when calling the 
  ## set function from outside, variables will be in the enclosing environment,
  ## hence <<-
  setMatInverse <- function(mInv) {
    matInverse <<- mInv 
  }

  ## Get function; returns the value of matInverse
  getMatInverse <- function() {
    matInverse 
  }

  ## Create list containing setters/getters
  list(
    set = set, 
    get = get,
    setMatInverse = setMatInverse,
    getMatInverse = getMatInverse
	)
}

cacheSolve <- function(x, ...) {
  ##============================================================================
  ## Author: Sabu Varghese
  ## Date: 10/22/2014
  ## Function Arguments:
  ##     x - list; result from makeCacheMatrix()
  ##
  ## Purpose: Get a special vector of setters/getters from the makeCacheMatrix.
  ## If inverse is already calculated for the matrix, return the inverse from 
  ## memory cache. If not already calculated, find and return the inverse of the
  ## matrix. 
  ##============================================================================
  
  mi <- NULL
  
  ## See if the inverse of the matrix exists in cache
  matInverse <- x$getMatInverse()
  
  ## If inverse of the matrix exists in cache, assign to mi; else compute
  ## the inverse of the matrix using solve() function and assign the result to mi
  if (!is.null(matInverse)) { 
    print ("Inverse already calculated for this matrix...getting cached inverse")
    mi <- matInverse
  } else {
    mat <- x$get()
    matInverse <- solve(mat, ...)
    x$setMatInverse(matInverse)
    mi <- matInverse
  }

  ## Return the inverse of the matrix
  return (mi)
  
}
