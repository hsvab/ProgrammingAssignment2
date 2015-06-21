# The function makeCacheMatrix stores the matrix M and its inverse.
# The variable inverse_matrix is NULL as default.
# The function 'set' changes the M matrix (keeping the inverse_matrix as NULL).
# The function 'setinverse' changes the inverse_matrix matrix.
# The function 'get' returns M matrix.
# The function 'getinverse' returns inverse_matrix matrix.

makeCacheMatrix <- function(M = matrix())
{

  inverse_matrix <- NULL
  
  set <- function(new_matrix)
  {
    M <<- new_matrix
    inverse_matrix <<- NULL
  }
  
  setinverse <- function(inverse)
  {
    inverse_matrix <<- inverse
  }
  
  get <- function() M
  
  getinverse <- function() inverse_matrix
  
  list(set = set, setinverse = setinverse, get = get, getinverse = getinverse)
}

# The function cacheSolve returns the inverse of matrix M.
# The logical test verifies if the inverse is already stored.
# If it is (so, inv is not NULL), the inverse matrix in cache is returned.
# If it is NULL, the solve function is applied, getting the M matrix
# through the 'get' function of the makeCacheMatrix.
# At the end, the cacheSolve uses the 'setinverse' function to save
# the inverse matrix in cache and returns it.

cacheSolve <- function(x)
{
  inv <- x$getinverse()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}