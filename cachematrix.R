## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ### This function creates a special "matrix" object that can cache its inverse.
  
  ## input: an invrertable already inverted matrix
  
  ##output: a special "vector", which is really a list containing a function to
  
  # set the value of the vector             - setmatrix
  # get the value of the vector             - getmatrix
  # set the value of the inversed matrix    - setinverse
  # get the value of the inversed matrix    - getinverse
  
  # <<- operator can be used to assign a value to an object in an environment that is different from the current environment
  #this output is the input to cacheSolve()
  
  cached_inv_matrix <- NULL
  setmatrix <- function(y) {
                            x <<- y
                            cached_inv_matrix <<- NULL
                            }
  getmatrix <- function() x
  setinverse <- function(inverse) cached_inv_matrix <<- inverse
  getinverse <- function() cached_inv_matrix
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


  ## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cached_inv_matrix <- x$getinverse()
  if(!is.null(cached_inv_matrix)) {
    message("getting cached data")
    return(cached_inv_matrix)
  }
  data <- x$getmatrix()
  cached_inv_matrix <- solve(data, ...)
  x$setinverse(cached_inv_matrix)
  cached_inv_matrix
}
