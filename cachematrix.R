## In combination, these two functions (a) create a special pseudo-matrix object 
## capable of cacheing a copy of its own inverse; and (b) determine whether the
## matrix inverse has been calculated, and calculate it if it is not already
## present

## makeCacheMatrix creates a special "matrix" capable of storing a cache of its
## own inverse. When run, this function returns a list of functions to
## (a) Establish the values of this matrix object; (b) Retrive the values of the
## matrix object; (c) Set the inverse of the matrix; and (d) retrieve the inverse
## of the matrix (if already calculated)

makeCacheMatrix <- function(x = matrix()) {
    
  # Default the inverse matrix to NULL
  inverse_mat <- NULL
  
  # Create set function (to establish the matrix values & reset inverse_mat to NULL)
  set <- function(y){
    x <<- y
  }
  
  # Create get function (to retrieve the matrix)
  get <- function(){
    x
  }
  
  # Create setinverse function (to set the inverse of the matrix)
  setinverse <- function(inv){
    inverse_mat <<- inv
  }
  
  # Create getinverse function (to retrieve the inverse value)
  getinverse <- function(){
    inverse_mat
  }
  
  # Return a list containing these four functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
 
}


## for a matrix of the generated with the makeCacheMatrix function,
## cacheSolves returns a matrix that is the inverse of 'x' and stores it to the
## cache in the original matrix

cacheSolve <- function(x, ...) {
  # Check whether the inverse has already been determined
  # If so, return the inverse and exit
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("Inverse already calcuated. Retrieving cached data.")
    return(inv)
  }
  
  # Retrieve data
  mat <- x$get()
  # Calculate the matrix inverse (assume matrix is square)
  inv <- solve(mat)
  # Store the matrix inverse
  x$setinverse(inv)
  # Return the inverse value
  inv
  
}
