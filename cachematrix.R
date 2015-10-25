## cachematrix.R objective is to create a special matrix which will store 
## the inverse of the matrix to avoid repeating the expensive inverse 
## computation. This is a acheived by two functions makecachematrix
## which creates the matrix and cacheSolve which computes and sets the
## inverse of the matrix for further reference.


## makeCacheMatrix assigns the matrix passed as an argument to global
## environment variable x. Also creates the access methods to set and get
## the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  ## initialize xinv to null
  xinv <- NULL
  
  ## set method for the matrix X. when called set the x matrix to the
  ## value y. Also re-initialize xinv to null as the matrix value
  ## changed.
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  
  ## returns the matrix X. 
  get <- function() x
  
  ## sets the inverse for matrix X. 
  setinverse <- function(xinverse) xinv <<- xinverse
  
  ## returns the stored inverse of matrix X
  getinverse <- function() xinv
  
  ## access methods defined
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cache solve sets the inverse of the matrix passed as an grument. 
## if the value is already set then it uses the cached inverse value.
## If the value is not set it computes using solve and sets the inverse.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinverse()
  
  ## Check if the return value is null
  ## if not null return the cached inverse
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  
  ## inverse is not cached. Retrive matrix X
  matrixval <- x$get()
  
  ## calculate the inverse
  xinverse <- solve(matrixval)
  
  ## set the inverse
  x$setinverse(xinverse)
  
  ## returns the calculated inverse
  xinverse
}



