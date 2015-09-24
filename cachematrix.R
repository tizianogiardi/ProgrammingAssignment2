## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  minv <- NULL

  set <- function(y = matrix()) {
    
    x <<- y
    minv <<- NULL
    matdim <- nrow(y)
    
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inv = matrix()) {
    minv <<- inv
  }
  
  getinverse <- function() {
    minv
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

         matrix_inverse <- x$getinverse()
         
         if (! is.null(matrix_inverse)) {
           print("Getting Matrix inverse value cached data")
           return(matrix_inverse)
         }
         data <- x$get()
         
         matrix_dim <- nrow(data)
         
         matrix_inverse <- solve(data,diag(matrix_dim))
         
         x$setinverse(matrix_inverse)
        
        matrix_inverse
}
