## These functions cache the calcuation of the inverse of a square matrix

## makeCacheMatrix -> this function takes in input a numeric square matrix and provides a list of function to store and retrieve
## the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  minv <- NULL

  set <- function(y = matrix()) {
    
    x <<- y
    minv <<- NULL
    
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


## cacheSolve -> this function takes in input an object created through makeCacheMatrix function and calculates the inverse
## matrix only if the inverse calculation had not already performed before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inverse <- x$getinverse()
         
         if (! is.null(matrix_inverse)) {
           print("Getting Matrix inverse value cached data")
           return(matrix_inverse)
         }
         #Get the matrix data
         data <- x$get()
         
         #Get the rows number (=columns number) of the input square matrix
         matrix_dim <- nrow(data)
         
         #Calculate inverse matrix
         matrix_inverse <- solve(data,diag(matrix_dim))
         
         #Cache inverse matrix
         x$setinverse(matrix_inverse)
        
        matrix_inverse
}
