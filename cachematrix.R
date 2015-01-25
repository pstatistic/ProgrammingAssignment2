## A special "matrix" object (list) is built using the the makeCacheMatrix function.
## This function provides the find and put values of the matrix to get the stored matrix
## and an inverse.
## The cacheSolve function gets the inverse matrix cached in the matrix object.
## If there are no cached inverse matrices then the cacheSolve function will calculate 
## the inverse and save it in the matrix object.
## The preconditions taken in to consideration are the matrices beig invertible
## and that they haven't been changed after inverse calculation.
## The following list of functions create and also return the special matrix object that 
## can be made using the builder functions to get the matrix and its inverse to finally get
## the object as a  list.
## list includes 1. put= To set the matrix that needs to be inverted.
## 2. find= To get the value of the matrix
## 3. putInverse= Sets the value of the inverse cache.
## 4. findInverse= To get the value of Inverse cache.

makeCacheMatrix <- function(x = matrix()) {

  ## for storing the inverse of  amatrix
  inverseOfMatrix <<- NULL

  ## The function listed below sets the value of the matrix
  put <- function(matrix){
  x <<- matrix 
  inverseOfMatrix <<- NULL
  }
  
  ## The following function gets the value of matrix
  find <- function() x
  
  ## Function to set the Inverse cache value
  putInverse <- function(inverse) inverseOfMatrix <<- inverse
 
  ## Function to get the value of inverse cache
  returnInverse <- function() inverseOfMatrix

  ## Final output of the special matrix in the form of a list.
  list(put = put, find = find, putInverse = putInverse, returnInverse = returnInverse)
}


## The cacheSolve function uses the makeCacheMatrix function above to return the inverse of the 
## pre calculated special matrix. It verifies the presence of an already present inverse cache and 
## obtains the value.  If it is not present then, it calculates the inverse, sets the inverse cache and then 
## returns the inverse of the matrix.
cacheSolve <- function(x,...){
  
  ## Getting the already present inverse cached matrix
  inverseOfMatrix <- x$returnInverse()
  ##inverseMatrix <- matrix(c(2,2,3,2), nrow = 2, ncol = 2)
    
  ##Verifying if the inverse cache has any value and if not returning the inverse
  if(!is.null(inverseOfMatrix)){
    message("getting the cached inverse") 
    return(inverseOfMatrix)
  }
  
  ## If there are no cached inverse values then the following steps get the matrix that needs to be inverted
  matrix <- x$find()
  
  ##calculating the inverse of the matrix
  inverseOfMatrix <- solve(matrix)
  
  ## Implementing the newly calculated inverse in the vaue of cache
  x$putInverse(inverseOfMatrix, ...)
  
  ## Finally Returning the inverse of matrix x
  inverseOfMatrix
}


