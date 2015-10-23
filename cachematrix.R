##
## Matrix inversion is usually a costly computation 
## and there IS  some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly
## 


## 
## makeCacheMatrix creates a special type of matrix
## that can cache the Inverse for use multiple times
## 


makeCacheMatrix <- function(x = matrix()) {

  ## Initializing variables for the entire scope of the 'special matrix'
  ## Variable names document the use
  
  initialized  <- FALSE
  cachedInverseMatrix <- matrix()
  cachedMatrix <- matrix()
  
  ## 
  ##  INTERFACE FUNCTIONS AREA
  ##  InterfaCe functions are: set, get and getInverse
  ## 
  
  set <- function(MatrixToCache) {
    if (initialized) {
      ## NOT the first time call to set function
      
      if (! identical(MatrixToCache, cachedMatrix )) {
        
        ## There was a cached inverse and matrix but the call to set is a different matrix. Change both values
        cachedMatrix <<- MatrixToCache    
        cachedInverseMatrix <<- solve(MatrixToCache)
        initialized <<- TRUE
      }
      else {
        ## The function set was called with a matrix that is identical to the previous value. 
        ## Saving time not calculating the inverse
        message("set: Previously initialized but identical. NOT changing cached values")
      }
    }
    else{
      ## First time to SET the values of the 'Special Matrix'
      cachedMatrix <<- MatrixToCache    
      cachedInverseMatrix <<- solve(MatrixToCache)
      initialized <<- TRUE
      message("First set for the matrix. Caching matrix and its inverse")
    }
  }
  
  
  get  <- function() {cachedMatrix}
  
  getInverse  <-  function(){cachedInverseMatrix}
  

  ##  RETURN VALUE
  ## a list of functions that let the user "connect" with the 'special matrix' to change it's values
  ## or to get current cached values of original matrix and its inverse
  
  list(set=set, get=get, getInverse=getInverse )
  
}



## 
## cacheSolve lets the user to obtain the inverse of a special matrix: argument x
## created with makeCacheMatrix
## 

cacheSolve <- function(x, ...) {
  
  ## returns a matrix that is the inverse of special matrix 'x' created with makeCacheMatrix function
  x$getInverse()
  
}

