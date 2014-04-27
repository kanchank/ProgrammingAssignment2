## This R script creates a special Matrix whose inverse can be cached. 
## Whenever the matrix changes, its inverse it set to null and cache is rebuilt.

## Below function creates a special maptrix and provides a list of functions 
## to get and set the matrix along with its inverse

makeCacheMatrix <- function(x = matrix()) {

  # Initialize the inverse i to NULL
  i <- NULL
  
  # set function to override the matrix
  set <- function(y){
    x <<- y
    # whenever the matrix is changed, set the inverse to NULL as old inverse value is no longer valid.
    i <<- NULL
  }
  
  # returns current matrix
  get <- function() x
  
  #set the inverse of a matrix
  setInverse <- function(inverse) i <<- inverse
  
  # returns inverse of a matrix
  getInverse <- function() i
  
  #finally a list of functions is returned to make changes to matrix and get its values and inverse
  list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
  
}


## Function creates the inverse of a matrix if it is not counf in cache and sets it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  if(!is.null(i)){
    ##return the inverse if it is found in cache
    return(i)    
  }
  
  # if cache us empty, calculate the inverse
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)  
  #return inverse of matrix
  i  
}

