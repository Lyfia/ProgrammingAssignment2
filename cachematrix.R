##This file contains two functions relating to cacheMatrix objects
##A cacheMatrix is a vector which contains a matrix, as well as it's inverse value. 
##The function makeCacheMatrix creates the matrix object 
##The function cachSolve returns the inverse of the matrix 
##Both functions assume that the matrix supplied is always invertable




##makeCacheMatrix Creates a special "matrix" object 
##This object conatins a matrix, the inverse of that matrix 
##as well as helper functions to get and set both stored values.
makeCacheMatrix <- function(x = matrix()) {
  
  ##initilize the inverse matrix.  
  ##When we first create a cacheMatrix we have not calulated the inverse matrix
  ##so this value is initialized to null
  cachedInverse <- NULL
  
  
  ##Get: Returns the stored matrix
  get <- function() x
  
  ##Set: Sets the value of the matrix to the provided value
  set <- function(y) {
    
    x <<- y
    
    ##when we reset the matrix clear the old inverse value as it's no longer correct
    cachedInverse <<- NULL
  }
  
  
  ##getInverse: returns the inverse matrix
  getInverse <- function() cachedInverse
  
  ##setInverse: sets the value of the inverse matrix to the provided value
  setInverse <- function(inverse) cachedInverse <<- inverse
  
  
  
  ##List all of the methods which can be called on a cacheMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


##cacheSolve
##Takes a cacheMatrix object and returns a matrix which is it's inverse 
##If the inverse has already been calculated this function will return 
##the cached value 
##If this has not been calculated the value will be calculated
cacheSolve <- function(x, ...) {
  
  ##Initilize inv as  working variable containg the currently stored inverse of X
  inv <- x$getInverse();
  
  ## Check to see if the inverse has already been calculated
  if(!is.null(inv)){
    
    ##If the inverse isn't null that means we've already calculated it.  
    ##That means we can return the previously calculated value
    message("getting cached data")
    return(inv)
  }
  
  ##if the inverse is null than we need to 
  ## 1 calcuate the inverse 
  ## 2 store the inverse in the cached matrix object
  ## 3 return the cached value
  
  ##Calculate the inverse using the solve function
  inv <- solve(x$get())
  
  ##Store the inverse the cachedMatrixObject so that it can be used in the future
  x$setInverse(inv)
  
  ## Return the result
  inv
}
