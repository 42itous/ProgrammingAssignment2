## The 'makeCacheMatrix' function creates a get - set for the matrix 'x'
## It uses the setsolve and getsolve methods to store and retrieve the inverse 'inv'

makeCacheMatrix <- function(x = matrix()) 
  {
    inv <- NULL
  
    set <- function(valToSet)
    {
      x <<- valToSet
      inv <<- NULL   # Every time a fresh value of X is provided, previous inv is cleared and flagged as NULL
    }
  
    get <- function() x
    
    setsolve <- function(solve) inv <<-solve
    getsolve <- function() inv
  
    list (set=set, get=get, setsolve=setsolve, getsolve=getsolve)
  }


## The 'cacheSolve' function checks if an inverse exists in the cache (not NULL), picks it up (gets it) if true
## else (if NULL) it creates a fresh value of the inverse and sets it in the cache

cacheSolve <- function(x, ...) 
  {
        ## Returns 'inv', that is the inverse of 'x'
  
    inv <- x$getsolve()
    if(!is.null(inv)) 
      {
      message("getting cached data")
      return(inv)
      }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setsolve(inv)
    
    inv
  
}
