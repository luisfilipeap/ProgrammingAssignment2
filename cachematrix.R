


#function that creates a CacheMatrix object
makeCacheMatrix <- function(x = matrix()) {

  #when a new object is created, the marix is given but we do not know if its inverse is cached or not 
  inv <- NULL
  mat <- x

  #get and sets functions to access the matrix and its inverse. 'setMat' is not required because the variable mat is setted
  #by the moment an new cacheMatrix object is created
  getMat <- function() mat
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  
  #return a list of functions that could be used to operate with the CacheMatrix
  list(getMat = getMat, getInv = getInv, setInv = setInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
    t <- x$getInv()
    if(is.null(t)){
        #if the input inverse was not calculated before, do it now! and store it in cache
        i <- solve(x$getMat())
        x$setInv(i)
        return(i)
    } else {
        #otherwise, return the cached inverse
        message("getting cached data")
        return(t)
    }
}
