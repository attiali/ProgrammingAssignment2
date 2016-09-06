## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                         ##  initialize the inverse of a matrix to NULL
  
  
  set <- function(y) {                            #  Declare a function called set.  In this function we will cache the value 
    x <<- y
    inverse <<- NULL                               # if the the matrix is  changed we need to change the  inverse of the matrix 
    
  }
  get <- function() x                              # let s get the value of the inverse
  setinverse <- function(solve) inverse <<- solve  #  we will calculate the inverse using the solve function
  getinverse <- function() inverse                 #  get the inverse
  list(set = set, get = get,                       
       setinverse = setinverse,                   # pass the value of the inverse to make CacheMatrix
       getinverse = getinverse)
}



## let s get the cached matrix

cacheSolve <- function(x, ...) {                    
  inverse<- x$getinverse()                           ##  put getinverse in inverse
  if(!is.null(inverse)) {                            ##  check to see if we have a null
    message("getting cached data")
    return(inverse)                                  ##  return inverse
  }
  data <- x$get()                                    # claculate and retrieve the inverse
  inverse <-solve(data, ...)
  x$setinverse(inverse)
  inverse
}





