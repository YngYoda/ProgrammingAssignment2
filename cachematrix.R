## Put comments here that give an overall description of what your
## functions do

  ## The following functions are created to simply the data retrieval process when dealing with a lot
  ## of matrices. The function is created to calculate the inverse of an invertible matrix.
  ## If the inverse is already calculated before 
  ## then we can just retrive its value without make R compute it again.


    ## Write a short comment describing this function
    ## The Function makeCacheMatrix is defined to accept a matrix as input and output a 
    ## "new" object that is used to calculate the inverse of the matrix. 
    ## Assuming that the matrix is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  
  # initiaize myInverse to NULL
  myInverse <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    myInverse <<- NULL
  }
  
  # get the value of the matrix back
  get <- function() x
  
  #set the inverse of the matrix by using the solve function of cacheSolve
  setinverse <- function(solve) myInverse <<- solve
  
  #get the inverse value of the matrix computed by cacheSolve
  getinverse <- function() myInverse
  # store to a list that creates a new object
 
   list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

    ## The following function takes the output of the previous function i.e the matrix created 
    ## and computes the inverse of the object. If the object has already been created then 
    ## it retrieves it from the cache.

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  myInverse <- x$getinverse()
  
  ## if already stored inv of matrix then retrive it from cache
  if(!is.null(myInverse)) {
    message("getting cached data")
    return(myInverse)
  }
  
  ## else using the output of makeCacheMatrix as input compute inverse
  data <- x$get()
  myInverse <- solve(data, ...)
  x$setinverse(myInverse)
  
  ## display the inverse and return 
  myInverse
  
}
