#creates a matrix object
makeCacheMatrix <- function(x = matrix()) 
{
  # initialize the matrix to NULL
  inverse  <- NULL
  
  set <- function(y) 
  {
    x <<- y
    inverse <<- NULL
  }
  
  #returns the matrix
  get <- function() 
  {
    x
  }
  
  #assigns the argument matrix to inverse 
  setinverse <- function(inv)
  { 
    inverse <<- inv
  }
  
  #returns the inverse matrix
  getinverse <- function()
  {
    inverse
  } 
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# calculates the inverse of the matrix returned by "makeCacheMatrix"
## If the inverse is already computed and the matrix has not modified, 
### then the "cachesolve" retrieves the inverse from the cache instead of calculating again.
cacheSolve <- function(x, ...) 
{
  inverse <- x$getinverse()
  #check if the returned inverse matrix is not null. It will be null if running for the first time
  if(!is.null(inverse)) 
  {
    message("retrieving cached data")
    return(inverse)
  }
  #get the matrix 
  data <- x$get()
  #calculate the inverse of x
  inverse <- solve(data, ...)
  #set the inverse matrix. 
  x$setinverse(inverse)
  inverse
}

# check if the function creates a new inverse matrix when the matrix is modified
test <- function() 
{
  matrix_1 <- makeCacheMatrix(matrix(1:4, 2,2))
  matrix_1$get()
  matrix_1$getinverse()
  cacheSolve(matrix_1)
  matrix_1$set(matrix(c(0,22,33,76), nrow=2, ncol=2)) # Modify existing matrix
  cacheSolve(matrix_1)   # calculates, caches and returns new matrix inverse
  matrix_1$get()         # Returns matrix
  matrix_1$getinverse()  # Returns matrix inverse    
  matrix_1$get() %*% matrix_1$getinverse() # returns the identity matrix
}


# check if the function retrieves cached data when the matrix is not modified
test_no_modification <- function() 
{
  matrix_2 <- makeCacheMatrix(matrix(1:4, 2,2 ))
  matrix_2$get()
  matrix_2$getinverse()
  cacheSolve(matrix_2)   
  cacheSolve(matrix_2)   # returns the cached matrix
  matrix_2$get()         # return matrix
  matrix_2$getinverse()  # return inverse matrix  
  matrix_2$get() %*% matrix_2$getinverse() # returns the identity matrix
}
