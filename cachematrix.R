## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## You can use the code block to test the function

# > a <- makeCacheMatrix()
# > a$set(matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0),3,3) )
# > a$get()
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0
# > cacheSolve(a)
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# > cacheSolve(a)
# getting cached data
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x ##Single line function that returns the value already set using object$set(...)
  
  setInverse <- function(solve) m <<- solve 
  
  getInverse <- function() m
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  if(!is.null(m)) ##Check if the inverse of the object has been cached/solved
  {
    message("getting cached data")
    return(m)
  }
  
  ##If null then get the already set value of the matrix
  data <- x$get()
  
  m <- solve(data, ...)  ##Solve for the inverse of the matrix 
  
  x$setInverse(m)  ##Set the inverse of the matrix
  
  return(m)
  
}
