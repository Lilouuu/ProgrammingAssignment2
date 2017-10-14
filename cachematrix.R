## These functions aim to create a special object who cashes a matrix and its inverse. 
## You can find two funcions below:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then cacheSolve should retrieve the inverse from the cache



## makeCacheMatrix:
## contains a list of functions
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
    }
  get <- function () x
  setInverse <- function (inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve:
## check if there exists already the inverse of matrix
## if not then calculate the inverse and caches the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$get()
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  inv <- solve(mat)
  x$setInverse(inv)
  x$set(mat)
  inv
}
