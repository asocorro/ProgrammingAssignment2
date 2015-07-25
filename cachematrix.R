## These functions allow the computation and memory storage of the inverse of a matrix.

## The first function creates a vector to hold the matrix and its inverse, together with
## accessor functions.  The second function computes the inverse of the matrix in such 
## a vector, if it has not been computed yet.  If it has, it just returns the stored inverse.

## Note: an error will be returned if the matrix does not have an inverse.

## Create a vector to hold the matrix and its cached inverse
makeCacheMatrix <- function(x = numeric()) {
  # inv - internal variable to hold the inverse of matrix x
  inv <- NULL
  
  set <- function(value) {
    x <<- value
    inv <<- NULL
  }
  
  # this function returns the original matrix
  get <- function() x
  
  # this function saves the calculated inverse
  fnSetInverse <- function(inverse) inv <<- inverse
  
  # this function returns the stored inverse
  fnGetInverse <- function() inv
  
  list(set = set
       , get = get
       , setInverse = fnSetInverse
       , getInverse = fnGetInverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  else {
    message("no cached data")
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

# ----------- TEST CASES ---------------

# invertible matrix example from http://www.mathwords.com/i/inverse_of_a_matrix.htm
tm1 <- matrix(c(4,3,3,2), nrow=2, ncol=2)
tm1c <- makeCacheMatrix(tm1)
# message should be 'no chached data'
cacheSolve(tm1c)
# message should be 'getting cached data'
cacheSolve(tm1c)

# invertible matrix example from http://www.sosmath.com/matrix/matinv/matinv.html
tm2 <- matrix(c(1,-1,1,2), nrow=2, ncol=2)
tm2c <- makeCacheMatrix(tm2)
cacheSolve(tm2c)
cacheSolve(tm2c)

# invertible matrix example from https://answers.yahoo.com/question/index?qid=20120114080640AAoM43Q
tm3 <- matrix(c(2, 3, 1, 5, 1, 0, 3, 1, 0, 2, -3, 2, 0, 2, 3, 1), nrow=4, ncol=4)
tm3c <- makeCacheMatrix(tm3)
cacheSolve(tm3c)
cacheSolve(tm3c)
