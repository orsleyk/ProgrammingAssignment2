
##Inverts a given matrix 

makeCacheMatrix <- function(X = matrix()) {
  Z = NULL
  set_val <- function(A){
    X <<- A
    Z <<- NULL
  }
  get_val <-function() X
  set_inverse <- function(inverse) Z <<- inverse
  get_inverse <- function() Z
  list(set_val = set_val,
       get_val = get_val,
       set_inverse = set_inverse,
       get_inverse= get_inverse)
}


## Retreives unchaged matrix inverse value from cache

cacheSolve <- function(X, ...){
  Z <-X$get_inverse()

  matrix <- X$get_val()
  Z <- solve(matrix, ...)
  X$set_inverse(Z)
  Z
        ## Returns a matrix that is the inverse of X
}
