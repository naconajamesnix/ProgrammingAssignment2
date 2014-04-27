## The following two functions cache the inverse of a matrix in a way
## that is computationally efficient

## This function assesses the value of a matrix and facilitates the caching
## of its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <‐ NULL
  set <‐ function(y) {
    x <<‐ y
    i <<‐ NULL
  }
  get <‐ function() x
  setInverse <‐ function(solve) i <<‐ solve
  getInverse <‐ function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## This function takes the matrix created in the previous function and
## returns its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <‐ x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i) 
  }
  data <‐ x$get()
  i <‐ solve(data, ...) 
  x$setInverse(i)
  i
}
