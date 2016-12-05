#
# MakeCacheMatrix - for caching the matrix - includes functions for set, get,
# setInverse (for storing the matrix) and getInverse (for retrieving the matrix)
# The stored / retrieved matrix capability takes adavantage of the way R stores
# variables and values by pushing them onto the stack when environments are created
# for functions. It basically means that the values of variables persist within their
# calling environments and can be retrieved when needed.
#

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  #
  # get function
  # set function
  # setInverse function
  # getInverse function
  #
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# 
# cacheSolve - use solve() to invert matrix - tries to retrieve the matrix
# (if it exists) and returns a message. If it needs to be inverted, it uses the solve
# function to perform the inversion
#
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  #
  # Use the solve function to invert the matrix
  #
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
# Sample data and test code:
# > x = rbind(c(2, -1), c(-1, 2))
# > m = makeCacheMatrix(x)
# > m$get()
#       [,1] [,2]
# [1,]    2   -1
# [2,]   -1    2
#
# first test
# > cacheSolve(m)
#           [,1]      [,2]
# [1,] 0.6666667 0.3333333
# [2,] 0.3333333 0.6666667
#
# second test
# > cacheSolve(m)
# getting cached data.
#           [,1]      [,2]
# [1,] 0.6666667 0.3333333
# [2,] 0.3333333 0.6666667
#