# COURSERA R Programming

# Week n.3: Loop Functions and Debugging

# Assignment n.2: Lexical Scoping

# Luca Cilumbriello

# Start of the script


# Matrix inversion is usually a costly computation.
# There may be some benefit to caching the inverse of a matrix. 

# To do it, I wrote these two functions: 
# 1. The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# 2. The cacheSolve function computes the inverse of the special "matrix" returned by the previous function:
# if the inverse has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache.


# The makeCacheMatrix function receives in input a square invertible matrix 'x'
# and returns a list containing functions to:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse
# 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  
    inv = NULL
    set = function(y) {
      x <<- y # The <<- symbol is used to assign a value to an object  
              # in an environment which is different from the current environment. 
      inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


# The cacheSolve function uses the list created above as the input 
# and return a matrix that is the inverse of the original matrix 'x'

cacheSolve <- function(x) {
 
  inv = x$getinv()
  
  # If the inverse has already been calculated ...
  if (!is.null(inv)){
    
    # ... then get it from the cache and skips the computation.
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculates the inverse and ... 
  mat.data = x$get()
  inv = solve(mat.data)
  
  # ... sets the value of the inverse in the cache by using the setinv function.
  x$setinv(inv)
  
  return(inv)
}


# End of the script
