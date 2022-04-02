# R Course: Programming Assignment 2 - Lexical Scoping

### Functions ###

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # base inverse as null
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get matrix
  get <- function() x
  
  # set / get matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  # return functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # set inverse & return if not null
  inv <- x$getInverse()
  if (!is.null(inv)) {return(inv)}
  
  # get matrix & solve inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # cache inverse
  x$setInverse(inv)
  
  # return inverse
  inv
}

### Test new functions ###
x <- matrix(sample(1:1000,9),3,3)

my_matrix <- makeCacheMatrix(x)

print('Get matrix')
print(my_matrix$get())

print('Get matrix inverse')
print(my_matrix$getInverse())

print('Set & get cached inverse')
cached <- cacheSolve(my_matrix)
print(my_matrix$getInverse())