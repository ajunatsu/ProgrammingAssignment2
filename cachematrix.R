## MakeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Input to the function is a matrix whose inveser need to be calculated 
## Function Returns list with 4 functions 
## set function used to set data i.e matrix
## get function used to get matrix in return
## setinverse is used to set inverse ofa matrix 
## get inverse will return inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## function will check if inverse of matrix exist or not
## if it exist it will return invers of matrix exist
## if not then it will calculate inverse and set inverse using setinverser function 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}
