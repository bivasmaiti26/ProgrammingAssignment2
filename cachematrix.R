## makeCacheMatrix is a list of functions
## usage: a<- makeCacheMatrix(matrix(1:4,2,2))
##set- Sets a matrix. ex: a$set(matrix(1:4,2,2))
## get- gets the matrix. ex: a$get()
## getinv-gets the inverse 
## setinv- sets the inverse ex: a$setinv(matrix(c(1,1,1,1),2,2)
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  a<-get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## this function calculates the inverse of the matrix.
##If the inverse is already cached, it returns the cached value of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

