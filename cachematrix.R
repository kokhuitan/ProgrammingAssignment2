## Calculate inverse of a matrix and then cache the result
## If the matrix has been evaluated before, retrieve from cache

## Store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- mean
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Check if the inverse of a matrix has been cached
## If so, recall from cache
## If not, calculate the inverse and cache it

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## Return a matrix that is the inverse of 'x'
  m <- solve(data, ...)
  x$setinv(m)
  m
}