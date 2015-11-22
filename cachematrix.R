makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(a = set, b = get, c = setsolve, d = getsolve)
}


cacheSolve <- function(x, ...) {
  m <- x$d()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$b()
  m <- solve(data, ...)
  x$c(m)
  m
}

