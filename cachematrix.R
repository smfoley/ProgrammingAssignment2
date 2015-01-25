## Takes the matrix and caches the matrix into a list to allow for cacheSolve to transpose the matrix.

makeCacheMatrix <- function(x = matrix()) {
t <- NULL
set <- function(y) {
  x <<- y
  t <<- NULL
} 
get <- function() x
settranspose <- function(transpose) t <<- transpose
gettranspose <- function() t
list(set = set, get = get,
     settranspose = settranspose,
     gettranspose = gettranspose)
}


## takes the cached matrix from makeCacheMatrix and displays "getting cached data", 
## then transposes the matrix. Returns transposed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  tp <- x$gettranspose()
  if(!is.null(tp)) {
    message("getting cached data")
    return(tp)
  }
  data <- x$get()
  tp <- t(data, ...)
  x$settranspose(tp)
  tp
}


