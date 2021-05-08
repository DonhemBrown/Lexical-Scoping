
##creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) Inv <<- Inverse
  getInverse <- function() Inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##returns the cached inverse of a mat if it exists
## if it does not yet exist, it solves for the inverse

CacheSolve <- function (x, ...){
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("Returning Cached Inverse")
    return(Inv)
  }
  mat <- x$get()
  Inv <- solve(mat, ...)
  x$setInverse(Inv)
  Inv
}
