## A fun��o tem como objetivo inverter uma matrix (n,n)
## Marcelo Miranda - 2016-01-24

## A fun��o cria uma matrix em cache
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## A fun��o faz a invers�o da matriz em cache
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  
  x$setinverse(inv)
  
  inv
}
