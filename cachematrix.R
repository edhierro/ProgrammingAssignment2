## The first function makeCacheMatrix returns a list
## of the following  4 functions:
##   - set()           Sets the value of a matrix.
##   - get()           Gets the value of a matrix.
##   - setinverse()    Set the inverse of an invertible matrix.  
##   - getinverse()    Gets the value of a matrix.


makeCacheMatrix <- function(x = matrix()) {
  mt <-NULL
  set <<- function(y){
    x<<- y
    m<<-NULL
  }
  get <-function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculates the inverse of the "vector"
## created by the MakeCacheMatrix function shown above
## if the matrix inverse exists it will retrieve the value
## from cache othewise it will calculate the value, store
## in cache and return it. 


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("gettting cached data")
    return(m)
  }
  data <-x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


