## Implement caching of a matrix inversion operation by using two functions: 1) makeCacheMatrix() 
## 2) cacheSolve()

## makeCacheMatrix creates a list of four functions-set,get,setmean and getmean.The list is then
## used by the second function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve uses the output of the makeCacheMatrix() to decide if the inverse is already
## calculated, if not it calculates the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
