## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.




## The first one, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
    invm <- NULL
    set <- function(y) {
      x <<- y
      invm <<- NULL
    }
    get <- function() {x}
    setinvm <- function(invm) invm <<- ginv(x)
    getinvm <- function() invm
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)

}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    invm <- x$getinvm()
    if(!is.null(invm)) {
      message("getting cached data")
      return(invm)
    }
    data <- x$getinvm()
    invm <- getinvm(data, ...)
    x$setinvm(invm)
    invm
  
  
}
