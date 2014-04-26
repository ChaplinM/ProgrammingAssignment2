## These functions allow caching to be wrapped around a normal matrix,
## specifically in terms of caching the inverse of a matrix.
## Calculating the inverse of a large matrix is an expensive operation,
## so we would like to avoid doing this repeatedly for a matrix that is
## not actually changing.

## Creates a cache-ready matrix that holds the matrix arg as internal data.
## The use of the "<<-" operator and the return of a list of functions is
## a typical feature of functional programming in R.
makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  
  # Assigns the argument to a variable called 'x' in the parent environment.
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  
  # Gets the value of variable 'x' in the parent environment.
  get <- function() x
  
  # Assign the argument to a variable called 'mx' in the parent environment.
  setinv <- function(inv) mx <<- inv 
  
  # Gets the value of the variable called 'mx'.
  getinv <- function() mx
  
  # Return a list of the functions defined within makeCacheMatrix.
  # This is like defining a set of methods in a class in object-oriented terms.
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Takes a cached matrix and populates the inverse, if not already set.
## Will get an error if you pass an atomic matrix, e.g. matrix(1:9,3,3) as arg.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # check if the inverse has already been cached and return early if so
  mx <- x$getinv()
  if(!is.null(mx)) {
    message("getting cached matrix inverse")
    return(mx)
  }

  # since the inverse is not cached, we need to get the data matrix
  data <- x$get()

  # use the in-built solve() function to solve the matrix equation ax=b where a
  # is our matrix and b is a default identity matrix
  # additional arguments to cacheSolve() are passed as-is to solve() function 
  mx <- solve(a=data, ...)

  # make sure that the inverse we just computed is cached for next time
  x$setinv(mx)

  # return the matrix
  mx
}


