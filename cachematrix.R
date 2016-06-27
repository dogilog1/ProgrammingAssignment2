## These functions are able to create a repository of inverse matrices by caching them.
## If a function has already been solved, it will be searched for in the repository. 

## This function creates an empty matrix that is used to cache inverses.

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(matrix) 
  {
    x <<- matrix
    m <<- NULL
  }
  get <- function()
  {
    x
  } 
  setsolve <- function(inverse)
  {
    m<<-inverse
  } 
  getsolve <- function()
    {
    m
    }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function searches for the matrix in the repository and if it is not there, computes and then stores it. 
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data) %*% data
  x$setsolve(s)
  s
}


