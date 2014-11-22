## Put comments here that give an overall description.
## In this program, we are going to do the following:
## 1. Create a special matrix object that can cache its inverse.
## 2. Compute the inverse of the special matrix; if inverse has already been calculated, then retrieve the inverse from cache.
##    Assume that the matrix is always invertible.
## --------------------------------------------------------------------------------------------Start number 1:

##1. Create a special matrix object that can cache its inverse:
makeCacheMatrix <- function(x = matrix()) 
{
  varm <- NULL
  set <- function(arg) 
  {
    x <<- arg
    varm <<- NULL
  }
  get <- function() x
  ## Set inverse:
  setInversematrix <- function(Inversematrix) varm <<- Inversematrix
  #3 Get inverse:
  getInversematrix <- function() varm
  ## Obtain the list of object functions:
  list(set=set, get=get, setInversematrix=setInversematrix, getInversematrix=getInversematrix)
}

## --------------------------------------------------------------------------------------------Start number 2:

##2. Compute the inverse of the special matrix; if inverse has already been calculated, then retrieve the inverse from cache:
cacheSolve <- function(x, ...) 
{
  varm <- x$getInversematrix()
  ##Check to see inverse has been calculated:
  if(!is.null(varm))
  {
    return(varm)
  }	
  else
  {
    ## Get the matrix:
    matrixObjectx <- x$get() 
    ## Inverse matrix:
    varm <- solve(matrixObjectx) 
    ## Cache inverse matrix into object:
    x$setInversematrix(varm) 
    return(varm)
  }
}
## --------------------------------------------------------------------------------------------End Program
