## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix create a special matrix as a list with :
# set and get functions which set and get the matrix 
#  setinverse and  getinverse  which set and get the  inverse matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
  
  #first we attribute a name to the inverse matrix 
  # if reversem exists , it is destroyed and put to NULL
  # it is an assignement for the first calculation of the inverse matrix
  reversem<- NULL
  
  # function which permit to set the matrix and its inverse
  
  setm <- function(y) {
    x <<- y
    reversem <<- NULL
  }
  getm <- function() x
  setinverse <- function(inverse) reversem<<- inverse
  getinverse <- function() reversem
  list(set = setm, get = getm,
       setinverse = setinverse,
       getinverse =  getinverse)

}


## Write a short comment describing this function
# cacheSolve calculate give the inverse of the matrix obtained by makeCacheMatrix  function
# if the inverse of the matrix was calculated once, it exists in the cache and cacheSolve only use 
# this result
# if there's no cache value, cacheSolve calculate the inverse of the matrix

cacheSolve<- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  
  # if the inverse of the matrix was calculated once then  x$getinverse as defined in the matrix constructed by
  #makeCacheMatrix contain the results else   getinverse  is null as defined in the function
  #makeCacheMatrix
  
  inversx <- x$getinverse()
  if(!is.null( inversx)) {
    message("getting cached data")
    return( inversx)
  }
  
  # if the inverse matrix is not in the cache we have to solve it
  # only a square matrix can be inverse

  data <- x$get()
  
  if (dim(data)[[1]]!=dim(data)[[2]])
  {
    stop("special matrix is not square one")
  }
  
  # if the matrix is a square one and if the cache doesn't contain a value then
  # the inverse matrix is calculated and put in the cache

  inversx <- solve(data, ...)
  x$setinverse( inversx )
  return(inversx)
}
