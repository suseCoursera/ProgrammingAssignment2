# Put comments here that give an overall description of what your
## functions do

#The functions get the inverse from a square matrix and "save" in memory
#so it is no necessary to calculate again, and get the result almost immediatly

#August 13th, 2015

## Write a short comment describing this function

#makeCacheMatrix does basically four actions
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the inverse of the matrix it was given at beginning
#4. get the inverse of the matrix it was given at beginning

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #set the matrix
  set <- function(y) {
    # "<<-" operator which can be used to assign a value to 
    # an object in an environment that is different from the current environment
    x <<- y
    m <<- NULL
  }
  
  #set the matrix
  get <- function() x
  
  #set the inverse
  setsolve <- function(solve) m <<- solve
  
  #get the inverse
  getsolve <- function() m
    
  #the functions defined previously
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

#the function identify if the matrix has already calculated its inverse
#in that case, get the inverse from the cache and return the value
#if not, then is calculated and "save" for future references

cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
  #get the inverse
  si <- x$getsolve()
  
  #if the inverse is not null, then return the inverse
  if(!is.null(si)) {
    message("getting cached inverse")
    return(si)
  }
  
  #other case, calculate the inverse
  data <- x$get()
  si <- solve(data, ...)
  
  #save the inverse in cache
  x$setsolve(si)
  
  si
}
