#Given a square matrix x, the cacheSolve function's task is to return the inverse 
#of x. cacheSolve accomplishes this task by first checking to see if the inverse 
#for x has already been calculated and cached in the makeCacheMatrix function. If 
#it has not been calculated already, then cacheSolve calculates the inverse of
#the square matrix x and caches it in the makeCacheMatrix function.

#The makeCacheMatrix function's task is to store the inverse of the matrix x.
#makeCacheMatrix accomplishes this by creating a special matrix object that 
#can cache it's inverse.

#makeCacheMatrix returns a list of size 4 containing four functions.
makeCacheMatrix <- function(x=matrix()) {
  
  #Set value of m to NULL to make sure it is clear.
  m<-NULL
  
  #"set" is a function that redefines the value of x and clears m.
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  #"get" is a function that retrieves the value of x.
  get<-function() x
  
  #"setinv" is a function that caches the x and it's inverse.
  setinv <- function(solve) m <<- solve
  
  #"getinv" is a function that retrieves the inverse of x given x.
  getinv <- function() m
  
  #"list" is a data structure that contains the four functions. 
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

#cacheSolve returns the inverse of a square matrix x.
cacheSolve <- function(x, ...) {
  
  #"m" retrieves the inverse of x if it is available.
  m <- x$getinv()
  
  #if "m" is stored/cached, then:
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #If "m" is not stored/cached, then calculate by first retrieving
  #the square matrix x using get() contained in makeCacheMatrix().
  data <- x$get()
  
  #Next, calculate the inverse of x, if it exists, and define m as 
  #this inverse.
  m <- solve(data, ...)
  
  #Here, we cache the inverse of x so that it can be looked up instead
  #of having to recalculate.
  x$setinv(m)
  
  #Display inverse to console.
  m  
}
