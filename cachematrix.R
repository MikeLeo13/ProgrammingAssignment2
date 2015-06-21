## The first function can be assigned to a variable to create and cache a matrix or store a list of functions.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  #The functions that will be added to the list are created here.
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m

  #The following function adds the functions to a list.
    list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## The second function solves for the inverse of the cached matrix and produces an output, which is also a matrix.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  #The following if function checks if m is null.  If it is, m isn't changed and returned as is.
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #This part is the "else" that calculates the inverse of the matrix and returns it.
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

