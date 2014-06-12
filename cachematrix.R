#First I rename the function makeCacheMatrix, that creates a special "matrix" object that can cache its inverse and call it "a"
#Then I set up a matrix for that function.

a<-makeCacheMatrix()
a$set(matrix(1:4,2,2))

# Then I create the special vector makeCacheMatrix which is really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the matrix
# 4. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
  setmatrix=setmatrix,
  getmatrix=getmatrix)
}

# Then I create another function that computes the inverse of the special "matrix" returned by makeCacheMatrix.  
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }  
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}   

# Lastly I return a matrix that is the inverse of 'x' by putting the makeCacheMatrix function into the CacheSolve function.

cacheSolve(a)
