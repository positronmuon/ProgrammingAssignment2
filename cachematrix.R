## The following two functions makeCacheMatrix and cacheSolve perform the cached inverse calculation for a matrix

## Write a short comment describing this function

#create an object for makeCacheMatrix
# it will be able to set and get the value of the matrix. set and get the
# inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set<-function(y)
      {
        x<<-y
        m<<-NULL
      }
      get<-function() x
      
      setCacheMatrix<-function(z) m<<-z
      getCacheMatrix<-function() m
      
      list(set=set,
           get=get,
           setCacheMatrix =setCacheMatrix,
           getCacheMatrix =getCacheMatrix
          )
      
  
}


# cacheSolve first checks whether the inverse of the matrix 
#  is already calculated and cached, if yes, it got it from
#  the cache, otherwise, it calculates its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getCacheMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-solve(data)
  x$setCacheMatrix(m)
  m
  
  }
