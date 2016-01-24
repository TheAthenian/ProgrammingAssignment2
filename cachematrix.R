# "makeCacheMatrix" is a function that
#  - sets a matrix as an argument of the function (x=Matrix())
#  - assigns a value for the matrix (with set function, x<<-y)
#  - gets the value (with the get function)
#  - assigns the value of the inverse function (with setinverse and invrs<<-inveerse)
#  - finally gets the value of hte inverse function

makeCacheMatrix<-function(x=matrix()) {
  invrs<-NULL
  set<-function(y) {
    x<<-y
    invrs<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) invrs<<-inverse
  getinverse<-function() invrs
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

#The function "cacheSolve" returns the inverse of the matrix created and cached with "makeCacheMatrix"
#  - it checks if the inverse has already been created (with the if statement)
#  - if yes, it returs its values (with return statement)
#  - if not, it calculates the invers by calling the setinverse function from a different environment
#  (in this case the different environment is the environment of hte makeCacheMatrix Fucntion)

cacheSolve<-function(x,...) {
  invrs<-x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  matrix<-x$get()
  invrs<-solve(matrix,...)
  x$setinverse(invrs)
  invrs
}

# In the following lines i create a 2x2 matrix to test the funcction

x<-matrix(1:4, nrow=2, ncol=2)
invrs=makeCacheMatrix(x)
invrs$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(invrs)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 
