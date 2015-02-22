## The two functions below work in conjunction to make use of the scoping rules in R to store 
##the invers of a matrix thus reducing time by not having to recompute the matrix invers.
##These functions assume that the matrix they are passed can infect be inverted.

## Note this code is a modification of the example code supplied in the README.md

## makeCacheMatrix: takes a matrix as its argument the body off the code then creates an
##empty variable the function then creates four enclosed functions set, (which takes a sets 
##the original matrix to equal a new matrix and empty's the Q variable), get (returns the x 
##variable which is the original matrix passed to the function), setInvers (uses the solve 
##function on a matrix and assigns it to the q variable), getInvers (returns the q variable).
##Finally a list containing all the functions is created and returned.

makeCacheMatrix <- function(x = matrix()) {
  q<-NULL
  set <- function(b=matrix) {
    x <<- b
    q <<- NULL
  }
  get<-function()x
  setInvers<-function(solve) q<<-solve
  getInvers<-function()q
  list(set=set,get=get,setInvers=setInvers,getInvers=getInvers)
}


##cacheSolve: takes the previous function or a saved instance of the previous function 
##(makeCacheMatrix) and arguments for the solve function. The function then uses the listed 
##functions to see if there is an inverted matrix stored. If there is the stored inverted 
##matrix it is returned. If there is not the function gets the matrix that is stored by the 
##makeCacheMatrix function then passes the matrix to the solve function finely storing the 
##inverted matrix and returning it

cacheSolve <- function(x, ...) {
  c <-x$getInvers()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  matrix <- x$get()
  c<- solve(matrix, ...)
  x$setInvers(c)
  c    ## Return a matrix that is the inverse of 'x'
}


##Bibliography

##Peng, R. D. (0215, 02 22). rdpeng/ProgrammingAssignment2/blob/master/README.md. 
##Retrieved from github.com: https://github.com/rdpeng/ProgrammingAssignment2/blob/
##                           master/README.md

##solve {base}. (2015, 02 22). Retrieved from 
##  inside-r: http://www.inside-r.org/r-doc/base/solve
