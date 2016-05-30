## The following functions get the value of the matrix and the return
## the inverse for that matrix

## The makeCacheMatrix function returns a list that contains functions to:
## 1.set the value of the matrix
## 2.get the value of the vector
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
x<<-y
inv<-NULL
}
get<-function() x
sinv<-function(inverse) inv<<-inverse
ginv<-function() inv
list(set=set,get=get,sinv=sinv,ginv=ginv)
}


## cacheSolve returns the inverse of the matrix.If the inverse is already
## calculated,it gets the value of the inverse.Otherwise,it computes the 
## inverse and sets the value using the sinv function in the cache

cacheSolve <- function(x, ...) {
inv<-x$ginv()
if(!is.null(inv))
{
message("getting the cached inverse")
return(inv)
}
mat<-x$get()
inv<-solve(mat)
x$sinv(inv)
inv
}
