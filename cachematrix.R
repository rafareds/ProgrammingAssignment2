## As funcoes servem para retornar uma matriz quadrada e a sua matriz inversa.
## O calculo da matriz inversa e feito de forma tal que se ela ja estiver calculada
## a funcao cacheSolve retornara a matriz sem recalcula-la.

## A funcao vai definir os valores da matriz atraves da funcao set, 
## depois armazena-la com a funcao get.
## Ira tambem definir os valores da matriz inversa e armazena-la.

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  set <- function(y){
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinverse <- function(inv){
    m <<- inv 
  }
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## A funcao esta diretamente ligada com a funcao makeCacheMatrix,
## ela pega a matriz inversa, resultate da 1a funcao e, se os valores desta forem
## diferentes de NA, ela retorna a matriz inversa sem calcula-la, se não, ira 
## calcular a matriz inversa e retornar os seus valores.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(sum(!is.na(m))!=0){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
