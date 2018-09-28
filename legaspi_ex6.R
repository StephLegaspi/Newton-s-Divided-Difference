
RowColNames <- function(size){
  rcnames = c()
  for (i in 1:size) {
    rcnames <- c(rcnames, i)
  }
  return(rcnames)
}

FirstOrder <- function(x, y, order, m){
  col = 1
  for (i in 1:order) {
    fo = (y[i+1] - y[i])/(x[i+1] - x[i])
    m[i, col] = fo
    #print(fo)
  }
  return(m)
}

GetOrders <- function(x, y, order, m){
  count = order
  for(i in 2:(order)){
    count = count - 1
    for(j in 1:count){
      ans = (m[(j+1), (i-1)] - m[j, (i-1)])/(x[j+i] - x[j])
      m[j, i] = ans
    }
  }
  return(m)
}

GetMultipliers <- function(x, i, term){
  
  for(j in 1:i){
    term = paste(term, "(x - ", sep = " * ")
    term = paste(term, x[j], sep = "")
    term = paste(term, ")", sep = "")
  }
  
  return(term)
}

SetUpPolynomial <- function(x, y, m, order){
  term = y[1]
  row = 1
  for(i in 1:order){
    term = paste(term, "+", sep = " ")
    term = paste(term, m[1, i], sep = " ")
    
    term = GetMultipliers(x, i, term)
  }
  return(term)
}

GetCoefficients <- function(y, m, order){
  coeffs = c()
  coeffs = c(coeffs, y[1])
  for(i in 1:order){
    coeffs = c(coeffs, m[1, i])
  }
  return(coeffs)
}

NDD <- function(x, y, order){
  if(order == length(x) || order > length(x)){
    return(NA)
  }
  m = matrix(data=NA, nrow=order, ncol=order, dimnames = list(RowColNames(order), RowColNames(order)))
  m = FirstOrder(x, y, order, m)
  if(order != 1){
    m = GetOrders(x, y, order, m)
  }
  poly = SetUpPolynomial(x, y, m, order)
 
  args = "x"
  poly_func = list(coefficients = GetCoefficients(y, m, order), func = eval(parse(text = paste('function(', args, ') { return(' , poly , ')}', sep=''))))
  return(poly_func)
  
}

x = c(8, 9, 11, 12)
y = c(0.9031, 0.9542, 1.0414, 1.0792)
result_final = NDD(x, y, 3)
#print(result_final)
print(result_final$coefficients)
print(result_final$func(10))

