num2word <- function(x){
  # Checking whether number if integer
  if(x%%1 != 0){
    stop('Number is not in integer')
  } else {
    if(x == 0){out <- 'zero'} 
    else if(x == 1){out  <- 'one'} 
    else if(x == 2){out  <- 'two'} 
    else if(x == 3){out  <- 'three'} 
    else if(x == 4){out  <- 'four'} 
    else if(x == 5){out  <- 'five'} 
    else if(x == 6){out  <- 'six'} 
    else if(x == 7){out  <- 'seven'} 
    else if(x == 8){out  <- 'eight'} 
    else if(x == 9){out  <- 'nine'} 
    else if(x == 10){out <- 'ten'} 
    else if(x == 11){out <- 'eleven'} 
    else if(x == 12){out <- 'twelve'} 
    else if(x == 13){out <- 'thirteen'} 
    else if(x == 14){out <- 'fourteen'} 
    else if(x == 15){out <- 'fifteen'} 
    else if(x == 16){out <- 'sixteen'} 
    else if(x == 17){out <- 'seventeen'} 
    else if(x == 18){out <- 'eighteen'} 
    else if(x == 19){out <- 'nineteen'} 
    else if(x == 20){out <- 'twenty'} 
    else if(x >  20){out <- x}
  }
  return(out)
}