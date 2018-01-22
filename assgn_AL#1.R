### Algorithm_assignment_백광제

## 1. Monte Carlo Method

craps <- function() 
{
  dice1 <- sample(1:6, size = 1, replace = TRUE)
  dice2 <- sample(1:6, size = 1, replace = TRUE)
  sum  = dice1 + dice2
  
  if (sum == 7 || sum == 11) 
    return(1)
  
  else if (sum == 2 || sum == 3 || sum == 12)
    return(0)
  
  else {
    while (TRUE) {
      dice1 <- sample(1:6, size = 1, replace = TRUE)
      dice2 <- sample(1:6, size = 1, replace = TRUE)
      sum_new = dice1 + dice2
      
      if (sum_new == sum) 
        return(1)
      
      else (sum_new == 7)
        return(0)
    }
  }
}

monte_carlo <- function(ky_coin, sy_coin)
{
  ky_win_cnt = 0
  sy_win_cnt = 0
  
  for (i in 1:1000)  
  {
    ky_coin = 1
    sy_coin = 1
    
    while (TRUE) 
    {
      if (craps() == 1) 
      {
        ky_coin = ky_coin + 1
        sy_coin = sy_coin - 1
      }
      else 
      {
        ky_coin = ky_coin - 1
        sy_coin = sy_coin + 1
      }
      
      if (ky_coin == 0) 
      {
        sy_win_cnt = sy_win_cnt + 1
        break
      }
      else if (sy_coin == 0) 
      {
        ky_win_cnt = ky_win_cnt + 1
        break
      }
    }
  }
  result = c(ky_win_cnt, sy_win_cnt)
  return(result)
}

monte_carlo(12, 9)
monte_carlo(20, 9)
