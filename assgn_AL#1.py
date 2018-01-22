
import random

# returns:
#   True: win
#   False: lose
def craps():

    dice1 = random.randint(1, 6)
    dice2 = random.randint(1, 6)
    sum = dice1 + dice2
    
    if sum == 7 or sum == 11:
        return True
    elif sum == 2 or sum == 3 or sum == 12:
        return False
    
    while 1:
        dice1 = random.randint(1, 6)
        dice2 = random.randint(1, 6)
        sum_new = dice1 + dice2
        
        if sum_new == sum:
            return True
            
        elif sum_new == 7:
            return False
    
# returns:
#   ky wins, sy wins
def monte_carlo(loop_num=1000, init_ky_coin=1, init_sy_coin=1):

    ky_win_cnt = 0
    sy_win_cnt = 0
    
    for _ in range(loop_num):
        
        ky_coin = init_ky_coin
        sy_coin = init_sy_coin

        while 1:
    
            if craps():
                ky_coin += 1
                sy_coin -= 1
            else:
                sy_coin += 1
                ky_coin -= 1
                
            if not ky_coin:
                sy_win_cnt += 1
                break
            elif not sy_coin:
                ky_win_cnt += 1
                break
        
    return ky_win_cnt, sy_win_cnt
        
    
print("{} 1000 loop".format(monte_carlo()))
print("{} 1000 loop, ky, sy coin: 12, 9".format(monte_carlo(1000, 12, 9)))
print("{} 1000 loop, ky, sy coin: 20, 9".format(monte_carlo(1000, 20, 9)))


