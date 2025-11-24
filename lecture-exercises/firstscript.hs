doubleMe x = x+x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
    then x
    else x*2

factorial x = if x > 1 
    then x * factorial (x - 1)
    else 1