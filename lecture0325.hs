{-
    1. Chapter 8 -- Of the Synt
    2. Parsing Lists

    Parsing just means making numerical values into key words.
    Wanting to implement something that does this: 
    F x y = x^2 + y
    G x = F(x, F(1,2*x))
    > G 5 = F (5,F(1,2*5))
          = F (5,1^2+2*5) = F(5,11)
                          = 25 + 11 = 36



-}