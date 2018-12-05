module DominicSteinitz_BackpropArticles.NeuralNet where

-- write the cost function
-- L(x-vec, y-vec, m, c) = (1/2n) sum(1->n) (y-vec -(m * x-vec + c)) .^2
cost m c xs ys = ( / (2 * (fromIntegral $ length xs) )) $ sum $ zipWith errSq xs ys
    where errSq x y = z * z
          z = y - (m*x + c)
