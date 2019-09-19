module Test where

sayHello :: String -> IO ()
sayHello x =
    putStrLn("Hello, " ++ x ++ "!")

triple x = x * 3

approximateAreaOfCircleWithRadius radius = 3.14 * (radius * radius)

areaOfCircleWithRadius radius = pi * (radius * radius)
