
manhattanLength (x,y) (a,b) = abs (x - a) + abs (y - b)

path = ["R8","U5","L5","D3"]

dir :: String -> Char
dir p = head p

dist :: String -> Int
dist p = read (tail p)

pullCable startPoint p
    | dir p == 'U' = pullUp startPoint (dist p)
    | dir p == 'R' = pullRight startPoint (dist p)
    | dir p == 'D' = pullDown startPoint (dist p)
    | dir p == 'L' = pullLeft startPoint (dist p)


pullUp (x,y) n = [(a,b) | a <- [x], b <- (reverse [y..(y+n)])]

pullRight (x,y) n = [(a,b) | a <- (reverse[x..(x+n)]), b <- [y]]

pullDown (x,y) n = [(a,b) | a <- [x], b <- [(y-n)..y]]

pullLeft (x,y) n = [(a,b) | a <- [(x-n)..x], b <- [y]]

