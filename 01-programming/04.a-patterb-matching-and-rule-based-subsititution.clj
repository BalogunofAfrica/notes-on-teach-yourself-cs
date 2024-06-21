(def dsimp [] (simplifier derivative-rules))


(->> ["me" "myself" "i"]
     (map inc)
     (map keyword))
