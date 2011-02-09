module Main
    (
    ) where

import ContainerUtils

as = [0.9,0.1]
bs = [0.2,0.8]
cs = [0.9,0.1]
ds = [0.6,0.4]

commTest a b c d = abcd
    where ab = normalize $ (head a * head b) : (tail a ++ tail b)
          cd = normalize $ (head c * head d) : (tail c ++ tail d)
          abcd = normalize $ (head ab * head cd) : (tail ab ++ tail cd)