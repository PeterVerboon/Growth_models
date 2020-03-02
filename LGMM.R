require(lavaan)

## LGMM

LGM1 <- "

ic =~ 1*W5
ic =~ 1*W7
ic =~ 1*W9
ic =~ 1*Wxx

sl =~ 0*W5
sl =~ 1*W7
sl =~ 2*W9
sl =~ 3*Wxx


W5 ~~ W5
W7 ~~ W7
W9 ~~ W9
Wxx ~~ Wxx

ic ~~ ic
sl ~~ sl

"


out <-lavaan(LGM1, data=dat1)

summary(out)
