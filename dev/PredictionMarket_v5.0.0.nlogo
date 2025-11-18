extensions [nw]

globals [
  q-yes
  q-no
  price-yes
  price-no
  event-outcome

  yes-price-record
]

breed [bettors bettor]

bettors-own [
  ; characteristics
  inner-belief
  belief
  CARA
  loss-aversion

  ; outcome
  effective-belief
  submitted
  pos-yes
  pos-no
  weight

  ; tentative memory
  score
  decision
]


; ~~~~~~~~~~~~~~~~~~~~ buttons ~~~~~~~~~~~~~~~~~~~~
to setup
  clear-all
  reset-ticks
  initialize-globals
  create-market
  create-graph
end

to go
  repeat market-attempts [
    reset
    complete-market
    update-market-config
    record
    if (max list weighted-prediction (1 - weighted-prediction)) > (1 - 1e-6) [stop]
  ]
end


; ~~~~~~~~~~~~~~~~~~~~ variable control ~~~~~~~~~~~~~~~~~~~~

to initialize-globals ; box
  set q-yes 0
  set q-no  0
  set price-yes 0.5
  set price-no 0.5
  set-default-shape bettors "person"
  set yes-price-record []
end

to reset
  reset-bettors
  reset-market-states
  reset-ticks
end

to reset-bettors ; box
  ask bettors [
    set color gray
    set pos-yes 0
    set pos-no  0
    set decision "empty"
    set belief inner-belief
  ]
end

to reset-market-states ; box
  set q-yes 0
  set q-no  0
  set price-yes 0.5
  set price-no 0.5
  set-current-plot "YES price"
  clear-plot
  set-current-plot "NO price"
  clear-plot
  random-seed random 1000000000
end

to record
  set yes-price-record lput price-yes yes-price-record
end


; ~~~~~~~~~~~~~~~~~~~~ agent creation ~~~~~~~~~~~~~~~~~~~~

to create-market
  create-bettors num-bettors [
    initialize-bettor
  ]
end

to initialize-bettor ; box
  setxy random-xcor random-ycor
  set color gray
  set belief belief-min + random-float (belief-max - belief-min)
  set inner-belief belief
  set pos-yes 0
  set pos-no  0
  set weight 1
  set submitted random-float 1
  set CARA CARA-min + random-float (CARA-max - CARA-min)
  set loss-aversion loss-aversion-min + random-float (loss-aversion-max - loss-aversion-min)
end


; ~~~~~~~~~~~~~~~~~~~~ agent decision and market completion ~~~~~~~~~~~~~~~~~~~~

to complete-market
  repeat market-duration [
    betting-round
    tick
  ]
  draw-event
end

to betting-round
  ask bettors [
    make-bet
    resolve-position
    update-market-state
  ]
end

to make-bet
  set decision optimal-bet-position
  record-submittion
end

to-report optimal-bet-position ; in bettor ; box
  set effective-belief distort-belief
  report decision-function
end

to resolve-position ; in bettor ; box
  if decision != "empty" [
    let new-yes first  decision
    let new-no  last   decision
    let delta-yes new-yes - pos-yes
    let delta-no  new-no  - pos-no

    set pos-yes new-yes
    set pos-no  new-no

    if pos-yes = pos-no [set color gray]
    if pos-yes > pos-no [set color blue]
    if pos-yes < pos-no [set color red]

    set q-yes   q-yes + delta-yes
    set q-no    q-no  + delta-no
    set decision "empty"
  ]
end

to-report distort-belief
  set belief herd belief
  set belief network-information-sharing belief

  let p-hat belief

  if overconfident? = true [set p-hat overconfidence p-hat]

  let eps 1e-4
  if p-hat = 1 [set p-hat p-hat - eps]
  if p-hat = 0 [set p-hat p-hat + eps]

  report p-hat
end


; ~~~~~~~~~~~~~~~~~~~ decision function archive ~~~~~~~~~~~~~~~~~~~~

to-report decision-function ; in bettor ; box
  let gap (q-yes - q-no)
  let k CARA-loss-aversion-optimality gap

  let eps 1e-4

  let new-pos-yes pos-yes
  let new-pos-no  pos-no
  if k > 0 [
    set k budget-constraint k
    let cost-buy-yes  (liquidity * ln (exp ( q-no / liquidity) + exp ((q-yes + k) / liquidity) + eps)) - (liquidity * ln (exp ( q-no / liquidity) + exp ( q-yes / liquidity) + eps))
    let cost-sell-no  (- ((liquidity * ln (exp ((q-no  - k) / liquidity) +  exp ( q-yes / liquidity) + eps)) - (liquidity * ln (exp ( q-no / liquidity) + exp ( q-yes / liquidity) + eps))))

    ifelse cost-buy-yes < cost-sell-no [
      set new-pos-yes pos-yes + k
    ][
      ifelse cost-buy-yes > cost-sell-no [
        set new-pos-no  pos-no - k
      ][
        ifelse (random-float 1) < 0.5 [
          set new-pos-yes pos-yes + k
        ][
          set new-pos-no  pos-no - k
        ]
      ]
    ]
  ]

  if k < 0 [
    set k budget-constraint (- k)

    let cost-buy-no   (liquidity * ln (exp ((q-no + k) / liquidity) + exp ( q-yes / liquidity) + eps)) - (liquidity * ln (exp ( q-no / liquidity) + exp ( q-yes / liquidity) + eps))
    let cost-sell-yes (- ((liquidity * ln (exp ( q-no / liquidity) + exp ((q-yes - k) / liquidity) + eps)) - (liquidity * ln (exp ( q-no / liquidity) + exp ( q-yes / liquidity) + eps))))
    ifelse cost-buy-no < cost-sell-yes [
      set new-pos-no  pos-no + budget-constraint k
    ][
      ifelse cost-buy-no > cost-sell-yes [
        set new-pos-yes pos-yes - budget-constraint k
      ][
        ifelse (random-float 1) < 0.5 [
          set new-pos-no  pos-no + budget-constraint k
        ][
          set new-pos-yes pos-yes - budget-constraint k
        ]
      ]
    ]
  ]
  report (list new-pos-yes new-pos-no)
end

to-report budget-constraint [bet-size]
  report min list (abs bet-size) constraint
end

to-report overconfidence [x]
  let z overconfidence-temp * (2 * x - 1)
  report 1 / (1 + exp (- z))
end

to-report herd [x]
  report (x * (1 - herding)) + (price-yes * herding)
end

to-report network-information-sharing [x]
  let nbrs   link-neighbors
  let scale  ((max-pxcor - min-pxcor) * sqrt 2)
  let my-belief belief

  ifelse any? nbrs [
    let weighted-sum 0
    let weight-sum   0
    ask nbrs [
      let w (1 - (distance myself) / scale)
      if confirmation-bias [
        set w w * exp (- abs (belief - my-belief) / confirmation)
      ]
      set weighted-sum weighted-sum + (belief * w)
      set weight-sum   weight-sum   + w
    ]
    if weight-sum = 0 [ set weight-sum 1 ]  ;; safety
    let avg weighted-sum / weight-sum

    report (x * (1 - sharing-influence)) + (avg * sharing-influence)
  ][
    report x
  ]
end

to-report risk-neutral-optimality [g]
  report ((liquidity * ln (effective-belief / (1 - effective-belief))) - g)
end

to-report CARA-adjusted-optimality [g]
  let CARA-adjustment clip (1 + liquidity * CARA)
  report (risk-neutral-optimality g) / CARA-adjustment
end

to-report CARA-loss-aversion-utility [k]
  let k_ (ifelse-value (k > 0) [  0.01 ] [ ifelse-value (k < 0) [ (-0.01) ] [ 0 ] ])
  let dc (liquidity * ln (exp ((q-yes + k_) / liquidity) + exp (q-no / liquidity))) - (liquidity * ln (exp (q-yes / liquidity) + exp (q-no / liquidity)))
  let wY k_ - dc
  let wN (- dc)
  let eps 1e-8
  if abs CARA < eps [
    report effective-belief * (ifelse-value (wY >= 0) [ wY ] [ (- loss-aversion) * wY ]) + (1 - effective-belief) * (ifelse-value (wN >= 0) [ wN ] [ (- loss-aversion) * wN ])
  ]
  report effective-belief * (ifelse-value (wY >= 0) [ (1 - exp (- CARA * wY)) / CARA ] [ (- loss-aversion) * (1 - exp (CARA * wY)) / CARA ]) + (1 - effective-belief) * (ifelse-value (wN >= 0) [ (1 - exp (- CARA * wN)) / CARA ] [ (- loss-aversion) * (1 - exp (CARA * wN)) / CARA ])
end

to-report CARA-loss-aversion-optimality [g]
  let denom clip (1 + liquidity * CARA)

  let k-pos ((liquidity * ln (effective-belief / (loss-aversion * (1 - effective-belief))) - g) / denom)
  let k-neg (- ( (liquidity * ln ((1 - effective-belief) / (loss-aversion * effective-belief))) + g ) / denom)

  if k-pos < 0 [ set k-pos 0 ]
  if k-neg > 0 [ set k-neg 0 ]

  let best-k 0
  let best-eu CARA-loss-aversion-utility 0

  if k-pos != 0 [
    let eu-pos CARA-loss-aversion-utility k-pos
    if eu-pos > best-eu [ set best-eu eu-pos set best-k k-pos ]
  ]

  if k-neg != 0 [
    let eu-neg CARA-loss-aversion-utility k-neg
    if eu-neg > best-eu [ set best-eu eu-neg set best-k k-neg ]
  ]

  report best-k
end


; ~~~~~~~~~~~~~~~~~~~~ agent record control ~~~~~~~~~~~~~~~~~~~~

to record-submittion
  let new-yes first  decision
  let new-no  last   decision
  let delta-yes new-yes - pos-yes
  let delta-no  new-no  - pos-no
  set submitted submitted-probability delta-yes delta-no
end

to-report submitted-probability [dyes dno]
  let new-q-yes (q-yes + dyes)
  let new-q-no  (q-no  + dno)
  let Z (exp (new-q-yes / liquidity) + exp (new-q-no / liquidity))
  report exp (new-q-yes / liquidity) / Z
end


; ~~~~~~~~~~~~~~~~~~~~ market state update ~~~~~~~~~~~~~~~~~~~~

to update-market-state ;in bettor
  update-bet-price
end

to update-bet-price ; box
  let Z (exp(q-yes / liquidity) + exp(q-no / liquidity))
  set price-yes exp(q-yes / liquidity) / Z
  set price-no exp(q-no  / liquidity) / Z
end


; ~~~~~~~~~~~~~~~~~~~~ event state ~~~~~~~~~~~~~~~~~~~~

to draw-event ; box
  ifelse (random-float 1) < event-probability [ set event-outcome 1 ] [ set event-outcome 0 ]
end


; ~~~~~~~~~~~~~~~~~~~~ agent weight update ~~~~~~~~~~~~~~~~~~~~

to update-market-config
  update-weight
end

to update-weight
  ask bettors [compute-score]
  normalize-score
  ask bettors [
    weight-step
  ]
  update-size
end

to compute-score ; in bettor ; box
  set score (- log-loss event-outcome submitted)
end

to-report log-loss [target pred]
  let eps 1e-12
  let p  max list eps min list (1 - eps) pred  ;; clamp pred
  report -1 * ( target * ln p + (1 - target) * ln (1 - p ) )
end

to normalize-score ; box
  let max_ max [score] of bettors
  let min_ min [score] of bettors
  let range_ max_ - min_
  if range_ = 0 [ set range_ 1 ]
  ask bettors [
    set score (score - min_) / range_
  ]
end

to weight-step ; in bettor ; box
  set weight weight * exp(lr * score)
end


; ~~~~~~~~~~~~~~~~~~~ graph helpers ~~~~~~~~~~~~~~~~~~~~

to create-graph
  ask links [ die ]
  if num-edges > max-edges [ set num-edges max-edges ]
  while [ count links < num-edges ] [
    ask one-of bettors [
      create-link-with one-of other turtles
    ]
  ]
end

to-report max-edges
  report num-bettors * (num-bettors - 1) / 2
end


; ~~~~~~~~~~~~~~~~~~~ interface helpers ~~~~~~~~~~~~~~~~~~~~

to-report weighted-prediction
  let wq-yes sum [weight * (pos-yes - pos-no)] of bettors
  let wq-no sum [weight * (pos-no - pos-yes)] of bettors
  let Z (exp(wq-yes / liquidity) + exp(wq-no / liquidity))
  report exp(wq-yes / liquidity) / Z
end

to update-size
  let max-weights max [weight] of bettors
  let min-weights min [weight] of bettors
  let range_ max-weights - min-weights
  if range_ = 0 [set range_ 1]
  ask bettors [
    set size 0.5 + ((weight - min-weights) / range_)
  ]
end

to-report market-prediction-std
  ifelse length yes-price-record >= 2 [report standard-deviation yes-price-record] [report "nan"]
end

to-report clustering-coefficient
  report mean [ nw:clustering-coefficient ] of bettors
end

to-report clip [x]
  let eps 1e-6

  if x > 0 [
    report max list x eps
  ]

  if x < 0 [
    report min list x (- eps)
  ]

  report eps
end











@#$#@#$#@
GRAPHICS-WINDOW
1026
30
1419
424
-1
-1
11.67
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

SLIDER
0
438
172
471
num-bettors
num-bettors
0
1000
100.0
1
1
NIL
HORIZONTAL

BUTTON
0
404
66
437
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
67
404
130
437
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
824
10
1024
55
YES price
price-yes
17
1
11

MONITOR
824
102
1024
147
net outstanding YES bet tokens
q-yes
17
1
11

SLIDER
173
438
345
471
liquidity
liquidity
0.01
10
1.0
0.01
1
NIL
HORIZONTAL

PLOT
0
10
411
130
YES Price
ticks
price
0.0
0.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot price-yes"

SLIDER
173
472
345
505
market-duration
market-duration
0
1000
300.0
1
1
NIL
HORIZONTAL

PLOT
0
131
411
251
NO price
ticks
price
0.0
0.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot price-no"

MONITOR
824
56
1024
101
NO price
price-no
17
1
11

MONITOR
824
148
1024
193
net outstanding NO bet tokens
q-no
17
1
11

TEXTBOX
1030
10
1260
28
blue: betting YES, red: betting NO
11
0.0
1

SLIDER
173
540
345
573
event-probability
event-probability
0
1
0.75
0.01
1
NIL
HORIZONTAL

SLIDER
0
540
172
573
lr
lr
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
173
506
345
539
market-attempts
market-attempts
0
100
100.0
1
1
NIL
HORIZONTAL

PLOT
412
10
823
130
weighted prediction
repetition
probability
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks = market-duration [\nplot weighted-prediction\n]"

PLOT
412
131
823
251
event outcome
repetition
outcome
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks = market-duration [\nplot event-outcome\n]"

SLIDER
0
472
172
505
belief-min
belief-min
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
0
506
172
539
belief-max
belief-max
0
1
0.32
0.01
1
NIL
HORIZONTAL

MONITOR
824
194
1024
239
weighted YES probability
weighted-prediction
17
1
11

MONITOR
824
240
1024
285
weighted NO probability
1 - weighted-prediction
17
1
11

SLIDER
423
472
595
505
herding
herding
0
1
0.0
0.01
1
NIL
HORIZONTAL

MONITOR
824
286
1024
331
market result std
market-prediction-std
17
1
11

PLOT
0
252
411
372
Belief Distribution
value
count
0.0
0.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "set-plot-x-range 0 1\nset-histogram-num-bars 50\nhistogram [effective-belief] of bettors"

SLIDER
423
438
595
471
constraint
constraint
0.01
1
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
626
471
798
504
overconfidence-temp
overconfidence-temp
0.01
10
3.0
0.01
1
NIL
HORIZONTAL

SLIDER
423
506
595
539
num-edges
num-edges
0
10000
248.0
1
1
NIL
HORIZONTAL

MONITOR
824
332
1024
377
clustering coefficient
clustering-coefficient
17
1
11

SLIDER
423
540
595
573
sharing-influence
sharing-influence
0
1
0.0
0.01
1
NIL
HORIZONTAL

SWITCH
626
437
798
470
overconfident?
overconfident?
1
1
-1000

SWITCH
626
505
798
538
confirmation-bias
confirmation-bias
1
1
-1000

SLIDER
626
539
798
572
confirmation
confirmation
0.01
1
0.01
0.01
1
NIL
HORIZONTAL

SLIDER
828
437
1000
470
CARA-min
CARA-min
-5
0
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
828
471
1000
504
CARA-max
CARA-max
0
5
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
828
505
1000
538
loss-aversion-min
loss-aversion-min
1
4
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
828
539
1000
572
loss-aversion-max
loss-aversion-max
1
4
1.02
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
