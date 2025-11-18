extensions [nw]

globals [
  q-yes
  q-no
  wq-yes
  wq-no
  price-yes
  price-no
  wprice-yes
  wprice-no
  event-outcome

  eps
  sample-size
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
  cumulative-trading-cost
  profit
  pos-yes
  pos-no
  score
  weight

  ; tentative memory
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
  betting-round
  tick
end

to collect-data
  repeat market-attempts [
    restart
    complete-market
    update-market-config
  ]
  set-weight
  set sample-size sample-size + market-attempts
end

to restart
  reset-bettors
  reset-market-states
  reset-ticks
end


; ~~~~~~~~~~~~~~~~~~~~ variable control ~~~~~~~~~~~~~~~~~~~~

to initialize-globals ; box
  set q-yes 0
  set q-no  0
  set price-yes 0.5
  set price-no 0.5
  set wprice-yes 0.5
  set wprice-no 0.5
  set-default-shape bettors "person"
  set eps 1e-6
end

to reset-bettors ; box
  ask bettors [
    set color gray
    set pos-yes 0
    set pos-no  0
    set decision (list 0 0)
    set belief inner-belief
    set cumulative-trading-cost 0
  ]
end

to reset-market-states ; box
  set q-yes 0
  set q-no  0
  set price-yes 0.5
  set price-no 0.5
  set wprice-yes 0.5
  set wprice-no 0.5
  set wq-yes 0
  set wq-no 0
  set-current-plot "Price"
  clear-plot
  set-current-plot "Weighted Price (Prediction)"
  clear-plot
  random-seed random 1000000000
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
  set CARA CARA-min + random-float (CARA-max - CARA-min)
  set loss-aversion loss-aversion-min + random-float (loss-aversion-max - loss-aversion-min)
  set decision (list 0 0)
end


; ~~~~~~~~~~~~~~~~~~~~ market completion ~~~~~~~~~~~~~~~~~~~~

to complete-market
  repeat market-duration [
    betting-round
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
end

to-report optimal-bet-position ; in bettor ; box
  set effective-belief (clamp-p (distort-belief))
  report optimality
end

to resolve-position ; in bettor ; box
  let delta-yes first  decision
  let delta-no  last   decision

  set pos-yes pos-yes + delta-yes
  set pos-no  pos-no + delta-no

  set cumulative-trading-cost cumulative-trading-cost + (cost-function delta-yes delta-no)

  if pos-yes = pos-no [set color gray]
  if pos-yes > pos-no [set color blue]
  if pos-yes < pos-no [set color red]

  set q-yes q-yes + delta-yes
  set q-no q-no + delta-no

  set wq-yes wq-yes + (weight * delta-yes)
  set wq-no wq-no + (weight * delta-no)
end


; ~~~~~~~~~~~~~~~~~~~ belief distortion ~~~~~~~~~~~~~~~~~~~~

to-report distort-belief
  set belief herd belief
  set belief network-information-sharing belief

  let p-hat belief

  if overconfident? = true [set p-hat overconfidence p-hat]

  if p-hat = 1 [set p-hat p-hat - eps]
  if p-hat = 0 [set p-hat p-hat + eps]

  report p-hat
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


; ~~~~~~~~~~~~~~~~~~~ agent decision ~~~~~~~~~~~~~~~~~~~~

to-report step-constraint [bet-size]
  let clipped bet-size
  ifelse bet-size > 0 [
    set clipped min list constraint bet-size
  ][
    set clipped max list (- constraint) bet-size
  ]
  report clipped
end

to-report cost-function [ky kn]
  let after liquidity * ln ( ( exp ( (q-yes + ky ) / liquidity ) ) + ( exp ( ( q-no + kn ) / liquidity ) ) + eps )
  let before liquidity * ln ( ( exp ( q-yes / liquidity ) ) + ( exp ( q-no / liquidity ) ) + eps )
  report after - before
end

to-report payoff-function [ky kn side]
  let payoff 0
  if side = "yes" [
    set payoff ky - (cost-function ky kn)
  ]
  if side = "no" [
    set payoff kn - (cost-function ky kn)
  ]
  report payoff
end

to-report utility-function [ky kn side]
  let payoff payoff-function ky kn side
  if CARA = 0 [ ifelse payoff >= 0 [report payoff] [report loss-aversion * payoff] ]
  ifelse payoff >= 0 [report ( (1 - ( exp  ( - (payoff * CARA) ) ) ) / CARA ) ] [report ( (1 - loss-aversion * ( exp  ( - (payoff * CARA) ) ) ) / CARA )]
end

to-report expected-utility-function [ky kn]
  let yes-utility utility-function ky kn "yes"
  let yes-prob effective-belief
  let no-utility utility-function ky kn "no"
  let no-prob (1 - effective-belief)

  report yes-prob * yes-utility + no-prob * no-utility
end

to-report optimality
  let k-diff-star-abs-yes 0
  let k-diff-star-abs-no 0

  ifelse numerical? = true [
    set k-diff-star-abs-yes abs golden-search 0 10
    set k-diff-star-abs-no abs golden-search (- 10) 0
  ][
    let gap (q-yes - q-no)
    set k-diff-star-abs-yes abs ( ( liquidity * ( ln ( effective-belief / ( loss-aversion * ( 1 - effective-belief ) ) ) ) ) - gap )
    set k-diff-star-abs-no abs ( ( liquidity * ( ln ( ( 1 - effective-belief ) / ( loss-aversion * effective-belief ) ) ) ) + gap )
  ]

  let k-step-yes step-constraint k-diff-star-abs-yes
  let k-step-no step-constraint k-diff-star-abs-no

  let baseline expected-utility-function 0 0

  let yes-long-eutility expected-utility-function k-diff-star-abs-yes 0
  let no-short-eutility expected-utility-function 0 (- k-diff-star-abs-yes)

  let yes-short-eutility expected-utility-function (- k-diff-star-abs-no) 0
  let no-long-eutility expected-utility-function 0 k-diff-star-abs-no

  let choice ( list (list k-step-yes 0) (list 0 (- k-step-yes)) (list 0 k-step-no) (list (- k-step-no) 0) )
  let choice-eutility (list yes-long-eutility no-short-eutility no-long-eutility yes-short-eutility)

  report argmax choice choice-eutility baseline
end


; ~~~~~~~~~~~~~~~~~~~ numerical ~~~~~~~~~~~~~~~~~~~~

to-report golden-search [lo hi]
  let φ ((sqrt 5) - 1) / 2
  let x1 hi - φ * (hi - lo)
  let x2 lo + φ * (hi - lo)
  let f1 expected-utility-function x1 0
  let f2 expected-utility-function x2 0
  while [hi - lo > eps] [
    ifelse f1 < f2 [
      set lo x1
      set x1 x2
      set f1 f2
      set x2 lo + φ * (hi - lo)
      set f2 expected-utility-function x2 0
    ][
      set hi x2
      set x2 x1
      set f2 f1
      set x1 hi - φ * (hi - lo)
      set f1 expected-utility-function x1 0
    ]
  ]
  report (lo + hi) / 2
end


; ~~~~~~~~~~~~~~~~~~~~ market state update ~~~~~~~~~~~~~~~~~~~~

to update-market-state ;in bettor
  update-bet-price
  update-weighted-bet-price
end

to update-bet-price ; box
  let Z (exp(q-yes / liquidity) + exp(q-no / liquidity))
  set price-yes exp(q-yes / liquidity) / Z
  set price-no exp(q-no  / liquidity) / Z
end

to update-weighted-bet-price
  let Z (exp(wq-yes / liquidity) + exp(wq-no / liquidity))
  set wprice-yes exp(wq-yes / liquidity) / Z
  set wprice-no exp(wq-no / liquidity) / Z
end


; ~~~~~~~~~~~~~~~~~~~~ event state ~~~~~~~~~~~~~~~~~~~~

to draw-event ; box
  ifelse (random-float 1) < event-probability [ set event-outcome 1 ] [ set event-outcome 0 ]
end


; ~~~~~~~~~~~~~~~~~~~~ agent weight update ~~~~~~~~~~~~~~~~~~~~

to update-market-config
  ask bettors [accumulate-profit]
end

to accumulate-profit ; in bettor ; box
  let payoff (pos-yes * event-outcome) + (pos-no * (1 - event-outcome))
  set profit profit  + payoff - cumulative-trading-cost
end

to compute-weight
  ask bettors [
    set score profit
  ]
  normalize-score
  let denom sum [exp (score / weight-temperature)] of bettors
  ask bettors [
    set weight (exp score) / denom
  ]
end

to normalize-score ; box
  let baseline min [score] of bettors
  let denom (max [score] of bettors) - baseline

  if denom = 0 [ set denom 1 ]
  ask bettors [
    set score (score - baseline) / denom
  ]
end

to set-weight
  compute-weight
  update-size
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


; ~~~~~~~~~~~~~~~~~~~ helpers ~~~~~~~~~~~~~~~~~~~~

to update-size
  let max-weights max [weight] of bettors
  let min-weights min [weight] of bettors
  let range_ max-weights - min-weights
  if range_ = 0 [set range_ 1]
  ask bettors [
    set size 0.5 + ((weight - min-weights) / range_)
  ]
end

to-report clustering-coefficient
  report mean [ nw:clustering-coefficient ] of bettors
end

to-report clip [x]
  if (0 < x) and (x < eps) [
    report eps
  ]
  if (0 > x) and (x > (- eps)) [
    report (- eps)
  ]
  report x
end

to-report clamp-p [p]
  report max list eps min list (1 - eps) p
end

to-report argmax [choice choice-score baseline]
  let best-score baseline
  let best-choice list 0 0
  foreach choice-score [ action-score ->
    if action-score > best-score [
      set best-score action-score
      set best-choice item (position action-score choice-score) choice
    ]
    if action-score = best-score [
      if (random-float 1) < 0.5 [
        set best-score action-score
        set best-choice item (position action-score choice-score) choice
      ]
    ]
  ]
  report best-choice
end

to-report participation-rate
  report count bettors with [color != gray] / count bettors
end



@#$#@#$#@
GRAPHICS-WINDOW
0
10
393
404
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
399
297
571
330
num-bettors
num-bettors
0
1000
200.0
1
1
NIL
HORIZONTAL

BUTTON
399
229
460
262
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
461
229
516
262
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
394
131
599
176
YES price
price-yes
17
1
11

MONITOR
600
131
805
176
net YES bets
q-yes
17
1
11

SLIDER
399
399
571
432
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
394
10
805
130
Price
ticks
price
0.0
0.0
0.0
1.0
true
true
"" ""
PENS
"yes" 1.0 0 -13345367 true "" "ifelse ticks > 0 [plot price-yes][plot 0.5]"
"no" 1.0 0 -2674135 true "" "ifelse ticks > 0 [plot price-no][plot 0.5]"

SLIDER
602
308
774
341
market-duration
market-duration
0
1000
50.0
1
1
NIL
HORIZONTAL

MONITOR
394
177
599
222
NO price
price-no
17
1
11

MONITOR
600
177
805
222
net NO bets
q-no
17
1
11

SLIDER
602
410
774
443
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
602
342
774
375
market-attempts
market-attempts
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
399
331
571
364
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
399
365
571
398
belief-max
belief-max
0
1
0.7
0.01
1
NIL
HORIZONTAL

SLIDER
809
395
981
428
herding
herding
0
1
0.0
0.01
1
NIL
HORIZONTAL

PLOT
0
404
393
541
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
809
633
981
666
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
809
531
981
564
overconfidence-temp
overconfidence-temp
0.01
10
2.5
0.01
1
NIL
HORIZONTAL

SLIDER
809
429
981
462
num-edges
num-edges
0
1000
100.0
1
1
NIL
HORIZONTAL

MONITOR
0
542
126
587
clustering coefficient
clustering-coefficient
17
1
11

SLIDER
809
463
981
496
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
809
497
981
530
overconfident?
overconfident?
1
1
-1000

SWITCH
809
565
981
598
confirmation-bias
confirmation-bias
1
1
-1000

SLIDER
809
599
981
632
confirmation
confirmation
0.01
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
809
259
981
292
CARA-min
CARA-min
-4
4
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
809
293
981
326
CARA-max
CARA-max
-4
4
1.4
0.01
1
NIL
HORIZONTAL

SLIDER
809
327
981
360
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
809
361
981
394
loss-aversion-max
loss-aversion-max
1
4
1.84
0.01
1
NIL
HORIZONTAL

BUTTON
809
225
981
258
no-behavioral-option
set herding 0\nset sharing-influence 0\nset overconfident? false\nset confirmation-bias false\nset CARA-min 0\nset CARA-max 0\nset loss-aversion-min 1\nset loss-aversion-max 1
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
602
228
707
261
NIL
collect-data
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
517
229
572
262
NIL
restart
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
806
10
1217
130
Weighted Price (Prediction)
ticks
value
0.0
0.0
0.0
1.0
true
true
"" ""
PENS
"yes" 1.0 0 -13345367 true "" "ifelse ticks > 0 [plot wprice-yes][plot 0.5]"
"no" 1.0 0 -2674135 true "" "ifelse ticks > 0 [plot wprice-no][plot 0.5]"

SLIDER
602
376
774
409
weight-temperature
weight-temperature
0.01
10
1.0
0.01
1
NIL
HORIZONTAL

MONITOR
806
131
1010
176
weighted YES price
wprice-yes
17
1
11

MONITOR
806
177
1010
222
weighted NO price
wprice-no
17
1
11

MONITOR
1011
131
1217
176
weighted net YES bets
wq-yes
17
1
11

MONITOR
1011
177
1217
222
weighted net NO bets
wq-no
17
1
11

MONITOR
602
262
774
307
data sample size
sample-size
17
1
11

MONITOR
127
542
237
587
participation-rate
participation-rate
17
1
11

SWITCH
399
263
571
296
numerical?
numerical?
0
1
-1000

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
