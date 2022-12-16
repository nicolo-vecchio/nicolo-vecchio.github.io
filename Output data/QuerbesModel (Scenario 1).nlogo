;Copyright (C) 2017 Adrien Querbes
;This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.
;This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
;You can download a copy of the GNU General Public License by clicking here; you can also get a printed copy writing to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.



;;;;;;;;;;;;;;;;;
;;; Variables ;;;
;;;;;;;;;;;;;;;;;

breed [hosts host]
breed [guests guest]
breed [drawers drawer] ;; lines showing deciles on the

globals [
  host-number
  guest-number

  global-prices
  global-price-variance
  global-last-transaction-guests
  global-last-alignment-guests
  global-last-transaction-date-hosts
  global-last-transaction-date-guests
  global-last-grade-guests
  global-cumul-transac-vol-hosts
  global-cumul-transac-vol-guests
  global-cumul-transac-val-hosts
  global-cumul-transac-val-guests

]

turtles-own [
  value-transaction-list
  cumul-transac-vol
  cumul-transac-val
  date-transaction-list
  decile
  grades
  alignments
]

hosts-own [
  price
  price-list
  quality
  booked
  tick-price-change
;;  occupancy
  grade
]

guests-own [
  budget
]


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
to setup ;;;
;;;;;;;;;;;;
  clear-all

  ;; initiate local parameters and variables
  ; create random seed (fixed random seed for paired BehaviorSpace)
  if local-random-seed = 0 [
    set local-random-seed new-seed
    random-seed local-random-seed
    ]

  ; iniate the populations
  set host-number round (population / 2)
  set guest-number host-number

  setup-world

  if behaviorspace-run-number = 0 [setup-gui]
  reset-ticks
end

;;;;;;;;;;;;;;;;;
to setup-world;;;
;;;;;;;;;;;;;;;;;

 let loc-increment 100000 / host-number

 foreach n-values (host-number) [ ?1 -> (?1 + 1) * loc-increment ] [ ?1 -> ;; hosts uniformely distributed on quality
    create-hosts 1 [
      let price-random ((random 100) + 1) * 1000
      let price-real ?1
      set price init-price-randomness * price-random + (1 - init-price-randomness) * price-real
    set price-list (list price)
    set tick-price-change (list 0)
    set quality ?1
    set decile ceiling (quality / 10000)
    set grades (list)
    set alignments (list 0)
    set value-transaction-list (list 0)
    set date-transaction-list (list 0)
    set tick-price-change (list 0)
  ]
 ]

 foreach n-values (guest-number) [ ?1 -> (?1 + 1) * loc-increment ] [ ?1 -> ;; guests uniformely distributed on budget
  create-guests 1 [
    set budget ?1
    set decile ceiling (budget / 10000)
    set grades (list)
    set alignments (list 0)
    set value-transaction-list (list 0)
    set date-transaction-list (list 0)
    ]
 ]

end

;;;;;;;;;;;;;;;;;
to setup-gui  ;;;
;;;;;;;;;;;;;;;;;

  ask hosts [
    set shape "houser"
    set color green
    setxy (price / 100000) * 29 (quality / 100000) * 29 + 30
  ]

  ask guests [
    set shape "face neutral"
    set color white
    setxy (budget / 100000) * 29 random-float 29
  ]

  ask patches with [pycor >= 30] [set pcolor white]
  create-drawers 1 [
    set color black
    ; draw the price=quality diagonal
    setxy 29 59 ; start top right
    pen-down
    setxy 0 30 ; end middle left
    pen-up
    ; draw the vertical price = 0.5 line
    setxy 29 / 2 59
    pen-down
    setxy 29 / 2 30
    pen-up
  ]
  ; draw the horizontal deciles for hosts
  let list-ycor n-values 9 [ ?1 -> ?1 * 3 + 32 ]
  foreach list-ycor [ ?1 ->
    ask drawers [
      setxy 0 ?1
      pen-down
      setxy 29 ?1
      pen-up
    ]
  ]
  ask drawers [die] ;; remove drawers

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run-time procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;
to go ;;;
;;;;;;;;;
  if (ticks > number-steps) [
    get-data
    stop
    ]
  init-run

  match

  adjust-price

;; if ((ticks + 1) mod update-data-frequency = 0) and (ticks > 0) [get-data]
if get-data? and (ticks mod update-data-frequency = 0) and (ticks > 0) [
  get-data
;;  print ticks
  ]

  tick
end

;;;;;;;;;;;;;;;
to init-run ;;;
;;;;;;;;;;;;;;;
  if behaviorspace-run-number = 0 [
    ask guests [
      set color white
      set shape "face neutral"
    ]
    ask hosts [
      set color green
    ]
    ]
    ask hosts with [booked != 0] [
    set booked 0
  ]

end

;;;;;;;;;;;;
to match ;;;
;;;;;;;;;;;;

ask guests [

  ;; the guest get a list of all the hosts in its price range and not yet hosting
  let local-guest self
  let local-budget [budget] of self
  let local-hosts hosts with [booked = 0 and price <= local-budget and price >= (local-budget - (price-range-search * 1000))]
  let nb-local-hosts count local-hosts
;;  print (list 1 [who] of local-guest [local-budget] of local-guest nb-local-hosts)
  ifelse nb-local-hosts > 0 [ ;; if there are hosts available

    let local-choice nobody
    ifelse guest-preference = "price-pref" [ ;; filter first with lowest price, then grade
      let loc-cheapest-price min [price] of local-hosts
      set local-hosts local-hosts with [price <= (1 + tau-price) * loc-cheapest-price]
      set nb-local-hosts count local-hosts
      ifelse nb-local-hosts > 1[ ;; if there is a tie, filter with reputation
        let loc-highest-reputation max [grade] of local-hosts
        set local-hosts local-hosts with [grade >= (1 - tau-reputation) * loc-highest-reputation]
        set local-choice one-of local-hosts
      ][ ;; else set the local choice
      set local-choice one-of local-hosts
      ]
    ][
    ifelse guest-preference = "fairness-pref" [   ;; grade first, then lowest price
      let loc-highest-reputation max [grade] of local-hosts
      set local-hosts local-hosts with [grade >= (1 - tau-reputation) * loc-highest-reputation]
      set local-choice one-of local-hosts
      set nb-local-hosts count local-hosts
      ifelse nb-local-hosts > 1[ ;; if there is a tie, filter with price
        let loc-cheapest-price min [price] of local-hosts
        set local-hosts local-hosts with [price <= (1 + tau-price) * loc-cheapest-price]
        set local-choice one-of local-hosts
      ][ ;; else this is the local choice
      set local-choice one-of local-hosts
      ]
    ][  ;; "quality-pref" ;; grade first, then highest price
    let loc-highest-reputation max [grade] of local-hosts
    set local-hosts local-hosts with [grade >= (1 - tau-reputation) * loc-highest-reputation]
    set local-choice one-of local-hosts
    set nb-local-hosts count local-hosts
    ifelse nb-local-hosts > 1[ ;; if there is a tie, filter with price
      let loc-highest-price max [price] of local-hosts
      set local-hosts local-hosts with [price >= (1 - tau-price) * loc-highest-price]
      set local-choice one-of local-hosts
    ][ ;; else this is the local choice
    set local-choice one-of local-hosts
    ]
    ]
    ]

;; host has been found, finalize booking and visit
    let local-alignement 0
    let local-grade 0
    let local-price 0
    ask local-choice [
      set booked [who] of local-guest
      set local-alignement price - quality
      set local-price price
      if local-alignement <= 0 [
        set local-grade 1
      ]
      set grades lput local-grade grades
      set alignments lput local-alignement alignments
      set value-transaction-list lput local-price value-transaction-list
      set cumul-transac-vol cumul-transac-vol + 1
      set cumul-transac-val cumul-transac-val + local-price
      set date-transaction-list lput ticks date-transaction-list
    ]
    set grades lput local-grade grades
    set alignments lput local-alignement alignments
    set value-transaction-list lput local-price value-transaction-list
    set cumul-transac-vol cumul-transac-vol + 1
    set cumul-transac-val cumul-transac-val + local-price
    set date-transaction-list lput ticks date-transaction-list
  ][ ;; no hosts available
  set grades lput -1 grades
  ]
]

ask hosts with [booked = 0] [  ;; update the grades of hosts that have not been selected
  set grades lput -1 grades ;; this host learns that it doesn't had guests this time
]

if behaviorspace-run-number = 0 [
  ask guests with [last grades = 1] [
    set color 115
    set shape "face happy"
  ]
  ask guests with [last grades = 0] [
    set color 115
    set shape "face sad"
  ]

  ask hosts with [booked != 0] [
    set color red
  ]
]


;; compute grade with decreasing weights
let local-weights n-values (ticks + 1) [ ?1 -> (?1 + 1) ^ (ln(past-reviews-range) / ln(2000)) ]
set local-weights reverse local-weights
ask hosts [
  let grades-local (map [ [?1 ?2] -> ?1 * ?2 ] grades local-weights)

  set grades-local filter [ ?1 -> ?1 >= 0 ] grades-local

  set grade sum grades-local

]




end

;;;;;;;;;;;;;;;;;;;
to adjust-price ;;; restart-after-change?
;;;;;;;;;;;;;;;;;;;

;; all hosts can compute a proabbility to change price

ifelse new-adjust-price? [ ;; adjust-price JEEC rev (success - failures / local grades)
    ask hosts [ ;; adjust price old version
    let failure 0
    let success 0
    let local-grades sublist grades (last tick-price-change) (ticks + 1)
    ifelse price-strategy = "occupancy" [
      set failure length (filter [ ?1 -> ?1 = -1 ] local-grades)
      set success length (filter [ ?1 -> ?1 >= 0 ] local-grades)
    ][
    set failure length (filter [ ?1 -> ?1 < 1 ] local-grades)
    set success length (filter [ ?1 -> ?1 = 1 ] local-grades)
    ]

    ifelse failure > success [ ;;opportunity to reduce their price
      if random-float 1 < ((failure - success) / length local-grades) * proba-action [
        let temp-price (price - 1000)
        if temp-price < 0 [set temp-price 0]
        set price-list lput temp-price price-list
        set price temp-price
        set tick-price-change lput ticks tick-price-change
      ]
    ][ ;; opportunity to increases their price
    if (random-float 1 < ((success - failure) / length local-grades) * proba-action) and failure != success [
      let temp-price (price + 1000)
;;      if temp-price > 100000 [set temp-price 100000]
      set price-list lput temp-price price-list
      set price temp-price
      set tick-price-change lput ticks tick-price-change
    ]
    ]
  ]

][
  ask hosts [ ;; adjust price old version (success / local-grades or failure / local-grades)
    let failure 0
    let success 0
    let local-grades sublist grades (last tick-price-change) (ticks + 1)
    ifelse price-strategy = "occupancy" [
      set failure length (filter [ ?1 -> ?1 = -1 ] local-grades)
      set success length (filter [ ?1 -> ?1 >= 0 ] local-grades)
    ][
    set failure length (filter [ ?1 -> ?1 < 1 ] local-grades)
    set success length (filter [ ?1 -> ?1 = 1 ] local-grades)
    ]
    ifelse failure > success [ ;;opportunity to reduce their price
      if random-float 1 < (failure / length local-grades) * proba-action [
        let temp-price (price - 1000)
        if temp-price < 0 [set temp-price 0]
        set price-list lput temp-price price-list
        set price temp-price
        set tick-price-change lput ticks tick-price-change
      ]
    ][ ;; opportunity to increases their price
    if (random-float 1 < (success / length local-grades) * proba-action) and failure != success [
      let temp-price (price + 1000)
;;      if temp-price > 100000 [set temp-price 100000]
      set price-list lput temp-price price-list
      set price temp-price
      set tick-price-change lput ticks tick-price-change
    ]
    ]
  ]
]


  if behaviorspace-run-number = 0 [
    ask hosts with [last tick-price-change = ticks] [ setxy (price / 100000) * 29 ((quality / 100000) * 29 + 30) ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Statistics     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
to get-data ;;;
;;;;;;;;;;;;;;;;

let sorted-hosts sort-on [quality] hosts
let sorted-guests sort-on [budget] guests
;;print (map [ [ quality ] of ? ] sorted-hosts)


set global-prices map [ ?1 -> [ price ] of ?1 ] sorted-hosts
;;set global-price-variance map [ [ variance price-list ] of ? ] sorted-hosts
set global-last-transaction-guests map [ ?1 -> [ last value-transaction-list ] of ?1 ] sorted-guests
set global-last-alignment-guests map [ ?1 -> [ last alignments ] of ?1 ] sorted-guests
set global-last-transaction-date-hosts map [ ?1 -> [ last date-transaction-list ] of ?1 ] sorted-hosts
set global-last-transaction-date-guests map [ ?1 -> [ last date-transaction-list ] of ?1 ] sorted-guests
set global-last-grade-guests map [ ?1 -> [ last grades ] of ?1 ] sorted-guests
set global-cumul-transac-vol-hosts  map [ ?1 -> [ cumul-transac-vol ] of ?1 ] sorted-hosts
set global-cumul-transac-vol-guests   map [ ?1 -> [ cumul-transac-vol ] of ?1 ] sorted-guests
set global-cumul-transac-val-hosts map [ ?1 -> [ cumul-transac-val ] of ?1 ] sorted-hosts
set global-cumul-transac-val-guests map [ ?1 -> [ cumul-transac-val ] of ?1 ] sorted-guests

end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
518
619
-1
-1
10.0
1
10
1
1
1
0
0
0
1
0
29
0
59
0
0
1
ticks
30.0

INPUTBOX
103
47
203
107
local-random-seed
1.0
1
0
Number

INPUTBOX
5
122
71
182
population
100.0
1
0
Number

MONITOR
81
128
138
173
hosts
host-number
17
1
11

MONITOR
146
128
203
173
guests
guest-number
17
1
11

BUTTON
6
10
61
43
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
4
51
98
84
setup (new seed)
set local-random-seed new-seed\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
4
188
79
248
number-steps
2000.0
1
0
Number

BUTTON
67
10
122
43
go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
127
11
182
44
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
0

CHOOSER
8
315
181
360
guest-preference
guest-preference
"price-pref" "quality-pref" "fairness-pref"
1

CHOOSER
15
379
170
424
price-strategy
price-strategy
"occupancy" "occupancy-reviews"
1

SLIDER
7
264
179
297
proba-action
proba-action
0
1
0.01
0.01
1
NIL
HORIZONTAL

INPUTBOX
538
179
693
239
price-range-search
2.0
1
0
Number

INPUTBOX
539
251
694
311
tau-price
0.1
1
0
Number

CHOOSER
538
334
676
379
comput-quality
comput-quality
"abs" "rel"
0

INPUTBOX
703
256
858
316
tau-reputation
0.1
1
0
Number

INPUTBOX
537
398
692
458
past-reviews-range
0.9
1
0
Number

INPUTBOX
538
474
693
534
update-data-frequency
50.0
1
0
Number

SWITCH
16
480
126
513
get-data?
get-data?
1
1
-1000

SLIDER
16
436
188
469
focus-occupancy
focus-occupancy
0
1
0.5
0.05
1
NIL
HORIZONTAL

SWITCH
15
524
170
557
new-adjust-price?
new-adjust-price?
1
1
-1000

INPUTBOX
14
567
169
627
init-price-randomness
0.0
1
0
Number

PLOT
540
15
860
165
Booked Hosts
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"
"pen-1" 1.0 0 -7500403 true "" "plot count hosts with [booked != 0]"

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

houser
false
0
Rectangle -7500403 true true 15 45 180 255
Rectangle -16777216 true false 15 120 90 180
Polygon -7500403 true true 180 15 285 150 180 285
Line -16777216 false 180 30 180 270

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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment.revJEEC.1.seed1to40" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>global-prices</metric>
    <metric>global-last-alignment-guests</metric>
    <metric>global-last-grade-guests</metric>
    <metric>global-cumul-transac-vol-guests</metric>
    <metric>global-cumul-transac-val-hosts</metric>
    <enumeratedValueSet variable="population">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="get-data?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="local-random-seed" first="1" step="1" last="40"/>
    <enumeratedValueSet variable="init-price-randomness">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-adjust-price?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-strategy">
      <value value="&quot;occupancy&quot;"/>
      <value value="&quot;occupancy-reviews&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="guest-preference">
      <value value="&quot;price-pref&quot;"/>
      <value value="&quot;quality-pref&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proba-action">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-steps">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-range-search">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau-price">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau-reputation">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comput-quality">
      <value value="&quot;abs&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="past-reviews-range">
      <value value="0.9"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment.revJEEC.1.faster" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>global-prices</metric>
    <metric>global-last-alignment-guests</metric>
    <metric>global-last-grade-guests</metric>
    <metric>global-cumul-transac-vol-guests</metric>
    <metric>global-cumul-transac-val-hosts</metric>
    <enumeratedValueSet variable="population">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="get-data?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="local-random-seed" first="1" step="1" last="10"/>
    <steppedValueSet variable="init-price-randomness" first="0" step="0.25" last="1"/>
    <enumeratedValueSet variable="new-adjust-price?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-strategy">
      <value value="&quot;occupancy&quot;"/>
      <value value="&quot;occupancy-reviews&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="guest-preference">
      <value value="&quot;price-pref&quot;"/>
      <value value="&quot;quality-pref&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proba-action">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-steps">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="price-range-search">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tau-price" first="0" step="0.05" last="0.25"/>
    <steppedValueSet variable="tau-reputation" first="0" step="0.05" last="0.25"/>
    <enumeratedValueSet variable="comput-quality">
      <value value="&quot;abs&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="past-reviews-range">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
