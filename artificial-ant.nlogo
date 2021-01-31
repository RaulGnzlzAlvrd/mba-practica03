extensions [ rnd ]

;; BEST SOLUTION BY KOZA
;;if_food_ahead(ant.move_forward, prog3(ant.turn_left,
;;             prog2(ant.if_food_ahead(ant.move_forward, ant.turn_right),
;;             prog2(ant.turn_right, prog2(ant.turn_left, ant.turn_right))),
;;             prog2(ant.if_food_ahead(ant.move_forward, ant.turn_left), ant.move_forward)))

globals [
  global-best
]

patches-own [food]

breed [trees tree]
trees-own
[ fitness
  instructions
  nmoves
  nfood
  current-gen ]

to-report best-tree
 let best
  ["IF_FOOD_AHEAD"
    ["MOVE"]
    ["PROGN3"
      ["LEFT"]
      ["PROGN2"
        ["IF_FOOD_AHEAD" ["MOVE"] ["RIGHT"]]
        ["PROGN2"
          ["RIGHT"]
          ["PROGN2" ["LEFT"] ["RIGHT"]]]]
      ["PROGN2"
        ["IF_FOOD_AHEAD" ["MOVE"] ["LEFT"]]
        ["MOVE"]]]]
 report best
end

to setup
  clear-all
  reset-ticks
  ask patches
  [ set pcolor white
    set food false ]
  create-grid

  create-trees population-size
  [ set instructions (create-tree initial-depth)
    set heading 90
    set current-gen true ]

  ask one-of trees [
    set global-best (list instructions 0)
  ]
end

to run-global-best
  ask trees [die]
  create-trees 1 [
    set instructions (item 0 global-best)
    set heading 90
    setxy 0 0
  ]
  create-grid
  ask trees
  [ while [nmoves < max-allowed-moves]
    [eval-tree instructions]
    show nfood ]
end

to turn-left
  lt 90
end

to turn-right
  rt 90
end

to move
  fd 1
end

to-report if-food-ahead
  let val false
  ask patch-ahead 1
  [ if food = true
    [ set val true ] ]
  report val
end

to-report functional-set
  report ["IF_FOOD_AHEAD" "PROGN2" "PROGN3"]
end

to-report terminal-set
  report ["LEFT" "RIGHT" "MOVE"]
end

to-report create-tree [depth]
  ifelse (depth = 0) or (random-float 1 > random-float 1)
  [ report (list (one-of terminal-set)) ]
  [ let function one-of functional-set
    ifelse function != "PROGN3"
    [ report (list function (create-tree (depth - 1))  (create-tree (depth - 1))) ]
    [ report (list function (create-tree (depth - 1))  (create-tree (depth - 1)) (create-tree (depth - 1))) ] ]
end

to go
  create-next-gen
  if ticks >= max-generations + 1 [
    show global-best
    stop
  ]
  ask trees with [current-gen]
  [ ;;show "Excecuting"
    ;;show instructions
    while [nmoves < max-allowed-moves]
    [eval-tree instructions]
    create-grid ]

  tick
end

to create-next-gen
  ask trees with [current-gen] [set current-gen false]

  while [count (trees with [current-gen]) < population-size]
  [ ifelse random-float 1 < pc
    [ let p1 rnd:weighted-one-of (trees with [not current-gen]) [nfood]
      let p2 rnd:weighted-one-of (trees with [not current-gen]) [nfood]

      let instructions1 []
      let instructions2 []

      ask p1 [set instructions1 instructions]
      ask p2 [set instructions2 instructions]
      let childs sexual-reproduction instructions1 instructions2

      ask p1 [
        hatch 1 [
          set current-gen true
          set instructions (item 0 childs)
          set nmoves 0
          set nfood 0
        ]
      ]

      ask p2 [
        hatch 1 [
          set current-gen true
          set instructions (item 1 childs)
          set nmoves 0
          set nfood 0
        ]
      ]
    ]
    [ let p1 rnd:weighted-one-of (trees with [not current-gen]) [nfood]
      ask p1 [hatch 1 [
        set current-gen true
        set nmoves 0
        set nfood 0 ]] ] ]

  ask trees with [not current-gen] [die]
  ask trees with [current-gen] [
    set heading 90
    setxy 0 0
  ]
end

to exec-terminals [name]
  ( ifelse
    name = "LEFT"  [turn-left]
    name = "RIGHT" [turn-right]
    name = "MOVE"  [move] )
end

to eval-tree [treel] ;; tree of type list GRAPHIC EVAL
  if nmoves >= max-allowed-moves [stop]
  ( ifelse
    length treel = 1
    [ exec-terminals first treel
      set nmoves (nmoves + 1)
      let food-here false
      ask patch-here
      [ if food = true
        [ set food-here true
          empty ] ]
      if food-here [
        set nfood (nfood + 1)
      ]
    ]
    first treel = "PROGN3"
    [ eval-tree (item 1 treel)
      eval-tree (item 2 treel)
      eval-tree (item 3 treel) ]
    first treel = "PROGN2"
    [ eval-tree (item 1 treel)
      eval-tree (item 2 treel) ]
    first treel = "IF_FOOD_AHEAD"
    [ let food-ahead if-food-ahead
      ifelse food-ahead
      [eval-tree (item 1 treel)]
      [eval-tree (item 2 treel)] ] )
end

to create-grid
  ask patch 1 0   [food-trail] ;;1
  ask patch 2 0   [food-trail] ;;2
  ask patch 3 0   [food-trail] ;;3
  ask patch 3 -1  [food-trail] ;;4
  ask patch 3 -2  [food-trail] ;;5
  ask patch 3 -3  [food-trail] ;;6
  ask patch 3 -4  [food-trail] ;;7
  ask patch 3 -5  [food-trail] ;;8

  ask patch 4 -5  [food-trail] ;;9
  ask patch 5 -5  [food-trail] ;;10
  ask patch 6 -5  [food-trail] ;;11

  ask patch 7 -5  [empty] ;;

  ask patch 8 -5  [food-trail] ;;12
  ask patch 9 -5  [food-trail] ;;13
  ask patch 10 -5 [food-trail] ;;14
  ask patch 11 -5 [food-trail] ;;15
  ask patch 12 -5 [food-trail] ;;16
  ask patch 12 -6 [food-trail] ;;17
  ask patch 12 -7 [food-trail] ;;18
  ask patch 12 -8 [food-trail] ;;19
  ask patch 12 -9 [food-trail] ;;20

  ask patch 12 -10 [empty] ;;

  ask patch 12 -11 [food-trail] ;;21
  ask patch 12 -12 [food-trail] ;;22
  ask patch 12 -13 [food-trail] ;;23
  ask patch 12 -14 [food-trail] ;;24

  ask patch 12 -15 [empty] ;;
  ask patch 12 -16 [empty] ;;

  ask patch 12 -17 [food-trail] ;;25
  ask patch 12 -18 [food-trail] ;;26
  ask patch 12 -19 [food-trail] ;;27
  ask patch 12 -20 [food-trail] ;;28
  ask patch 12 -21 [food-trail] ;;29
  ask patch 12 -22 [food-trail] ;;30
  ask patch 12 -23 [food-trail] ;;31

  ask patch 12 -24 [empty]

  ask patch 11 -24 [food-trail] ;;32
  ask patch 10 -24 [food-trail] ;;33
  ask patch 9  -24 [food-trail] ;;34
  ask patch 8  -24 [food-trail] ;;35
  ask patch 7  -24 [food-trail] ;;36

  ask patch 6  -24 [empty]
  ask patch 5  -24 [empty]

  ask patch 4  -24 [food-trail] ;;37
  ask patch 3  -24 [food-trail] ;;38

  ask patch 2  -24 [empty]
  ask patch 1  -24 [empty]

  ask patch 1  -25 [food-trail] ;;39
  ask patch 1  -26 [food-trail] ;;40
  ask patch 1  -27 [food-trail] ;;41
  ask patch 1  -28 [food-trail] ;;42

  ask patch 1  -29 [empty]
  ask patch 1  -30 [empty]

  ask patch 2  -30 [food-trail] ;;43
  ask patch 3  -30 [food-trail] ;;44
  ask patch 4  -30 [food-trail] ;;45
  ask patch 5  -30 [food-trail] ;;46

  ask patch 6  -30 [empty]
  ask patch 7  -30 [empty]

  ask patch 7  -29 [food-trail] ;;47
  ask patch 7  -28 [food-trail] ;;48

  ask patch 7  -27 [empty]

  ask patch 8  -27 [food-trail] ;;49
  ask patch 9  -27 [food-trail] ;;50
  ask patch 10 -27 [food-trail] ;;51
  ask patch 11 -27 [food-trail] ;;52
  ask patch 12 -27 [food-trail] ;;53
  ask patch 13 -27 [food-trail] ;;54
  ask patch 14 -27 [food-trail] ;;55

  ask patch 15 -27 [empty]
  ask patch 16 -27 [empty]

  ask patch 16 -26 [food-trail] ;;56
  ask patch 16 -25 [food-trail] ;;57
  ask patch 16 -24 [food-trail] ;;58

  ask patch 16 -23 [empty]
  ask patch 16 -22 [empty]

  ask patch 16 -21 [food-trail] ;;59
  ask patch 16 -20 [food-trail] ;;60
  ask patch 16 -19 [food-trail] ;;61
  ask patch 16 -18 [food-trail] ;;62

  ask patch 16 -17 [empty] ;;
  ask patch 16 -16 [empty]
  ask patch 16 -15 [empty]

  ask patch 17 -15 [food-trail] ;;63

  ask patch 18 -15 [empty] ;;
  ask patch 19 -15 [empty]
  ask patch 20 -15 [empty]

  ask patch 20 -14 [food-trail] ;;64
  ask patch 20 -13 [food-trail] ;;65

  ask patch 20 -12 [empty]
  ask patch 20 -11 [empty]

  ask patch 20 -10 [food-trail] ;;66
  ask patch 20 -9  [food-trail] ;;67
  ask patch 20 -8  [food-trail] ;;68
  ask patch 20 -7  [food-trail] ;;69

  ask patch 20 -6  [empty]
  ask patch 20 -5  [empty]

  ask patch 21 -5  [food-trail] ;;70
  ask patch 22 -5  [food-trail] ;;71

  ask patch 23 -5  [empty]
  ask patch 24 -5  [empty]

  ask patch 24 -4  [food-trail] ;;72
  ask patch 24 -3  [food-trail] ;;73

  ask patch 24 -2  [empty]

  ask patch 25 -2  [food-trail] ;;74
  ask patch 26 -2  [food-trail] ;;75
  ask patch 27 -2  [food-trail] ;;76

  ask patch 28 -2  [empty]
  ask patch 29 -2  [empty] ;;

  ask patch 29 -3  [food-trail] ;;77
  ask patch 29 -4  [food-trail] ;;78

  ask patch 29 -5  [empty]

  ask patch 29 -6  [food-trail] ;;79

  ask patch 29 -7  [empty]
  ask patch 29 -8  [empty]

  ask patch 29 -9  [food-trail] ;;80

  ask patch 29 -10  [empty]
  ask patch 29 -11  [empty]

  ask patch 29 -12  [food-trail] ;;81

  ask patch 29 -13  [empty]
  ask patch 29 -14  [empty] ;;

  ask patch 28 -14  [food-trail] ;;82
  ask patch 27 -14  [food-trail] ;;83
  ask patch 26 -14  [food-trail] ;;84

  ask patch 25 -14  [empty]
  ask patch 24 -14  [empty]
  ask patch 23 -14  [empty]

  ask patch 23 -15  [food-trail] ;;85

  ask patch 23 -16  [empty]
  ask patch 23 -17  [empty]
  ask patch 23 -18  [empty]

  ask patch 24 -18  [food-trail] ;;86

  ask patch 25 -18  [empty]
  ask patch 26 -18  [empty]
  ask patch 27 -18  [empty]

  ask patch 27 -19  [food-trail] ;;87

  ask patch 27 -20  [empty]
  ask patch 27 -21  [empty]
  ask patch 27 -22  [empty]

  ask patch 26 -22  [food-trail] ;;88

  ask patch 25 -22  [empty]
  ask patch 24 -22  [empty]
  ask patch 23 -22  [empty]

  ask patch 23 -23  [food-trail] ;;89
end

to food-trail
  set pcolor black
  set food true
end

to empty
  set pcolor yellow
  set food false
end

;; TODO: Terminar esta función
to-report mutate-tree [treel]
  report (random (length unnest treel))
end

to-report sexual-reproduction [t1 t2]
  let point1 random-index t1
  let point2 random-index t2

  let subtree1 (item 0 (get-subtree-at t1 point1))
  let subtree2 (item 0 (get-subtree-at t2 point2))

  let child1 (item 0 insert-in-tree t1 point1 subtree2)
  let child2 (item 0 insert-in-tree t2 point2 subtree1)

  if not valid-tree child1 max-depth [set child1 t1]
  if not valid-tree child2 max-depth [set child2 t2]
  report (list child1 child2)
end

to-report unnest [xs]
  if empty? xs [report []]
  if is-list? (first xs) [report sentence (unnest first xs) (unnest (remove-item 0 xs))]
  report sentence (list (first xs)) (unnest (remove-item 0 xs))
end

to-report random-index [t]
  report random (length (unnest t))
end

to-report insert-in-tree [tree1 index subtree]
  ( ifelse
    index = 0 [report (list subtree index)]
    length tree1 = 1 [report (list tree1 index)]
    [ let i1 (insert-in-tree (item 1 tree1) (index - 1) subtree)
      set index (item 1 i1)
      if index = 0 [report (list (replace-item 1 tree1 (item 0 i1)) index)]

      let i2 (insert-in-tree (item 2 tree1) (index - 1) subtree)
      set index (item 1 i2)
      if index = 0 [report (list (replace-item 2 tree1 (item 0 i2)) index)]

      if first tree1 = "PROGN3"
      [ let i3 (insert-in-tree (item 3 tree1) (index - 1) subtree)
        set index (item 1 i3)
        if index = 0 [report (list (replace-item 3 tree1 (item 0 i3)) index)] ]
      report (list tree1 index)
    ]
  )
end

to-report get-subtree-at [t index]
  ( ifelse
    index = 0 [report (list t index)]
    length t = 1 [report (list t index)]
    [ let i1 (get-subtree-at (item 1 t) (index - 1))
      set index (item 1 i1)
      if index = 0 [report i1]

      let i2 (get-subtree-at (item 2 t) (index - 1))
      set index (item 1 i2)
      if index = 0 [report i2]

      if first t = "PROGN3"
      [ let i3 (get-subtree-at (item 3 t) (index - 1))
        set index (item 1 i3)
        if index = 0 [report i3] ]
      report (list t index)
    ]
  )
end

to-report valid-tree [t depth]
  report (get-depth t) <= depth
end

to-report get-depth [t]
  ifelse length t = 1
  [report 0]
  [ let d1 get-depth (item 1 t)
    let d2 get-depth (item 2 t)
    ifelse first t = "PROGN3"
    [ let d3 get-depth (item 3 t)
      report max (list d1 d2 d3) + 1]
    [ report max (list d1 d2) + 1] ]
end

;; TODO: Eliminar esta función
to test
  let depth 3

  let t1 (create-tree depth)
  show "t1"
  show t1

  let t2 (create-tree depth)
  show "t2"
  show t2

  let childs (sexual-reproduction t1 t2)
  show "c1"
  show (item 0 childs)
  show "c2"
  show (item 1 childs)
end


to-report get-best
  let best 0
  let t []
  let best-t (max-one-of trees [nfood])
  ask best-t [
    set best nfood
    set t instructions
  ]

  if ((item 1 global-best) < best) [
    set global-best (list t best)
  ]

  report best
end
@#$#@#$#@
GRAPHICS-WINDOW
235
23
826
615
-1
-1
17.67
1
10
1
1
1
0
1
1
1
0
32
-32
0
0
0
1
generation
30.0

BUTTON
19
81
86
115
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
118
82
181
115
NIL
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
13
32
185
65
population-size
population-size
10
600
500.0
10
1
NIL
HORIZONTAL

SLIDER
11
470
189
503
initial-depth
initial-depth
1
15
6.0
1
1
levels
HORIZONTAL

SLIDER
12
512
184
545
max-depth
max-depth
1
25
17.0
1
1
levels
HORIZONTAL

SLIDER
11
420
215
453
max-allowed-moves
max-allowed-moves
100
1000
400.0
50
1
NIL
HORIZONTAL

SLIDER
17
282
189
315
pc
pc
0
1
0.9
0.01
1
NIL
HORIZONTAL

SLIDER
21
235
193
268
max-generations
max-generations
1
55
51.0
1
1
NIL
HORIZONTAL

PLOT
20
630
463
826
Generación vs Fitness
generacion
fitness
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Average" 1.0 0 -955883 true "" "plot mean [nfood] of trees"
"Best" 1.0 0 -13840069 true "" "plot get-best"

BUTTON
580
708
676
741
Run best
run-global-best
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

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
NetLogo 6.1.1
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
