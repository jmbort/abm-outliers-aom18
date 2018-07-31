;;define breeds
breed [ resources res ] ;; Resources
breed [ entrepreneurs ent ] ;; Entrepreneurs



;;attributes of resource
resources-own[
  ;;value of resource
  value
]

entrepreneurs-own[
  ;;value of starting endowments
  endowments
  ;;value of expectations
  expectations

  ;;track movements
  movements

  ;;search neighborhood
  neighborhood

  resource-coords
  target-resource-coords
  failed-resource-coords

  ;; outlier flag
  isOutlier

]

globals[
  endowmentsMean
  totalEndowments
]

;;set up simulation
to setup

  ;;clear board
  clear-all
  ;;set ticks back to 0
  reset-ticks

  ;;generate resources
  generate-resources [ true ]
  ;;generate entrepreneurs
  generate-entrepreneurs
  ;;check outliers
  outlier-check
  update-plots
end

to go
  ifelse ticks >= tick-count [
    stop
  ] [
    ask patches[
      set pcolor black
    ]
  ]

  ifelse count entrepreneurs <= 0 [ stop ] ;;if everyone is dead, stop
  [
    ask entrepreneurs [
      ifelse length resource-coords >= 1 [
          capture-resources who
      ] [
         scan-resources who
      ]
   ]
  ]
  ;;check outliers
  outlier-check
  ;;regenerate resources?
  if RegenerateResources = true[
    ;;regenerate every twelve ticks
    if (ticks mod 12) = 0[
      generate-resources [ false ]
    ]
  ]
  tick
end

to outlier-check

   ;; standard deviation, mean of endowments
  let sd standard-deviation [endowments] of entrepreneurs
  let m mean [endowments] of entrepreneurs
  let mx max [endowments] of entrepreneurs

  ask entrepreneurs[
      ;;normal distribution use sd
      ifelse powerLaw = false[
       ;; is this an outlier?
       ifelse endowments > ( m + (sd * 2)) [
        set color red
        set isOutlier 1
       ][
        set color blue
        set isOutlier 0
       ]
      ][;;power law use quantile?
        ifelse endowments > (.9 * mx)[
          set color red
          set isOutlier 1
        ][
         set color blue
         set isOutlier 0
        ]
      ]
  ]

end

to capture-resources [ turtle-id ]
  ask turtle turtle-id [

    let resourceValue 0
    let captureValue 0

    ;;hold coords while switching between agent types
    let tempCoords []
    ;;does the ent have some targets yet?
    ifelse target-resource-coords = [] [
      ;; loop through the resources the ent knows about
      foreach resource-coords [ [coords] ->
        ;;find out more about this resource
        ask resources-on patch round(item 0 coords) round(item 1 coords)[
          ;; is this a more valuable resource?
          if (value > resourceValue)[
            set tempCoords coords
            set resourceValue value
          ]
        ]
        ;;if that resource has already been captured by someone else, remove it
        ifelse tempCoords = [][
          set resource-coords remove-item position coords resource-coords resource-coords
        ][
          ;;set ent target coords
          set target-resource-coords tempCoords
        ]
      ];;end for
    ][;; if target is already set, go for it
      ;;check to see if ent has reached their target resource
      ifelse round(xcor) != item 0 target-resource-coords or round(ycor) != item 1 target-resource-coords [
        ;; walk towards the resource
        turtle-walk who item 0 target-resource-coords item 1 target-resource-coords
        ;;check again
        if round(xcor) != item 0 target-resource-coords or round(ycor) != item 1 target-resource-coords [
          ;; attempt capture
          ifelse random 100 < captureProbability[
            ;;capture the resource
            ask resources-on patch round(xcor) round(ycor) [
               set captureValue value
               die
            ]
            set endowments endowments + captureValue
          ][;;failed capture, added to failed resources
            set failed-resource-coords target-resource-coords
          ]
          ;;remove from resource memory
          set resource-coords remove-item position target-resource-coords resource-coords resource-coords
          ;;remove the target
          set target-resource-coords []
          ;;burn an endowment
          if subtract-endowment turtle-id = true [
            die
            stop
          ]
        ]
      ][;; ent has reached resource

        ifelse random 100 < captureProbability[
            ;;capture the resource
            ask resources-on patch round(xcor) round(ycor) [
               set captureValue value
               die
            ]
            set endowments endowments + captureValue
          ][;;failed capture, added to failed resources
            set failed-resource-coords target-resource-coords
          ]
          ;;remove from resource memory
          set resource-coords remove-item position target-resource-coords resource-coords resource-coords
          ;;remove the target
          set target-resource-coords []
          ;;burn an endowment
          if subtract-endowment turtle-id = true [
            die
            stop
          ]
      ]
    ];; end target set

  ];;end ask
end

to scan-resources [ turtle-id ]
  let x 0
  let y 0
  let z 0

  ;; Makes the Traditional agent walk in random directions while looking for Opportunity agents
  ;; It also gets the current location of the Traditional agent
  ask turtle turtle-id [
    turtle-random-walk turtle-id
    set x round(xcor)
    set y round(ycor)
  ]

  ;; scan search neighborhood
  ask patches at-points neighborhood[

    ;;enable this to view the search neighborhood
    ;;set pcolor [color] of myself
    let px pxcor
    let py pycor
    if any? resources-on patch px py [
      ask turtle turtle-id [
        ;;has the ent already seen it?
        if member? (list px py) resource-coords = false [
          ;;has the ent already failed at it?
          if member? (list px py) failed-resource-coords = false [
            ;;add to resource memory
            set resource-coords lput (list px py) resource-coords
          ];;end
        ];;end if
      ];;
    ];;end resources on patch
  ];;end patches

end

;; populate environment with resources
to generate-entrepreneurs

  let sd 2

  set totalEndowments 0
  ;;loop to populate entrepreneurs
  repeat EntrepreneurCount [

    ;;placed variable
    let placed false
    ;;try to place the entrepreneur
    while[placed = false][

      ;; Generates random coordinates to place
      let x round(random-xcor)
      let y round(random-ycor)

      ;; is spot empty?
      if not any? turtles-on patch x y [
        create-entrepreneurs 1[

          set isOutlier 0
          set resource-coords []
          set target-resource-coords []
          set failed-resource-coords []

          set shape "person"
          set color blue
          set xcor x
          set ycor y
          set neighborhood moore-offsets 1 false
          ifelse PowerLaw = true[;;power law endowment distribution
            set endowments random-pareto .5 startingEndowments (startingEndowments * 10)
          ][;;not power law
            set endowments random-normal StartingEndowments sd
          ]
          set totalEndowments totalEndowments + totalEndowments
          ;;change color of outlier
          if endowments > (StartingEndowments + (sd * 2)) [
           set color red
           set isOutlier 1
           set neighborhood moore-offsets 2 false
          ]

          ;; entrepreneur successfully placed, break out of the loop
          set placed true
        ];;end create
       ];;end spot check
      ;;if entrepreneur wasn't placed, the loop repeats until it finds a spot
      ];; end if
    ];;end repeat

end

;; populate environment with resources
to generate-resources [ initialRun ]
  ;;loop
  let resourceValue 5
  let sdResourceValue 2
  let resCount ResourceCount

  if initialRun = false[
    ;;repopulate with somewhere around 1/2 the resources
   set resCount (random-normal resCount resCount * 0.20) * 0.50
  ]
  repeat ResourceCount [

    ;; placed variable
    let placed false
    ;;try to place the resource
    while [placed = false][

      ;; Generates random coordinates to place
      let x round(random-xcor)
      let y round(random-ycor)

      ;; is spot empty?
      if not any? turtles-on patch x y [
        ;;create resource
        create-resources 1 [
          set xcor x
          set ycor y
          set shape "circle"
          set color green
          set value random-normal resourceValue sdResourceValue ;; value between 1 and 100
        ]
        ;;resource successfully placed, exit loop
        set placed true
      ];; end spot check
      ;;if resource wasn't placed, the loop repeats until it finds a spot
    ];;end while
  ];;end repeat

end

to-report random-pareto [ pareto low high ]
   ;; report a pareto distributed value between low and high,
   ;; where the distribution has a curve of alpha (0 < k )
   let u random-float 1
   let ha high ^ alpha
   let la low ^ alpha
   report ((-1 * ( ( u * ha - u * la - ha  ) / ( ha * la ) ) ) ^ ( -1 / alpha ) + 2 )
   ;; see: http://en.wikipedia.org/wiki/Power_law
   ;; see: http://en.wikipedia.org/wiki/Pareto_distribution
end

;;moore search
to-report moore-offsets [n tesOff]
  let result [list pxcor pycor] of patches with [abs pxcor <= n and abs pycor <= n]
  if tesOff = true [
    set result remove [-2 1] result
    set result remove [-2 -1] result
    set result remove [2 1] result
    set result remove [2 -1] result
  ]
  report remove [0 0] result
end

to-report subtract-endowment [ turtle-id ]
  let is-dead false
  ask turtle turtle-id [
    set endowments endowments - 1
    set movements movements + 1
    if endowments <= 0 [
      set is-dead true
    ]
  ]
  report is-dead
end

;; This procedure makes the Traditional and ADHD agents walk in random directions
to turtle-random-walk [ turtle-id ]
  ask turtle turtle-id [
    left random 360
    right random 360
     forward 1
  ]
end

;; This procedure makes the Traditional and ADHD agents walk towards the given x and y coordinates
to turtle-walk [ turtle-id x y ]
  ask turtle turtle-id [
    facexy x y
    forward 1
  ]
end


to-report endowmentsOutliers
  let endowmentsOutlier sum [endowments] of entrepreneurs with [isOutlier = 1]
  report endowmentsOutlier
end

to-report endowmentsNormals
  let endowmentsNormal sum [endowments] of entrepreneurs with [isOutlier = 0]
  report endowmentsNormal
end
@#$#@#$#@
GRAPHICS-WINDOW
732
10
1278
557
-1
-1
8.82
1
10
1
1
1
0
1
1
1
-30
30
-30
30
1
1
1
ticks
30.0

BUTTON
7
12
70
45
setup
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

SWITCH
8
62
106
95
PowerLaw
PowerLaw
1
1
-1000

INPUTBOX
8
121
106
181
ResourceCount
100.0
1
0
Number

INPUTBOX
110
120
215
180
EntrepreneurCount
300.0
1
0
Number

BUTTON
80
12
146
45
go
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

INPUTBOX
210
47
326
107
StartingEndowments
10.0
1
0
Number

SLIDER
109
62
204
95
alpha
alpha
0
3
1.0
0.1
1
NIL
HORIZONTAL

PLOT
7
198
282
385
Survival Plot
time
Count
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"ENT Outliers" 1.0 0 -2674135 true "" "plot count entrepreneurs with [isOutlier = 1]"
"Resources" 1.0 0 -13840069 true "" "plot count resources"
"ENTs" 1.0 0 -13791810 true "" "plot count entrepreneurs with [isOutlier = 0]"

INPUTBOX
335
10
403
70
tick-count
2000.0
1
0
Number

SLIDER
155
12
327
45
captureProbability
captureProbability
0
100
31.0
1
1
NIL
HORIZONTAL

PLOT
12
406
281
604
Endowment Spread
NIL
NIL
0.0
100.0
0.0
20.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ endowments ] of entrepreneurs"

PLOT
471
10
671
160
Outlier Count
NIL
NIL
0.0
10.0
0.0
110.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [isOutlier] of entrepreneurs "

SWITCH
218
121
397
154
RegenerateResources
RegenerateResources
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
NetLogo 6.0.1
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
