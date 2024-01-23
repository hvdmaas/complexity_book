patches-own [
  attention
  information
  opinion
  threshold ; used to select agents in each interation
]

globals
[
  decay_A  ; decay in attention
  actives  ; number of agents selected in each interation
]

to setup
  clear-all
  ask patches [
   set attention random-normal mean-init-attention sd-init-attention
   set information  random-normal mean-init-information sd-init-information
   set opinion information
   set decay_A 1 ; start value decay attention
   set visualize "opinion"
  ]
  recolor visualize
  reset-ticks
end

to go
  ask patches [set threshold random-float 1]  ; random threshold per agent
  set actives count patches with [attention > threshold] ; count actives
  ask patches with [attention > threshold]  ; agent seclection depeding on attention (eq. 5)
  [
    let neigh one-of neighbors ; select a random neighbor
    let I information   ; collect variables of agent and its neighbor
    let A_n [attention] of neigh
    let I_n [information] of neigh
    let O_n [opinion] of neigh
    if abs(opinion - O_n) < bound     ; bounded confidence
    [
      set attention min list 1 (attention + da)   ; increase attnetion due to interaction (eq. 6a)
      ask neigh [set attention min list 1 (A_n + da)] ; also in neighbor
      set information new_information information I_n attention A_n ; udpate information (eq 7a)
      ask neigh [set information new_information I_n I A_n attention] ; also in neighbor
    ]
  ]
  update_decay_A  ; decay_A updated as function of difference in actives and %active_agents (intended number of actives)
  ask patches
   [
   set attention  max list 0.00 (decay_A * attention)  ; decay in attention  (eq 6b)
   set information decay_I * (information + random-normal m-noise-information sd-noise-information) ; decay in information + noise (eq 7b)
   set opinion opinion + cusp opinion attention information ; update opinion (eq 4)
  ]
  recolor visualize
  tick
end

to-report new_information [Ii Ij Ai Aj]  ; implemention eq 6a
  let r rmin  + (1 - rmin) / (1 + exp(-1 * p * (Ai - Aj)))
  report r * Ii + (1 - r) * Ij
end

to-report cusp [O A I]  ; implementation eq. 4
  report -1 * (O ^ 3 - (A - .5) * O - I) * dt + random-normal 0 sd-noise-opinion
end

to update_decay_A  ; decay_A update function
  let d_A (1 + (%active_agents / 100) - (actives / (world-width * world-height))) * decay_A
  set d_A min list 1 d_A
  set decay_A max list .5 d_A
end

to recolor [visual]  ; color patches depending on visual (default opinion)
  if visual  = "opinion" [
    ask patches with [opinion > 0] [set pcolor scale-color blue opinion 2 0]
    ask patches with [opinion <= 0] [set pcolor scale-color red opinion -2 0]]
  if visual  = "information" [
    ask patches with [information > 0] [set pcolor scale-color blue information 2 0]
    ask patches with [information <= 0] [set pcolor scale-color red information -2 0]]
  if visual  = "attention" [
     ask patches [set pcolor scale-color red attention 2 0]]


end

to add-activists    ; add activist on mouse click
   ask patch 0 0 [
    if mouse-down?
    [ ask patch-at mouse-xcor mouse-ycor
      [ set attention 1  ; high attention
        set information -.5 ; negative information
        set opinion -.5  ; negative opinion
        set opinion opinion + cusp opinion attention information
      ]
    ]
    display
  ]
end

to perturbate-activists  ; perturbate activists opinion in opposite direction
  let activists count patches with [opinion < 0]
  ask n-of (activists / 2) patches with [opinion < 0]
  [
    set opinion -1 * opinion
    set attention 1  ; set attention high
  ]
end

to setup-default  ; paramter settings Black Pete scanario (see HIOM paper)
 set %active_agents 80
 set mean-init-information 0
 set sd-init-information 0.2
 set mean-init-attention 0.5
 set sd-init-attention 0
 set da .02
 set rmin 0
 set p 3
 set decay_I 1
 set m-noise-information 0
 set sd-noise-information 0.0
 set sd-noise-opinion .001
 set dt .01
 set bound 10
 setup
end


to setup-black-pete  ; paramter settings Black Pete scanario (see HIOM paper)
 set %active_agents 50
 set mean-init-information .1
 set sd-init-information 0
 set mean-init-attention 0
 set sd-init-attention 0
 set da .02
 set rmin 0
 set p 4
 set decay_I 1
 set m-noise-information 0
 set sd-noise-information 0.0
 set sd-noise-opinion .001
 set dt .01
 set bound 10
 setup
end


to setup-meat-eating-vegaterian ; paramter settings meat eating vegetarian scanario (see HIOM paper)
 set %active_agents 20
 set mean-init-information .1
 set sd-init-information 0
 set mean-init-attention 0
 set sd-init-attention 0
 set da .01
 set rmin 0
 set p 2
 set decay_I 1
 set m-noise-information 0
 set sd-noise-information 0
 set sd-noise-opinion .0001
 set dt .01
 set bound .2
 setup
 ask n-of (count patches / 5) patches ;with [abs(pycor) < 10]
   [set information -.5
    set opinion -.5
  ]
 recolor visualize
end
@#$#@#$#@
GRAPHICS-WINDOW
192
16
667
492
-1
-1
9.16
1
10
1
1
1
0
1
1
1
-25
25
-25
25
0
0
1
ticks
30.0

BUTTON
395
505
489
566
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
20
13
96
46
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

SLIDER
15
76
190
109
%active_agents
%active_agents
0
100
50.0
1
1
NIL
HORIZONTAL

PLOT
678
17
985
149
Opinion
NIL
NIL
-2.0
2.0
0.0
10.0
true
false
"" ""
PENS
"opinion" 0.1 1 -16777216 true "" "histogram [opinion] of patches"

PLOT
678
162
985
289
Attention
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"attention" 0.02 1 -16777216 true "" "histogram [attention] of patches"

PLOT
676
299
986
439
Information
NIL
NIL
-1.0
1.0
0.0
10.0
true
false
"" ""
PENS
"Information" 0.02 1 -16777216 true "" "histogram [information] of patches"

SLIDER
15
112
187
145
da
da
0
.4
0.02
.001
1
NIL
HORIZONTAL

SLIDER
13
177
185
210
rmin
rmin
0
1
0.0
.001
1
NIL
HORIZONTAL

SLIDER
13
213
185
246
p
p
0
5
5.0
.1
1
NIL
HORIZONTAL

SLIDER
12
250
184
283
decay_I
decay_I
0
1
1.0
.001
1
NIL
HORIZONTAL

BUTTON
14
480
126
513
NIL
add-activists
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
110
12
183
46
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
682
447
784
492
%active agents
precision (100 * actives / (world-width * world-height)) 1
17
1
11

SLIDER
12
319
184
352
sd-noise-information
sd-noise-information
0
.2
0.0
.001
1
NIL
HORIZONTAL

CHOOSER
798
446
936
491
visualize
visualize
"opinion" "attention" "information"
0

TEXTBOX
19
160
169
178
Information parameters
11
0.0
1

TEXTBOX
22
57
172
75
Attention parameters
11
0.0
1

TEXTBOX
16
365
166
383
Opinion parameters
11
0.0
1

SLIDER
11
383
183
416
sd-noise-opinion
sd-noise-opinion
0
.1
0.001
.001
1
NIL
HORIZONTAL

SLIDER
10
421
182
454
dt
dt
0
.1
0.01
.001
1
NIL
HORIZONTAL

SLIDER
195
500
388
533
mean-init-information
mean-init-information
-1
1
0.1
.01
1
NIL
HORIZONTAL

SLIDER
195
538
387
571
sd-init-information
sd-init-information
0
.5
0.0
.001
1
NIL
HORIZONTAL

SLIDER
496
499
671
532
mean-init-attention
mean-init-attention
0
1
0.0
.01
1
NIL
HORIZONTAL

SLIDER
498
537
670
570
sd-init-attention
sd-init-attention
0
.2
0.0
.001
1
NIL
HORIZONTAL

TEXTBOX
129
483
214
511
Click in\nworld
11
0.0
1

BUTTON
12
551
167
584
NIL
perturbate-activists
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
12
585
167
618
bound
bound
0
5
10.0
.01
1
NIL
HORIZONTAL

PLOT
678
497
983
617
means
NIL
NIL
0.0
10.0
-0.5
0.5
true
true
"" ""
PENS
"O" 1.0 0 -16777216 true "" "plot mean [opinion] of patches"
"A" 1.0 0 -1604481 true "" "plot mean [attention] of patches"
"I" 1.0 0 -6759204 true "" "plot mean [information] of patches"

TEXTBOX
18
463
168
481
Black Pete simulation
11
0.0
1

TEXTBOX
14
524
164
552
Meat eating vegetarion\nsimulation
11
0.0
1

BUTTON
332
581
471
614
NIL
setup-black-pete
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
489
580
669
613
NIL
setup-meat-eating-vegaterian
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
12
285
185
318
m-noise-information
m-noise-information
-.01
.01
0.0
0.001
1
NIL
HORIZONTAL

BUTTON
195
582
311
615
NIL
setup-default
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

This is an implementation of the Hierarchical Ising Opinion Model (HIOM).

van der Maas, H. L. J., Dalege, J., & Waldorp, L. J. (2020). The polarization within and across individuals: The hierarchical Ising opinion model. Journal of Complex Networks, 8, cnaa010.

The opinion of agents is modeled by a stochastic (cusp) differential function in which attention and information are the control variables of the cusp. Agents interact and influence both attention and information. Due to hysteresis within agents, this leads to new explanations of polarization.

The cusp model of the individual is derived from the Ising attitude model, described in

Dalege, J., & van der Maas, H. L. J. (2020). Accurate by being noisy: A formal network model of implicit measures of attitudes. Social Cognition, 38, S26-S41.

Dalege, J., Borsboom, D., van Harreveld, F., & van der Maas, H. L. J. (2019). A network perspective on attitude strength: Testing the connectivity hypothesis. Social Psychological and Personality Science, 10, 746-756.

Dalege, J., Borsboom, D., van Harreveld, F., & van der Maas, H. L. J. (2018). The Attitudinal Entropy (AE) Framework as a General Theory of Individual Attitudes. Psychological Inquiry, 29, 175-193.

Dalege, J., Borsboom, D., van Harreveld, F., van den Berg, H., Conner, M., & van der Maas, H. L. J. (2016). Toward a formalized account of attitudes: The Causal Attitude Network (CAN) model. Psychological Review, 123, 2-22.


## HOW IT WORKS

The algorithm has been slightly changed 
and optimized compared to the original 2020 paper:

Iterate

  * Randomly choose a set of agents, weighted with attention A (modified eq. 5).
  * Iterate over these agents.
    * Randomly choose a neighbour of the agent i as partner j in the interaction.
    * Add attention to both agents i and j (modified eq.6a).
    * Exchange information (modified eq.7).
  * Apply decay in A to all agents (modified eq. 6b).
  * Add noise and apply decay in I to all agents (modified eq. 7b).
  * Update opinion O in all agents (eq. 4).

(4)	dO<sub>i</sub> = -(O<sub>i</sub>^3-(A<sub>i</sub>+A_min)O<sub>i</sub>-I<sub>i</sub> )dt+s<sub>O</sub> dW<sub>i</sub> (t)
(5)	Pr(select agent<sub>i</sub> )=A<sub>i</sub>
(6a)	A<sub>i</sub>= min⁡(1,A<sub>i</sub>+ d<sub>A</sub>)
(6b)	A<sub>i</sub>= decay<sub>A</sub>  A<sub>i</sub>
(7a)	I<sub>i</sub>=r I<sub>i</sub>+(1-r) I<sub>j</sub>, where r=rmin+(1-rmin)/(1+e^(-p(A<sub>i</sub>-A<sub>j</sub>)) )
(7b)	I<sub>i</sub>=decay<sub>I</sub>  (I<sub>i</sub>+Ν(m<sub>I</sub>,s<sub>I</sub>))

Where dt  = .01, Amin = -.5

O is opinion, A is attention, I is information

and parameters (set by sliders):
m<sub>I</sub>     = mean noise information (external field)
s<sub>I</sub>     = sd noise information
dA     = attention increase by interactions
p      = persuasion
rmin   = minimal resistance
decay<sub>I</sub> = decay information
s<sub>O</sub>     = sd noise opinion



## HOW TO USE IT

SETUP sets the agents to values determined by mean-init-information, sd-init-information, mean-init-attention, sd-init-attention. 
GO starts the simulation.

The sliders set parameters of the main equations for attention, information, and opinion (see HOW IT WORKS).
The %active-agents slider influences how many agents will be active in each interaction. %active-agents affects the decay of attention (decay_I). 

Visualize can be set to opinion, attention, information.

The histograms show the distribution of opinion, attention and information.
The mean plot shows the change of the mean of opinion, attention of information.

setup-black-pete is used to set the parameters to the value required for the Black-Pete simulation described in the HIOM 2021 paper. In this scenario, activists are added to a conservative community with low attention. If attention grows too quickly, polarization will result. Click on add-activist to allow activists to be added with mouse clicks.

setup-meat-eating-vegetarian is used to set the parameters to the value required for the meat-eating vegetarian simulation described in the HIOM 2021 paper. In this simulation, the bound is set low, meaning that agents that are too different will not interact. 
By clicking Perturb-Activists, some activists are perturbed to the meat-eating opinion, but their information position does not change. They can now influence the meat-eaters, which leads to a slow change to the vegaterian position.

## THINGS TO NOTICE

Polarization due to hysteresis (simulation 1 in the HIOM paper):

In the standard setup, you can see that high attention (set %active-agent high) leads to polarization. If you now decrease the difference in information (set decay_I to .5), the polarization remains even if the difference in underlying information is 0. Only if you also decrease attention (set %active-agents low), the polarization disappears. 

Opposition to activism: the case of Black Pete (simulation 2 in the HIOM paper):
See HOW TO USE IT

A solution to polarization: the meat-eating vegetarian (simulation 3 in the HIOM paper):
See HOW TO USE IT


## THINGS TO TRY

The mean noise information function as an external field. Test if varying this paramter gives hystersis at the level of the mean opionion over agents.

Test the robustness of the 3 simulations by adjusting model parameters with the sliders.

In the Black Pete simulation activist are more succesful when attention changes slowly. Adjust da and %active-agents to test this.

In the meat-eating vegetarian simulation, the pertubation only works when attention is relatively low. Test this by setting  %active-agents high.

## EXTENDING THE MODEL

Set-up the model in a preferential atttachment network

## CREDITS AND REFERENCES

van der Maas, H. L. J., Dalege, J., & Waldorp, L. J. (2020). The polarization within and across individuals: The hierarchical Ising opinion model. Journal of Complex Networks, 8, cnaa010.
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
NetLogo 6.3.0
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
