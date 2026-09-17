extensions [rnd csv]

; Variables

globals[
  ; political identity agentsets
  liberals ;; liberal turtles subset
  conservatives ;; conservative turtles subset

  ; average similarity of turtles with their neighbors based on colour (political identity)
  percent-similar              ;; all turtles
  liberal-percent-similar      ;; liberal turtles
  conservative-percent-similar ;; conservative turtles
  ; the average percantage of a turtle's neigbours that are perceived as ingroup (using the identify-friends function's friend count)
  perceived-percent-similar              ;; all turtles
  liberal-perceived-percent-similar      ;; liberal turtles
  conservative-perceived-percent-similar ;; conservative
  prop-homophily
  dissimilarity-tolerance-conservative

  ; choice-related
  mf-choice

  ; for data-collection
  sim-id
  mf-rev
]

turtles-own [  ; turtle-specific variables
  ; political identity
  identity-bin ;; discrete identity based on identity (conservative/liberal)

  ; decision weights for MFs
  w-care       ;; care (1)
  w-fair       ;; fairness (2)
  w-ingroup    ;; ingroup loyalty (3)
  w-authority  ;; authority (4)
  w-pure       ;; purity (5)

  ; neighbor relations
  similar-nearby      ;; how many neighboring patches have a turtle with my political orientation?
  other-nearby        ;; how many have a turtle of another political orientation?
  total-nearby        ;; sum of previous two variables
  total-all-nearby    ;; number of possible neighbours
  friend-count        ;; number of neighbouring turtles considered ingroup from matching past-choices
  my-friends          ;; agentset containing turtles considered friends on a given round
  not-friend-count    ;; number of neighbouring turtles considered outgroup from matching past-choices
  not-my-friends      ;; agentset containing turtles considered outgroup on a given round
  past-friends        ;; agentset with my-friends from last round
  still-my-friends    ;; agent set of neighbours that have been ingroup for more than one round in a row
  prop-similar-nearby ;; proportion of neighbouring turtles that are perceived as ingroup ( friend-count / 8 )
  friend-string       ;; string of who are considered friends

  ; choice variables
  my-choice         ;; MFs to choose between
  choice-weights    ;; the corresponding decision weights for the MFs to choose between (decision weights corresponding to the choice are drawn and stored here)
  conflicted        ;; (boolean) whether a turtle experieces conflict or not based on the difference between choice-weights
  action-taken      ;; how a choice is dealt with (choosing - cho - picking at random - ran - disengagement - dis - or copying the ingroup - co)
  decision          ;; holds the turtles decision until the end of the round for past-choices to updated simultaneously
  ;choice-log        ;; saves an agent's decision behaviour in each round for sanity checks, i.e., choice, e.g., mf 1 mf 2, decision; "NC" = no decision was made on a given round; "d" = decision was disengaged from
  choice-string     ;; a turtles past-choices as a string with no spaces (for leventstein distances)
  past-choices      ;; list of most recent choice outcomes, currently, 5 items
  unhappy           ;; (boolean) whether the turtle is happy (false) or unhappy (true)
  past-conflict     ;; how many rounds has the turtle been conflicted in a row
  ingroup-accuracy

  test1
  test2
  test3
  test4
  test5
  test6
]

directed-link-breed [ ingroup-bonds ingroup-bond ]
undirected-link-breed [ cluster-bonds cluster-bond ]

ingroup-bonds-own [
  ticks-distant ;; if the child is distant from the parent, how many ticks since it moved away
]
links-own [
  age           ;; for how mnny ticks has the link been established

]

; for data collection to call clearing up separately from setup to make sure settings in external documents are not erased
to tidy
  clear-all
end

; main funciton 1 - setup
to setup
  ; clear previous round
  ifelse save-data
  [ set MF-revision ifelse-value mf-rev = 0 [ "sigmoid" ] [ "linear" ] ]
  [ clear-all ]

  ; create turtles on random patches
  resize-world ( - world-size ) world-size ( - world-size ) world-size

  set prop-homophily homophily / 8 * 100
  set dissimilarity-tolerance-conservative ( ifelse-value asymmetric-tolerance
      [ dissimilarity-tolerance - dt-offset ]
      [ dissimilarity-tolerance ] )

  ask patches [
      set pcolor 9.9

    ; generate turtles
    if random 100 < density [ ;; roughly a proportion of patches corresponding to the density will spawn a turtle
      sprout 1 [
        set size 1
        ; Setting up choice variables
        set past-choices []   ;; choice memory is an empty list to start
        ;set choice-log []     ;; choice log is an empty list to start
        set my-friends []

        ; randomly assign political identity
        set color one-of [ 105 14 ]
        set identity-bin ifelse-value ( color = 105 ) [ "liberal" ] [ "conservative" ]

         set color one-of [ 105 14 ]
        set identity-bin ifelse-value ( color = 105 ) [ "liberal" ] [ "conservative" ]
      ]
    ]
  ]

  setup-morals

  ; political identity agentsets
  set liberals turtles with [ color = 105 ]      ;; liberal turtles
  set conservatives turtles with [ color = 14 ] ;; conservative turtles

  reset-ticks ; resets ticks from last run

  if save-data [ collect "init" ]
end


; (level 1) sub functions called by setup
to setup-morals ; gives a starting set of weights to each mf depending on this identity
                ;; setting starting weights
  let cons-weights (extract-mfs-weights "conservative")
  let lib-weights (extract-mfs-weights "liberal")

  ask turtles [
    let my-weights [] ; placeholder

    ifelse color = 105 [
      set my-weights item 0 lib-weights
      set lib-weights remove-item 0 lib-weights
    ] [
      set my-weights item 0 cons-weights
      set cons-weights remove-item 0 cons-weights
    ]

    set w-care item 0 my-weights
    set w-fair item 1 my-weights
    set w-ingroup item 2 my-weights
    set w-authority item 3 my-weights
    set w-pure item 4 my-weights

    let weights softmax ( list  w-care w-fair w-ingroup w-authority w-pure )
    set past-choices n-values 5 [ weighted-prob-draw weights [ 1 2 3 4 5 ] ]
  ]
end

; main function 2 - procedures for each round
to go
  update-turtles         ; updates initial turtle properties
  identify-friends       ; identifies the neighbours considered ingroup
  update-ingroup-bonds
  make-turtle-choices    ; generates a choice for a give proportion of turtles
  determine-unhappiness  ; determines whether a turtle is unhappy or not
  update-past-choices    ; updates the past-choices variable with the current round's decision
  update-cluster-bonds
  ifelse  save-data [ collect "" ] [ update-plot-info ] ;; save data or update summary values for GUI plots
  update-links
  if save-view [ picture ]
  if recording [ record ]
  if movement [ move-turtles ]           ; moves turtles at the end of the iteration
  tick
end


; (level 1) sub functions called by go
to update-turtles                          ; updates initial turtle variables
  ask turtles [
    ; neighbouring turtle properties
    ;; based on colour similarity (actual political identity - only liberal (any red) vs. consertative (any blue))
    set similar-nearby count (turtles-on neighbors) with
    [ color >= ( [ color ] of myself - 10 ) and color <= ( [ color] of myself + 10 ) ] ;; counts number of neighbouring turtles with the same (binary) political identity
    set other-nearby count (turtles-on neighbors) with
    [ color < ( [ color ] of myself - 10 ) or color > ( [ color] of myself + 10 ) ]    ;; counts number of neighbouring turtles with other (binary) political identity
    set total-nearby similar-nearby + other-nearby                                     ;; number of neighbouring turtles
    set total-all-nearby count ( neighbors)                                            ;; counts neighbouring patches - always 8

    set choice-string ifelse-value ( length past-choices != 0 )
    [ string-from-list ( sort past-choices ) ( "" ) ]
    [ "" ]

    ; resets choice variables
    set conflicted false
    set my-choice [ 0 0 ]
    set choice-weights [ 0 0 ]
    set action-taken "NC"
    set decision 0
  ]
end

to update-ingroup-bonds
  ask ingroup-bonds [
    let past-state ticks-distant
    let distant ifelse-value ( member? end2 (turtles-on [ neighbors ] of end1) )
    [ "close" ]
    [ "far" ]

    set ticks-distant (
      ifelse-value
      ( ( past-state > 0 ) and ( distant = "far" ) ) [ ticks-distant + 1 ]
      ( distant = "far" ) [ 1 ]
      [ 0 ]
    )
  ]
end

to update-links
  ask links [
    set age age + 1
  ]

  ask ingroup-bonds [
    let past-state ticks-distant
    let distant ifelse-value ( member? end2 (turtles-on [ neighbors ] of end1) )
    [ "close" ]
    [ "far" ]

    set ticks-distant (
      ifelse-value
      ( ( past-state > 0 ) and ( distant = "far" ) ) [ ticks-distant + 1 ]
      ( distant = "far" ) [ 1 ]
      [ 0 ]
    )
  ]

end

to identify-friends                        ; lets turtle perform social inference about the political identity of their neighbouring turtles by comparing their past-choices
  ask turtles [

    ; save the set from last round
    set past-friends my-friends

    ; reset counts from last round
    let friend-choices []   ;; placeholder for identified ingroup members
    let not-friend-choices []   ;; placeholder for identified outgroup members

    ; leventstein distance between sorted past-choices of the turtle and each ot their neighbours
    foreach [ choice-string ] of ( turtles-on neighbors ) ;; goes through each neighbour (with non-empty past-choices)
      [ neighbor-choices ->
        let ratio lev-ratio choice-string neighbor-choices     ;; for each it computes the leventstein ratio (sum(string lengths) - ld ) / sum(string lengths)
        ; compare ratio to tolerance and account for whether they qualify as an ingroup member or not
        if ratio * 100 >= 100 - ( ifelse-value ( asymmetric-tolerance and color = 14 ) [ dissimilarity-tolerance-conservative ][ dissimilarity-tolerance ] )
          [ set friend-choices fput neighbor-choices friend-choices ]   ;; if they are considered outgroup, add their choices to a list to identify them
        if ratio * 100 <= ( ifelse-value ( asymmetric-tolerance and color = 14 ) [ dissimilarity-tolerance-conservative ][ dissimilarity-tolerance ] )
          [ set not-friend-choices fput neighbor-choices not-friend-choices ]
    ]

    ; finds turtles with ingroup past-choices and saves the agentset
    set my-friends ( turtles-on neighbors ) with [ member? choice-string friend-choices ]
    ; counts number of turtles with matching strings
    set friend-count length [ who ] of my-friends

    set ingroup-accuracy 0
    let act-ingroup ((turtles-on neighbors) with [ color = [color] of myself])
    foreach [ who ] of act-ingroup  [act -> if member? act [ who ] of my-friends [set ingroup-accuracy ingroup-accuracy + 1] ]

    ; agentset of neighbours whi have been perceived as ingroup for more than 1 round in a row
    carefully
    [ set still-my-friends turtle-set remove " " map [ x -> ifelse-value ( member? x [ who ] of past-friends ) [ turtle x ] [ " " ] ] [ who ] of my-friends ]
    [ set still-my-friends [] ]
    set friend-string string-from-list [ who ] of my-friends ", "

    ; compute the proportion of neigjbouring turtles that are perceived as friends
    carefully
    [ set prop-similar-nearby friend-count / total-all-nearby ]
    [ set prop-similar-nearby 0 ]

    ; once friends have been identified, the most certain ones are used to update turtles decision weights
    carefully
    [ social-influence "+" still-my-friends ]
    []

    if negative-influence [
      ; finds turtles with outgroup past-choices and saves the agentset
      set not-my-friends ( turtles-on neighbors ) with [ member? choice-string not-friend-choices ]
      ; counts number of turtles with matching strings
      set not-friend-count length [ who ] of not-my-friends
      ; once outgroup have been identified, the most certain ones are used to update turtles decision weights
      carefully
      [ social-influence "-" not-my-friends ]
      []
    ]
    ; create links to close friends
    carefully
    [ create-ingroup-bonds-to still-my-friends who-are-not my-out-ingroup-bonds ]
    [ ]

    ; remove old linkts
    if count my-out-ingroup-bonds > 8 [
      let keep min-n-of 8 my-out-ingroup-bonds [ ticks-distant ]
      ask my-out-ingroup-bonds who-are-not keep [ die ]
    ]
  ]
end


to make-turtle-choices                     ; gives a specified proportion of turtles a choice between two mfs, determines conflict from wegiths, and provides options to conflicted (disengage, copy, make marginal choice) and non-conflicted turtles (make choice from weights)
  ; should a choice take place on this round?
  if choice-prevalence > random 100 [ ;; depends on interface choice prevalence

    ; what is the choice of the round
    let choices ( list [1 2] [1 3] [1 4] [1 5] [2 3] [2 4] [2 5] [3 4] [3 5] [4 5] ) ;; all possibles ordered pairs of mfs (1 = care/harm, 2 = fairness, 3 = ingorup loyalty, 4 = authority, 5 = purity). Note, see choices.R for documentation on generating the list
    set mf-choice one-of choices ;; choose one at randome

    ; which turtles are choosing this round
    ask n-of (choice-proportion / 100 * count turtles) turtles [ ;; randomly pick a proportion of turtles equivalent to the choice proportion to make a choice between two mfs

      ; what are they choosing between
      set my-choice ifelse-value ( choice-invariance > random 100 )
      [ mf-choice ]
      [ one-of choices ]

      ; (5) convert the mfs to their corresponding decision weights
      set choice-weights map [ mf -> get-weight mf ] my-choice

      if item 1 choice-weights = 0 and item 1 choice-weights = 0 [
        set choice-weights replace-item 0  choice-weights 0.01
        set choice-weights replace-item 1 choice-weights 0.01
      ]

      ; (6) is the turtle conflicted by the choice and how should it proceed
      ;; conflict criteria: the difference in weights must be within the conflict range
      ifelse
      ( abs ( item 0 choice-weights - item 1 choice-weights) <= conflict-range ) ;; checking conflict criteria 2

      ;; if fulfilling both criteria, the agent is conflicted - it proceeds to deal with the experience of conflict (copying, disengagement or picking at random)
      [ set conflicted true
        if past-conflict < past-choices-length  ;;; a turtle only remembers its x most recent choices
        [ set past-conflict past-conflict + 1 ]  ;;; adds 1 to number of conflicts experienced in a row
        deal-with-conflict ]

      ;; otherwise, if not conflicted, the turtle proceeds to make a choice based on the size of its weights
      [ set conflicted false ;;; not conflicted
        set past-conflict 0  ;;; resets number of times conflicted in a row

        ;;; choose from weights
        ifelse item 0 choice-weights > item 1 choice-weights
        [ update-choices item 0 my-choice "cho" ] ;;;; if the first weight is larger it wins
        [ update-choices item 1 my-choice "cho" ] ;;;; otherwise, the other wins (they must be different to not cause conflict, i.e., weights are never equivalent)
      ]
    ]
  ]

  ; turtles with no choice this round
  ;ask turtles with [ my-choice = [ 0 0 ] ]
  ;[ set choice-log lput "NC" choice-log ] ; make records of this in their log
end

to determine-unhappiness                   ; probabilistically determines the happiness of turtles and deals with (un)happiness
  ask turtles [

    let unhappiness-prob 0 ; place holder

    ; component 1 (of 4) - does the turtle feel like it has enough friends
    if prop-similar-nearby * 100 < prop-homophily   ;; if the proportion of neighbours perceived as friends is below the homophily threshold (degree of homophily)
      [ set unhappiness-prob 1 ]   ;; the turtle must be unhappy ( a turtle cannot be happy if it feels alone )

    ; components 2 - 4 (of 4) - conflict elements
    if conflicted and unhappiness-prob != 1 [  ;; only run for conflicted agents who have enough friends (those without enough friends are already unhappy)

      ;; component 2 - relevance
      let mf-relevance ( sum choice-weights / 10 ) * 0.25   ;;; Average weight of the two relevant decision weights - the more relevant the conflict is (=the larger the decision weights involved),
                                                            ;;; the more likely the turtle is to be unhappy

      ;; compontent 3 - how is conflict dealth with
      let action (ifelse-value
        action-taken = "co" [ 0 ]       ;;; if copying no decrement is incured outside component 2 (the turtle used its friends to deal with conflict and should not have a higher chance of disserting the group)
        action-taken = "ran" [ 0.5 ]    ;;; if picking at random 1 / 8 (0.5 * 0.25) is added
        action-taken = "dis" [ 1 ]      ;;; if disengaging entirely 1 / 4 (1 * 0.25) is added to the unahppiness probability
      ) * 0.25

      ;; component 4 - how persistent is conflict
      let recur ( past-conflict / past-choices-length ) ;;; the proportion of past-choices that has led to conflict

      ; Determine the probability of unhappiness as:
      set unhappiness-prob ( action + mf-relevance ) * ( 1 + recur )
      ;; the sum of component 2 and 3 ( max 0.5 - when disengaging from a choice with both weights = 5 )
      ;; proportioned by past-conflict ( max 2 - when all five past choices induced conflict; max unhappiness probability = 1 (0.5 * 2)
    ]

    ; use the probability to determine turtle's unhappiness
    ifelse random 100 < unhappiness-prob * 100
      ;; if unhappy
      [ set unhappy true
      set shape "X" ]
    ;; otherwise, if happy
    [ set unhappy false
      set shape "square" ]
  ]
end

to update-cluster-bonds
  ask turtles [
    ifelse unhappy = false [
      let in-cluster-turtles (turtles-on neighbors) with [unhappy = false]

      create-cluster-bonds-with in-cluster-turtles who-are-not my-cluster-bonds
      ; create list of links to delete
      let del my-cluster-bonds with [not member? other-end in-cluster-turtles]
      ask del [ die ]
    ] [
      ask my-cluster-bonds [ die ]
    ]
  ]
end

to update-plot-info                        ; updates variables used in the interface plots
  ; similarity based on color (objective)

  ;; all turtles
  let similar-neighbors sum [ similar-nearby ] of turtles ;;; total neighbours count with the same colour
  let total-neighbors sum [ total-all-nearby ] of turtles ;;; total neighbour count

  ;;; calculate the average colour similarity
  carefully
  [ set percent-similar ( similar-neighbors / total-neighbors ) * 100 ]
  [ set percent-similar 0 ]  ;;;; in case any of the numbers are 0 and produces an error

  ;; liberal turtles
  ;;; count similar neighbours and all neighbours of liberal turtles
  let liberal-similar-neighbors sum [ similar-nearby ] of liberals
  let liberal-total-neighbors sum [ total-all-nearby ] of liberals

  ;;; calculate the average colour similarity
  carefully
  [ set liberal-percent-similar ( liberal-similar-neighbors / liberal-total-neighbors ) * 100 ]
  [ set liberal-percent-similar 0 ]

  ;; conservative turtles
  ;;; count similar neighbours and all neighbours of liberal turtles
  let conservative-similar-neighbors sum [ similar-nearby ] of conservatives
  let conservative-total-neighbors sum [ total-all-nearby ] of conservatives
  carefully
  [ set conservative-percent-similar ( conservative-similar-neighbors / conservative-total-neighbors ) * 100 ]
  [ set conservative-percent-similar 0 ]


  ; perceived similarity based on social inference (subjective)

  ;; all turtles
  let perceived-similar-neighbors sum [ friend-count ] of turtles  ;;; sum of turtles' perceived ingroup neighbours

  ;;; calculate the average perceived similarity
  carefully
  [ set perceived-percent-similar ( perceived-similar-neighbors / total-neighbors ) * 100 ]
  [ set perceived-percent-similar 0 ]

  ;; liberal turtles
  let liberal-perceived-similar-neighbors sum [ friend-count ] of liberals

  ;;; calculate the average perceived similarity
  carefully
  [ set liberal-perceived-percent-similar ( liberal-perceived-similar-neighbors / liberal-total-neighbors ) * 100 ]
  [ set liberal-perceived-percent-similar 0 ]

  ;; conservative turtles
  let conservative-perceived-similar-neighbors sum [ friend-count ] of conservatives

  ;;; calculate the average perceived similarity
  carefully
  [ set conservative-perceived-percent-similar ( conservative-perceived-similar-neighbors / conservative-total-neighbors ) * 100 ]
  [ set conservative-perceived-percent-similar 0 ]
end

to update-past-choices                     ; updates variables that cannot be updated during the round because turtles make decisions one by one
  ask turtles with [ decision != 0 ]   ; only turtles who have encountered a choice
  ; (1) update past-choices with decision made
  [ set past-choices lput decision ifelse-value ( length past-choices = past-choices-length )
    [ but-first past-choices ]  ;; if past-choices is full, remove the oldest (first) item
    [ past-choices ]            ;; otherwise, simply add it on

    ; (2) update weights from decision
    let updates []  ;; place holder

    ;; loop through each of the mfs in the choice and update the relevant weight
    foreach my-choice [  ;;; go through each choice mf
      mf -> let new-weight get-weight mf + ( ifelse-value
        ;;;; increment the weight if the mf was chosen and has been chosen at least once in the past 5 rounds
        mf = decision
        [ growth-revision get-weight mf ]
        ;;;; decrement the weight if the mf was not chosen (both for disengaging) and has not been chosen at least once in the past 5 rounds
        mf != decision and not member?  mf past-choices
        [ -1 * growth-revision get-weight mf ]
        ;;;; otherwise, no update is needed
        [ 0 ] )

      ;;; catch function if weights go slightly above their bounds
      if new-weight > 5 [ set new-weight 5 ]
      if new-weight < 0 [ set new-weight 0.01 ]

      ;;; new weights for the two mfs are collected here
      set updates lput new-weight updates ]

    ;; update weight variables with new weights
    ( foreach my-choice updates [  ;;; goes through the items of two lists in parallel, the choice mf(s) and the weight updates
      ;;; assign the weight based on the mf which triggered the update
      [ mf new-weight ] -> ( ifelse
        mf = 1 [ set w-care new-weight]
        mf = 2 [ set w-fair new-weight ]
        mf = 3 [ set w-ingroup  new-weight ]
        mf = 4 [ set w-authority new-weight ]
        [ set w-pure new-weight ]
      ) ]
    )
  ]
end

to move-turtles
  ask turtles [ if unhappy [

    find-new-spot

    move-to patch-here ; move to center of unoccupied patch
    ]
  ]
end


; (level 2) sub functions called by level 1 sub functions
to deal-with-conflict                      ; determines which action is taken to deal with conflict and makes turtles perform that action
  let choices lput "d" my-choice       ; a list of all avaliable options to copy - one of the two mfs or disengagement
  let prop-relevant 0                  ; placeholder - prop-relevant is determined in carefully

  ; when encountering conflict a turtle can 1) copy its ingroup (the most common of choices in their joint past-choices including disengagement),
  ;                                         2) disengage from the choice
  ;                                         3) pick at random between the mfs
  ; probabilities are determiend for each and a weighted draw is performed to find the appropriate way to deal with the experience of conflict

  ; Determining probabilities for each
  ;; 1) copying the ingroup
  let copy-prob 0

  let group-choices ifelse-value ( any? my-friends )   ;;; creates a list with all ingroup members' past-choice
    [ reduce sentence [ past-choices ] of my-friends ]
    [ [] ] ;;; unless the turtle have no percevied friends, then an empty list is made
    let comp map [opt -> occurrences opt group-choices] choices ;;; countes occurrences of each item on the choices list (the two mfs in conflict and disengagement) on friends' joint past-choices list

  ;;; the joint proportion of friends' past-choices relevant and friends of neighbours (more restrictive)
  carefully
    [ set prop-relevant ( sum comp / length group-choices )  ]
    [ set prop-relevant 0 ]

  set copy-prob precision ( prop-relevant * prop-similar-nearby ) 5  ; make this the probability of copying

  ;; 2) disengagement
  let past-dis 0
  let dis-prob 0

  ;;; the probability of disengagement depends on
  ;;; a) the size of the copy probability
  ;;; b) the size of the weights conflicting (larger weights -> larger probability of disengagement)
  ;;; c) how much an agent has disengaged in their past five choices

  ;;; calculate past propensity to disengage
  set past-dis occurrences "d" past-choices /  past-choices-length  ;;;; find the proportion of past-choices that are disengagement

  ;;; calculate the probability of disengagement
  ( ifelse
    past-dis > 0  and past-dis < 1 ;;;; if past disengagement is part of but not the only decision an agent has made in the past
    [ set dis-prob precision ( ( 1 - copy-prob ) * ( sum choice-weights / 10 ) * ( past-dis / ( sum choice-weights / 10 ) - past-dis + 1 ) ) 5 ]  ;;;; disengagement is the remaining probability space
                                                                                                                                                  ;;;; proportionated by the size of the weights and past propensity
                                                                                                                                                  ;;;; (the latter corresponding to taking away the proportion of disengagement from random picking and adding it to disengagement)
    past-dis = 1  ;;;; if the turtle has only disengaged in the past
    [ set dis-prob precision ( 1 - copy-prob ) 5 ] ;;;; disengagement takes up the remaining probability space (no probability of random picking)
    ;;;; otherwise, if past-dis = 0
    [ set dis-prob precision ( ( 1 - copy-prob ) * ( sum choice-weights / 10 ) ) 5 ] ) ;;;; the probability of disengagement is the proportion of the remaining probability space
                                                                                   ;;;; corresponding to the proportion of 5 (max weight) of the average weight of the confliciting mfs
  ;; 3) random picking
  let ran-prob precision ( 1 - copy-prob - dis-prob ) 5   ;;; the remainder of the probability space ( if the weights are small this is likely to be larger than disengagement)
                                                          ;;; note, if past-dis = 1, ran-prob will be 0, hence, no change

  ;; preforming a random draw with weighted probabilities of the three actions
  set action-taken weighted-prob-draw (list copy-prob dis-prob ran-prob) ["co" "dis" "ran"]

  ; Action taken
  (ifelse
    ;; if the agent want to copy
    action-taken = "co" [                                   ;;; The agent will copy their surrounding ingroup
      let probs softmax comp  ;;; softmax function to generate probabilities for each option (mf1, mf2, disengage)

      let copy-draw weighted-prob-draw probs choices        ;;; run weighted draw
      update-choices copy-draw action-taken]                ;;; update choices with the outcome
    ;; if they want to disengage
    action-taken = "dis" [update-choices "d" action-taken]  ;;; The agent will disengage from the choice
    ;; otherwise, they will want to pick at random
    [update-choices item (random 2) my-choice action-taken] ;;; The agent will randomly pick one of the conflicting mfs
  )
end

to social-influence [ dir agentset ]           ; lets a turtle update their choice weights based on how well their choices match the inputted agentset (e.g., their friends)
                                               ; only executed if the inputted agentset is not empty
  if any? agentset [

    let decisions [ 1 2 3 4 5 ]  ;; mfs that past-choices could contain
                                 ;; (note, disengagement is not counted, but if friends' past-choices "d" count > mine, the friends would have fewer mf decisions -> the mfs which have been disengaged from are discounted if this agent
                                 ;; did not already disengage from them themselves)

    ; count occurrences of each mf on the turtles own past choices
    let my-choices map [x -> occurrences x past-choices ] decisions

    ; count average occurrences of each mf on turtle's ingroup's past choices
    let friend-avg map [ decision-item -> occurrences decision-item ( reduce sentence [ past-choices ] of agentset ) / count agentset ] decisions  ;; computes the average count for its friends

    ; The difference between a turtle's past-choices and the average of its current friends is used to update weights
    let updates [] ;; place holder

    ( foreach friend-avg my-choices decisions [ ;; the three lists that will be used (the friend average count, a turtles own count and the 5 mf options)
      [ their-count my-count mf ] -> ;; names for each item on each list
                                     ;; the value to update the weights are given by logistic growth proportioned by the strength of social influence, and the difference in the turtle's and its friends proportion of each mf of past-choices
      let upd ifelse-value dir = "-"
      [ (-1 * growth-revision get-weight mf) * influence-strength * ( my-count - their-count ) / past-choices-length ]
      [ (growth-revision get-weight mf) * influence-strength * ( their-count - my-count ) / past-choices-length ]
      ;; update each weight with the value
      let new-weight get-weight mf + upd

      ;; catch function to ensure weights do not exceed their bounds
      if new-weight > 5 [ set new-weight 5 ]
      if new-weight <= 0 [ set new-weight 0.01 ] ;; the weights should not be 0 as they are used for division later on

      ;; adds the new weights to for each mf to a 5-item list
      set updates lput new-weight updates ]
    )

    ; the decision weights are updated (like update-past-choices, but for all 5 mfs)
    (foreach updates decisions [ [ new-weight mf ] ->
      ( ifelse
        mf = 1
        [ set w-care new-weight ]
        mf = 2
        [ set w-fair new-weight ]
        mf = 3
        [ set w-ingroup new-weight ]
        mf = 4
        [ set w-authority new-weight ]
        [ set w-pure new-weight ]
      ) ]
    )
  ]
end


to update-choices [ outcome method ]         ; short-cut to update variables when a turtle makes a decision
  set decision outcome  ; carries the outcome of a choice (to ensure all turtles makes their choice before past-choices are updated)
  ;set choice-log lput (list my-choice outcome method) choice-log  ; record choices in log

  set action-taken method  ; record how the decision was reached (cho = choice, ran = random picking, co = copying, dis = disengagement)
end


to find-new-spot                          ; move until a turtle finds an unoccupied spot
  let nearest-ingroup min-one-of my-out-ingroup-bonds with [ ticks-distant > 0 ] [ ticks-distant ]

  if is-ingroup-bond? nearest-ingroup [
    let dir [ link-heading ] of nearest-ingroup

    let pos-pat ( map [d -> patch-at-heading-and-distance dir d ] (list 1 2 3 4 5 6 7 8 9 10))

    carefully[
      foreach pos-pat [p ->  if any? turtles-on p [ set pos-pat remove p pos-pat] ]
      move-to one-of ( patch-set pos-pat )
    ] [ random-walk ]
  ]
  if not is-link? nearest-ingroup [ random-walk ]
end

to random-walk
  rt (random 360) + 1
  jump (random 10) + 1
  if any? other turtles-here [ random-walk ]
end


to-report growth-revision [ number ]       ; returns a value equivalent to the growth rate of logistic function for weight updating (see code book for details)
  let result (
    ifelse-value
    MF-revision = "sigmoid"
    [ ( 2.5 * exp ( -2.5 * ( number - 2.5 ) ) ) / ( 1 + exp ( -2.5 * ( number - 2.5 ) ) )^ 2 ]   ; f'(x), where f(x) = 1 / ( 1 + e^-2.5 * (x-2.5) ) ]
    [ 0.2 ] ; f'(x) where is a linear function, f(x) = 0.2x, which has the same density as the logistic function within the possible range of values for an MF weight (0,5)
  )
  ; round the result to 2 decimal places
  report precision result 2
end

to-report random-normal-in-bounds [mid dev mmin mmax]   ; provides random draws from a bounded random distribution by catching results outside the bounds and redefining them
  let result random-normal mid dev   ; a random draw from a normal distribution (m = mid, sd = dev)
  ; catch function + round draw
  report precision ( ifelse-value
    result < mmin [ mmin + random-float dev ] ;; if the result is below the lower bound, add a random number within 1 sd
    result > mmax [ mmax - random-float dev ] ;; if the result is above the upper bound, subtract a random number within 1 sd
    [ result ] ;; otherwise, simply report the draw
  ) 2
end

to-report random-possion-in-bound [lambda mmin mmax]
  let result random-poisson lambda
  while [ result > mmax ] [ set result random-poisson lambda ]
  report result
end

to-report get-weight [mf]                  ; exchanges a mf number (1-5) for its associated weight for a given turtle
  report item (mf - 1) (list w-care w-fair w-ingroup w-authority w-pure)
end

to-report softmax [ weight-list ]
  report map [ w -> exp w / sum (map exp weight-list) ] weight-list
end

to-report occurrences [x the-list]         ; from NetLogo manual - counts the number of times an item appears on a list
  report reduce
    [ [ occurrence-count next-item ] -> ifelse-value ( next-item = x ) [ occurrence-count + 1 ] [ occurrence-count ] ] ( fput 0 the-list )
end

to-report weighted-prob-draw [prob-list option-list]     ; generates an outcome from a set of probabilities and their associated outcomes
  let pairs (map list option-list prob-list)
  report first rnd:weighted-one-of-list pairs [ [p] -> last p ]
end

to-report lev [ a b ]                      ; calculates the Leventstein distance between two strings
  let dist (
    ifelse-value
    length b = 0 [ length a ]
    length a = 0 [ length b ]
    item 0 a = item 0 b [ lev but-first a but-first b ]
    [ 1 + min ( list lev but-first a b lev a but-first b lev but-first a but-first b ) ]
  )
  report dist
end

to-report lev-ratio [ a b ]               ; calcualtes the the Leventstein ratio (sum of string lengths - Leventstein distance) / sum of string lengths
  let ld lev a b
  let len-sum length a + length b
  let ratio ( ( len-sum - ld ) / len-sum )
  report precision ratio 2
end

to-report string-from-list [ lst sep ]
  let final-string ( ifelse-value
    length lst > 1
    [ reduce [ [result-so-far next-item] -> ( word result-so-far sep next-item ) ] lst ]
    length lst = 1
    [ ( word item 0 lst ) ]
    [ "NA" ] )
  report final-string
end

to collect [state]
  let txt-loc "outputs/raw_txt/"
  let csv-loc "outputs/raw_csv/"
  let tick-state ifelse-value state = "init" [ ticks ][ ticks + 1 ]

  let sim-name word "Sample" sim-id
  let tick-name ( word "_tick" tick-state)

  ; save this tick as .txt
  export-world ( word txt-loc sim-name tick-name ".txt" )

  ; save as csv
  let turtle-props extract-turtle-list ; get the properties
  ; add sim properties
  set turtle-props map [ i -> fput ( word sim-id "_" item 0 i) i ] turtle-props ; add unique identifier for this iteration
  set turtle-props map [ i -> fput ( word sim-id "_" tick-state ) i ] turtle-props ; add unique identifier for this iteration
  set turtle-props map [ i -> fput tick-state i ] turtle-props ; add tick
  set turtle-props map [ i -> fput sim-id i ] turtle-props ; add sample id
  ; rename first line with col names for sim properties
  set turtle-props replace-item 0 turtle-props (replace-item 0 (item 0 turtle-props) "sample")   ; replace for the first number of the first line with the col name
  set turtle-props replace-item 0 turtle-props (replace-item 1 (item 0 turtle-props) "tick")  ; replace for the second number of the first line with the col name
  set turtle-props replace-item 0 turtle-props (replace-item 2 (item 0 turtle-props) "u_tick")  ; replace for the third number of the first line with the col name
  set turtle-props replace-item 0 turtle-props (replace-item 3 (item 0 turtle-props) "u_who")  ; replace for the third number of the first line with the col name

  ; adjustments beyond setup
  if not (state = "init") [
    let sim-so-far csv:from-file ( word csv-loc sim-name ".csv") ; read in previous iterations
    set turtle-props remove-item 0 turtle-props ; remove col names
    let new-turtle-props sim-so-far ; start the new table
    foreach turtle-props [r -> set new-turtle-props lput r new-turtle-props] ; add each line from this iteration

    set turtle-props new-turtle-props ; change name
  ]

  csv:to-file (word csv-loc sim-name ".csv") turtle-props ; save it

end

to picture
  if member? ticks [0 1 10 20 30 40 50 60 70 80 90 100] [
    ask links [set hidden? True]
    export-view (word "Sim example, tick " ticks ".png")
  ]
end

to record
    ask links [set hidden? True]
end

to-report extract-mfs-weights [ id ]
  let all-weights []
  let current-line 0

  file-open ifelse-value ( id = "liberal" ) [ "lib_mfs.csv" ] [ "cons_mfs.csv" ]
  let first-line csv:from-row file-read-line

  while [ not file-at-end? ] [
    set current-line csv:from-row file-read-line
    set all-weights lput current-line all-weights
  ]

  file-close

  report all-weights
end

to-report extract-turtle-list
  let turtle-properties []

  foreach [ who ] of turtles [
    w -> let this-turtle-properties ( list
      w
      last [ xcor ] of turtles with [ who = w ]
      last [ ycor ] of turtles with [ who = w ]
      last [ identity-bin ] of turtles with [ who = w ]
      last [ w-care ] of turtles with [ who = w ]
      last [ w-fair ] of turtles with [ who = w ]
      last [ w-authority ] of turtles with [ who = w ]
      last [ w-ingroup ] of turtles with [ who = w ]
      last [ w-pure ] of turtles with [ who = w ]
      ifelse-value ( (last [my-choice] of turtles with [ who = w ]) = 0) [0] [item 0 (last [ my-choice ] of turtles with [ who = w ])]
      ifelse-value ( (last [my-choice] of turtles with [ who = w ]) = 0) [0] [item 1 (last [ my-choice ] of turtles with [ who = w ])]
      last [ decision ] of turtles with [ who = w ]
      last [ action-taken ] of turtles with [ who = w ]
      last [ conflicted ] of turtles with [ who = w ]
      last [ past-conflict ] of turtles with [ who = w ]
      last [ unhappy ] of turtles with [ who = w ]
      item 0 last [ past-choices ] of turtles with [ who = w ]
      item 1 last [ past-choices ] of turtles with [ who = w ]
      item 2 last [ past-choices ] of turtles with [ who = w ]
      item 3 last [ past-choices ] of turtles with [ who = w ]
      item 4 last [ past-choices ] of turtles with [ who = w ]
      last [ friend-count ] of turtles with [ who = w ]
      last [ similar-nearby ] of turtles with [ who = w ]
      last [ total-nearby ] of turtles with [ who = w ]
      last [ total-all-nearby ] of turtles with [ who = w ]
      last [ ingroup-accuracy ] of turtles with [ who = w ]
      world-size
      density
      conflict-range
      choice-prevalence
      choice-proportion
      choice-invariance
      homophily
      dissimilarity-tolerance
      dt-offset
  )
  set turtle-properties fput this-turtle-properties turtle-properties
  ]
  let col-names (
    list "who" "x_cor" "y_cor" "identity" "care_weight" "fairness_weight" "authority_weight"
    "ingroup_loyalty_weight" "purity_weight" "choice_option_1" "choice_option_2" "decision" "action_taken" "conflicted" "past_conflicts"
    "unhappy" "past_choices_1" "past_choices_2" "past_choices_3" "past_choices_4" "past_choices_5" "perceived_ingroup_n"
    "actual_ingroup_n" "total_neighbours" "total_possible_neighbours" "ingroup_accuracy" "world_size" "density" "conflict_range"
    "ch-prev" "ch_prop" "ch_inv" "homophily" "distol" "dt_offset"
  )

  set turtle-properties fput col-names turtle-properties

  report turtle-properties
end
@#$#@#$#@
GRAPHICS-WINDOW
323
57
841
576
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
-25
25
-25
25
1
1
1
ticks
30.0

SLIDER
140
45
249
78
density
density
1
100
86.0
1
1
%
HORIZONTAL

BUTTON
140
276
195
309
Setup
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

MONITOR
725
578
775
623
N
count turtles
17
1
11

BUTTON
197
276
255
309
Go Once
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

BUTTON
257
276
312
309
Go
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
664
578
723
623
% similar
precision percent-similar 2
17
1
11

PLOT
842
10
1330
164
Similarity over time
Time
% Similar
0.0
10.0
0.0
60.0
true
true
"" ""
PENS
"Liberal" 1.0 0 -5298144 true "" "plot liberal-percent-similar"
"Conservative" 1.0 0 -13345367 true "" "plot conservative-percent-similar"
"Total" 1.0 0 -16777216 true "" "plot perceived-percent-similar"
"Perceived-Liberal" 1.0 0 -1604481 true "" "plot liberal-perceived-percent-similar"
"Perceived-Conservative" 1.0 0 -8020277 true "" "plot conservative-perceived-percent-similar"
"Perceived-Total" 1.0 0 -4539718 true "" "plot perceived-percent-similar"

SLIDER
421
10
575
43
choice-proportion
choice-proportion
0
100
80.0
1
1
%
HORIZONTAL

SLIDER
127
371
291
404
conflict-range
conflict-range
0
2
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
199
248
350
281
homophily
homophily
0
8
4.0
1
1
neighbours
HORIZONTAL

PLOT
0
413
320
575
Conflict over time
Time
% Conflicted
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Liberals" 1.0 0 -5298144 true "plot 0" "plot count liberals with [ conflicted ] * 100 / count liberals"
"Conservatives" 1.0 0 -13345367 true "plot 0" "plot count conservatives with [ conflicted ] * 100 / count conservatives"

MONITOR
323
578
399
623
% unhappy
precision (count turtles with [unhappy] * 100 / count turtles ) 2
17
1
11

MONITOR
400
578
546
623
% unhappy conservatives
precision (count conservatives with [unhappy] * 100 / count conservatives) 2
17
1
11

MONITOR
548
578
661
623
% unhappy liberals
precision (count liberals with [unhappy] * 100 / count liberals) 2
17
1
11

PLOT
0
578
320
725
Disengagement over time
Time
% Disengaging
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Liberals" 1.0 0 -5298144 true "" "plot count liberals with [ decision = \"d\" ] * 100 / count liberals "
"Conservatives" 1.0 0 -14070903 true "" "plot count conservatives with [ decision = \"d\" ] * 100 / count conservatives"

MONITOR
581
626
672
671
shared choice
mf-choice
0
1
11

SLIDER
274
10
420
43
choice-prevalence
choice-prevalence
0
100
50.0
1
1
%
HORIZONTAL

PLOT
843
166
1331
286
Similarity over time for happy turtles
Time
% Similar
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Total" 1.0 0 -16777216 true "" "let similar-neighbors-happy sum [ similar-nearby ] of turtles with [ not unhappy ]\nlet total-neighbors-happy sum [ total-all-nearby ] of turtles with [ not unhappy ]\nplot ( similar-neighbors-happy / total-neighbors-happy ) * 100"
"pen-3" 1.0 0 -5298144 true "" "let lib-similar-neighbors-happy sum [ similar-nearby ] of liberals with [ not unhappy ]\nlet lib-total-neighbors-happy sum [ total-all-nearby ] of liberals with [ not unhappy ]\nifelse any? liberals with [ not unhappy ]\n  [ plot ( lib-similar-neighbors-happy / lib-total-neighbors-happy ) * 100 ]\n  [ plot 0 ]"
"pen-5" 1.0 0 -13345367 true "" "let con-similar-neighbors-happy sum [ similar-nearby ] of conservatives with [ not unhappy ]\nlet con-total-neighbors-happy sum [ total-all-nearby ] of conservatives with [ not unhappy ]\nifelse any? conservatives with [ not unhappy ]\n  [ plot ( con-similar-neighbors-happy / con-total-neighbors-happy ) * 100 ]\n  [ plot 0 ]"

MONITOR
469
626
580
671
% d liberal choices
occurrences \"d\" reduce sentence [ past-choices ] of liberals * 100 / length reduce sentence [ past-choices ] of liberals
2
1
11

MONITOR
323
625
467
670
% d conservative choices
occurrences \"d\" reduce sentence [ past-choices ] of conservatives * 100 / length reduce sentence [ past-choices ] of conservatives
2
1
11

SLIDER
0
45
138
78
past-choices-length
past-choices-length
3
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
197
195
343
228
influence-strength
influence-strength
0
1
1.0
0.05
1
NIL
HORIZONTAL

PLOT
843
289
1334
614
MFs
Time
Moral relevance
0.0
10.0
0.0
5.0
true
true
"" ""
PENS
"Fairness (cons)" 1.0 0 -3844592 true "" "plot (sum [w-fair] of conservatives) / (count conservatives)"
"Care (cons)" 1.0 0 -12087248 true "" "plot (sum [w-care] of conservatives) / (count conservatives)"
"Ingroup (cons)" 1.0 0 -14070903 true "" "plot (sum [w-ingroup] of conservatives) / (count conservatives)"
"Authority (cons)" 1.0 0 -10141563 true "" "plot (sum [w-authority] of conservatives) / (count conservatives)"
"Purity (cons)" 1.0 0 -4757638 true "" "plot (sum [w-pure] of conservatives) / (count conservatives)"
"Fairness (libs)" 1.0 0 -817084 true "" "plot (sum [w-fair] of liberals) / (count liberals)"
"Care (libs)" 1.0 0 -8732573 true "" "plot (sum [w-care] of liberals) / (count liberals)"
"Ingroup (libs)" 1.0 0 -10649926 true "" "plot (sum [w-ingroup] of liberals) / (count liberals)"
"Authority (libs)" 1.0 0 -6917194 true "" "plot (sum [w-authority] of liberals) / (count liberals)"
"Purity (libs)" 1.0 0 -1664597 true "" "plot (sum [w-pure] of liberals) / (count liberals)"

SWITCH
51
161
216
194
asymmetric-tolerance
asymmetric-tolerance
0
1
-1000

CHOOSER
94
81
186
126
MF-revision
MF-revision
"sigmoid" "linear"
0

SWITCH
964
617
1064
650
save-data
save-data
1
1
-1000

SWITCH
844
650
962
683
save-view
save-view
1
1
-1000

SWITCH
964
651
1085
684
movement
movement
0
1
-1000

SWITCH
844
616
962
649
recording
recording
1
1
-1000

INPUTBOX
250
68
320
128
world-size
25.0
1
0
Number

SLIDER
110
127
325
160
dissimilarity-tolerance
dissimilarity-tolerance
0
100
30.0
1
1
%
HORIZONTAL

SLIDER
576
10
725
43
choice-invariance
choice-invariance
0
100
100.0
1
1
%
HORIZONTAL

SLIDER
217
161
323
194
dt-offset
dt-offset
0
dissimilarity-tolerance
4.0
1
1
NIL
HORIZONTAL

SWITCH
1065
618
1237
651
negative-influence
negative-influence
1
1
-1000

@#$#@#$#@
## WHAT IS IT?

This (second; see credits and references for iteration 1) iteration of the model attempts to model political mass polarisation (ideologically and exclusivity of social group) as a function of moral decisions between 1-2 moral foundations (where two can induce conflict) for political agents (identifies as liberal/conservative) used as social signalling of political identity and uncertain social inference from other turtles moral decisions.

## HOW IT WORKS



## HOW TO USE IT

During setup political turtles are generated in the model with the variables needed to make moral choices, find friends, deal with conflict, and determine their own happiness. Prior to setup, the following interface objects should be considered: (1) density. Turtles are generated at the density specified with the density slider (i.e., the proportion of patches that will hold a turtle with some random noise). (2) the n-identity chooser. This specifies the resolution of turtles' political identity space, either binary (conservative/liberal) or graded (six political identities; very liberal/conservative, liberal/conservative and slightly liberal/conservative). (3) identity-colour. This allows you to have graded identities show in turtle colours if this was chosen (i.e., lighter blue/red for slightly liberal/conservative and darker for very liberal/conservative). Choosing identity affects how decision weights are generated during setup. (4) past-choices-length. This indicates how many items a turtle's memory of its own and others' choices should be. Note, this parameter requires a lot of computional memory beyond 5.

The remaining parameters can be changed throughout as they are used by go as well. This includes turtle-shape, which allows you to distinguish happy and unhappy turtles (square-x) or see the overall pattern regardless of happiness status (all square). The conflict-range slider indicates the upper boundary for a difference between decision weights before it is considered conflict, i.e., the higher it is the more common conflict will be. The 

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## RECOMMENDED INTERFACE



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
