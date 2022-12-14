# Modelling for Sustainable Societies: a sharing economy model - group assignment
The present webpage has been created in order to enable the reproduction of results of the research paper. Information about input data, output data and the model code will be here presented.

## Input data
The four scenarios have been run on the following settings. It is important to notice how the only changing settings are 'guest-preference' and 'price strategy'.

### Scenario 1

| Setting | Value |
| ------ | ----------- |
| number-steps | 2000 |
| comput-quality | "abs" |
| guest-preference | "quality-pref" |
| get-data? | FALSE |
| update-data-frequency | 50 |
| tau-price | 0.1 |
| population | 100 |
| focus-occupancy | 0.5 |
| new-adjust-price? | FALSE |
| local-random-seed | 1 |
| init-price-randomness | 0 |
| price-range-search | 2 |
| price-strategy | "occupancy-reviews" |
| tau-reputation | 0.1 |
| proba-action | 0.01 |
| past-reviews-range | 0.9 |

### Scenario 2

| Setting | Value |
| ------ | ----------- |
| number-steps | 2000 |
| comput-quality | "abs" |
| guest-preference | "price-pref" |
| get-data? | FALSE |
| update-data-frequency | 50 |
| tau-price | 0.1 |
| population | 100 |
| focus-occupancy | 0.5 |
| new-adjust-price? | FALSE |
| local-random-seed | 1 |
| init-price-randomness | 0 |
| price-range-search | 2 |
| price-strategy | "occupancy-reviews" |
| tau-reputation | 0.1 |
| proba-action | 0.01 |
| past-reviews-range | 0.9 |

### Scenario 3

| Setting | Value |
| ------ | ----------- |
| number-steps | 2000 |
| comput-quality | "abs" |
| guest-preference | "quality-pref" |
| get-data? | FALSE |
| update-data-frequency | 50 |
| tau-price | 0.1 |
| population | 100 |
| focus-occupancy | 0.5 |
| new-adjust-price? | FALSE |
| local-random-seed | 1 |
| init-price-randomness | 0 |
| price-range-search | 2 |
| price-strategy | "occupancy" |
| tau-reputation | 0.1 |
| proba-action | 0.01 |
| past-reviews-range | 0.9 |

### Scenario 4

| Setting | Value |
| ------ | ----------- |
| number-steps | 2000 |
| comput-quality | "abs" |
| guest-preference | "price-pref" |
| get-data? | FALSE |
| update-data-frequency | 50 |
| tau-price | 0.1 |
| population | 100 |
| focus-occupancy | 0.5 |
| new-adjust-price? | FALSE |
| local-random-seed | 1 |
| init-price-randomness | 0 |
| price-range-search | 2 |
| price-strategy | "occupancy" |
| tau-reputation | 0.1 |
| proba-action | 0.01 |
| past-reviews-range | 0.9 |

---
## Output data
data well documented

## Model code

The researchers have reached out to the author of the paper, Adrien Querbes, who kindly shared the appositely coded model. The code is here reported in its original version, which has been used in the present study.

    // Some comments
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

--- 
