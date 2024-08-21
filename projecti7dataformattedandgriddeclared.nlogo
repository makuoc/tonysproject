extensions [csv table]  ; Allows the use of the CSV extension for handling CSV files

globals [  ; Declares global variables
  primary-sectors  ; List of primary sectors
  secondary-sectors  ; List of secondary sectors
  tertiary-sectors  ; List of tertiary sectors
  bank-sets
  business-spending  ; Corrected from "buisness-spending"
  current-year; make list to store yearly data projections
  days-per-year
  some-threshold
  desired-growth-rate
  desired-revenue-target
  economic-growth
  exchange-rate  ; Exchange rate (updated based on inflation)
  gdp  ; Current Gross Domestic Product
  inflation-rate  ; Inflation rate calculated based on GDP changes
  previous-gdp  ; GDP from the previous time step
  gdp-data  ; Data containing GDP values loaded from the CSV file
  household-taxation-rate  ; Households tax rate
  month
  palliative-threshold
  palliative-amount
  sectors  ; List of all sectors in the economy
  sector-gdp  ; GDP values for each sector
  sector-expenditures  ; Expenditures for each sector
  sector-lending
  sector-spending
  sector-tax-revenue
  sme-taxation-rate  ; SMEs tax rate
  stolen-money-list
  total-business-spending  ; Corrected from "total-buisness-spending"
  total-tax-revenue
  year1; initialised to 1981
  best-fitness-history  ; History of best fitness values
  population  ; Population for the genetic algorithm (not directly used in agent behaviors)
  best-fitness  ; Best fitness value found by the genetic algorithm
  best-solution  ; Best solution (combination of fiscal policy and monetary policy)
  mutation-rate  ; Mutation rate for the genetic algorithm
  num-generations  ; Number of generations for the genetic algorithm
]

breed [households household]  ; Defines a breed called households
breed [banks bank]            ; Defines a breed called banks
breed [governments government] ; Defines a breed called governments
breed [large-businesses large-business] ; Corrected from "large-buisnesses"
breed [smes sme]             ; Defines a breed called SMEs (Small and Medium Enterprises)

households-own [  ; Declares properties owned by households
  income  ; Income of the household
  consumption  ; Total consumption expenditure of the household
  angent-type
  savings  ; Amount of income that is saved
  sector-employed
  previous-wealth
  agent-type
  has-account
  savings-percentage
]

banks-own [  ; Declares properties owned by banks
  agent-type
  lending-criteria  ; Lending criteria of the bank
  interest-rates  ; Interest rates offered by the bank
  previous-wealth
  bank-balance  ; Sources of funds for the bank
  loans
]

governments-own [  ; Declares properties owned by governments
  agent-type
  bank-influence
  budget-constraints  ; Budget constraints of the government
  corruption
  deficit
  expenditure
  fiscal-policy  ; Fiscal policy (government spending)
  gov-lending-criteria
  monetary-policy  ; Monetary policy (interest rates)
  palliative
  previous-wealth
  revenue
  revenue-generated
  sector-priorities  ; Priorities for each sector (for budget allocation)
  taxation-rate  ; Taxation rate
]

large-businesses-own [  ; Corrected from "large-buisnesses-own"
  agent-type
  capital  ; Capital owned by the large business
  expenditure
  has-account
  industry-sector
  investment
  investment-percentage
  loan-amount
  operating-costs
  previous-wealth
  revenue-generated
  loan-amount
  loan-payment-with-interest
  savings
  savings-percentage
]

smes-own [
  agent-type
  capital
  expenditure  ; Expenditure of the SME (not directly used)
  has-account
  industry-sector  ; The sector the SME operates in
  investment
  investment-rate
  lending-criteria
  loan-amount
  loan-source
  previous-wealth
  repayment-rate
  revenue-generated;
  savings
  savings-percentage
]

to initialize-agents
  let sectors-list sectors  ; Use the existing 'sectors' variable

  ; Create one household for each sector in the list
  foreach sectors-list [
    sector ->
    create-households 1 [
      set income get-sector-gdp sector-employed year1
      set agent-type "household"
      set consumption income * random-float 1
      set sector-employed sector
      set savings 0
      set color blue
      set has-account one-of [true false]
      set shape "person"
      set savings-percentage random 0.2
    ]
  ]

  ; Create additional households to reach a total of 1000
  create-households (1000 - length sectors-list) [
    set income get-sector-gdp sector-employed year1
    set agent-type "household"
    set consumption income * random-float 1
    set sector-employed one-of sectors-list
    set savings 0
    set color blue
    set has-account one-of [true false]
    set shape "person"
    set savings-percentage random 0.2
  ]

  create-banks 10 [
    set lending-criteria random 100
    set agent-type "bank"
    set interest-rates random 10 + 5
    set sector-lending map [sector -> (list sector (random-float 1))] sectors
    set color yellow
    set bank-balance 20000000
    set shape "star"
  ]
  create-governments 1 [
    set bank-influence 0  ;; Example default value
    set budget-constraints budget-constraints + 1000000000
    set corruption random 0.4 ;; Example default value
    set deficit 0  ;; Example default value
    set expenditure 0
    set fiscal-policy budget-constraints - budget-constraints * corruption + revenue-generated
    set gov-lending-criteria 0  ;; Example default value
    set monetary-policy 0.05
    set palliative 0  ;; Example default value
    set previous-wealth 0  ;; Example default value
    set revenue-generated 0 ;; Initialize revenue
    set taxation-rate 0  ;; Example default value
    set color red
    set shape "circle"

  ]

  foreach sectors-list [
    sector ->
    create-smes 1 [
      set industry-sector one-of sectors
      set color orange
      set capital random 100000
      set lending-criteria random 100
      set investment-rate random 0.7
      set investment capital * investment-rate
      set savings 100000
      set agent-type "smes"
      set loan-amount 0
      set has-account one-of [true false]
      set shape "arrow"
      set color green
      set repayment-rate 0.2
      set savings-percentage 0.6
    ]
  ]
  create-smes (499 - length sectors-list) [
    set industry-sector one-of sectors
    set color orange
    set savings 100000
    set capital random 100000
    set agent-type "smes"
    set investment-rate random 0.7
    set investment capital * investment-rate
    set lending-criteria random 100
    set has-account one-of [true false]
    set shape "arrow"
    set color green
    set repayment-rate 0.2
    set savings-percentage random 0.6
  ]
  foreach sectors-list [
    sector ->
    create-large-businesses 1 [
      let initial-sectors one-of sectors
      set industry-sector initial-sectors
      set capital random 10000000
      set agent-type "large-businesses"
      set color brown
      set shape "triangle"
      set has-account one-of [true false]
      set savings-percentage random 0.4
      set loan-amount 0
      set operating-costs 0.4 * capital
      set investment capital - operating-costs * investment-percentage
    ]
  ]
  create-large-businesses (49 - length sectors-list) [
    let initial-sectors one-of sectors
    set industry-sector initial-sectors
    set capital random 1000000
    set agent-type "large-businesses"
    set color brown
    set has-account one-of [true false]
    set shape "triangle"
    set savings-percentage random 0.4
    set loan-amount 0
  ]
end

to load-gdp-data [file-path]
  set gdp-data csv:from-file file-path
;  print "Loaded data:"
;  print-csv-data
;  print "Raw gdp-data for sectors extraction:"
  set sectors remove "Period" (remove "tyear" but-last but-last but-last item 0 gdp-data)
  print "Extracted sectors:"
  ;print sectors
  process-gdp-data but-first gdp-data
end

to process-gdp-data [input-data]
  ;; Create a new table to store GDP data by sector
  set sector-gdp table:make

  ;; Process each row in the input data
  foreach input-data [row ->
    let year item 0 row  ;; Extract the year from the row
    let period item 1 row  ;; Extract the period (e.g., "Annual") from the row

    ;; Process only the "Annual" periods
    if member? period ["Annual"] [
      let sector-values sublist row 2 (length row)  ;; Extract sector values

      ;; Iterate through each sector value and associate it with the sector name
      let num-sectors 52
      let index 0
      repeat num-sectors [
        let sector-name item index sectors  ;; Get the sector name from the sectors list
        let value item index sector-values  ;; Get the corresponding GDP value

        ;; Check if the sector is already in the table
        ifelse table:has-key? sector-gdp sector-name [
          ;; If the sector exists, update its GDP values by appending the new year's data
          let existing-data table:get sector-gdp sector-name
          let updated-data lput (list year value) existing-data
          table:put sector-gdp sector-name updated-data

          ;; Print for debugging
          ;print (word "Updated sector " sector-name " for year " year " with data: " updated-data)
        ] [
          ;; If the sector doesn't exist, create a new entry with the current year's data
          let new-data (list (list year value))  ;; Create a list of lists for the new sector
          table:put sector-gdp sector-name new-data

          ;; Print for debugging
          ;print (word "Created new sector " sector-name " for year " year " with data: " new-data)
        ]

        set index index + 1  ;; Increment the index to move to the next sector
      ]
    ]
  ]

  ;; Print the final grouped GDP data stored in the table
  print (word "Final sector-gdp data: " table:to-list sector-gdp)
end

to-report get-sector-gdp [sector year]
  if table:has-key? sector-gdp sector [
    let sector-data table:get sector-gdp sector

    foreach sector-data [data-point ->
      if item 0 data-point = year [
        let gdp-in-billions item 1 data-point * random-float 0.001
        let scaled-gdp gdp-in-billions / 1000  ;; Convert billions to millions
        report scaled-gdp
      ]
    ]
  ]

  report 0  ;; Return 0 if sector or year not found
end

to setup
  clear-all
  load-gdp-data "C:/Users/idima/OneDrive/Desktop/New folder/project folder/RealGDP28072024.csv"
  ;set scaling-factor 10
  set household-taxation-rate 0.15
  set sme-taxation-rate 0.20
  set some-threshold 50000000  ;; Example threshold for deficit triggering tax increase
  set desired-growth-rate 0.03  ;; Example target for economic growth rate (3%)
  set desired-revenue-target 1000000
  set economic-growth gdp - previous-gdp
  set inflation-rate 0
  set year1 1981
  set month 1
  set gdp 0
  set bank-sets banks
  set previous-gdp 0
  set mutation-rate 0.4
  set num-generations 50


  initialize-population
  initialize-agents
  update-size
  create-sectors


;  ; Initialize sector spending
;  set sector-expenditures n-values 52 [0] ;Similar to sector-spending, but this likely tracks the expenditures made by each sector.
;  set sector-lending n-values 52 [[]] ;Initializes a list where each element is an empty list, representing loans or financial lending related to each sector
;  set sector-tax-revenue n-values 52 [0] ;Initializes a list of zeros, where each element tracks the tax revenue generated by each sector.

  reset-ticks
end

to go

  evolve
  plot-best-fitness
  set month month + 1
  if month > 12 [
    set month 1
    set year1 year1 + 1
  ]
end


;  update-economic-indicators
;  tick

;to go

;agent-actions
;  ;; Add a stopping condition to prevent infinite recursion
;;  ifelse ticks < num-generations [
;;    tick
;;    go
;;  ] [q
;;    show "Simulation complete"
;;  ]
;end

to-report flatten [list-of-lists]
  report reduce [[?1 ?2] -> sentence ?1 ?2] list-of-lists
end


to agent-actions
  decide-household-actions
  decide-bank-actions
  government-actions
  ;distribute-business-revenue
  ;decide-smes-actions
  ;update-economic-indicators

end

to decide-household-actions
  ask households [
    update-household-income
    collect-taxes
    ;; implement governemnt income tax here

    ;; Combine all businesses (SMEs and large businesses) into a single list.
    let all-businesses (list smes large-businesses)
    let flat-businesses reduce sentence all-businesses

    ;; Decide how many businesses the agent will buy from
    let num-businesses-to-visit random (length flat-businesses) + 1

    ;; Select a random subset of businesses to spend on
    let chosen-businesses n-of num-businesses-to-visit all-businesses

    ; Iterate over each set in the list
    foreach chosen-businesses [current-business ->
      let current-set current-business  ;;
      let chosen-business one-of current-set ;; Random business from current set
      ;; Calculate the amount to spend on this business
      let amount-to-spend consumption
      ;; Check if the chosen business is an SME
      if member? chosen-business smes [
        ;; If it's an SME, add the amount spent to its revenue-generated property
        ask chosen-business [
          set revenue-generated revenue-generated + amount-to-spend
        ]
      ]

      if member? chosen-business large-businesses [
        ;; If it's an large-businesses, add the amount spent to its revenue-generated property
        ask chosen-business [
          set revenue-generated revenue-generated + amount-to-spend
        ]
      ]

      ;; Subtract the amount spent from the household's consumption income
      set consumption consumption - amount-to-spend
    ]
    if consumption > 0 or income > 0 [
      set savings savings + consumption + income
      set consumption 0  ;; Optionally reset consumption to 0 after saving
      set income 0  ;; Optionally reset income to 0 after saving
    ]

    let chosen-bank one-of banks
    ask chosen-bank [
      set bank-balance bank-balance + savings
      ;; Apply interest to the bank's balance
      set bank-balance bank-balance + (bank-balance * 0.01)
      ;update-wealth-and-animations
    ]

    spend-savings
    ]



end

to spend-savings
  ;; Households decide randomly whether to spend their savings
  ask households [
    if savings > 0 [
      ;; Randomly decide whether to spend (e.g., 50% chance)
      if random-float 1 < 0.5 [
        ;; Choose a random percentage of savings to withdraw
        let withdrawal-percentage random-float 1.0  ;; Generates a value between 0 and 1
        set income savings * withdrawal-percentage

        ;; Choose a bank to withdraw from
        let chosen-bank one-of banks

        ;; Withdraw the amount from the bank's balance
        ask chosen-bank [
          set bank-balance bank-balance - income
          ;update-wealth-and-animations
        ]

        ;; Reduce savings by the withdrawal amount
        set savings savings - income
        ;update-wealth-and-animations
      ]
    ]
  ]
end

to update-household-income
  ;; Update income based on sector GDP for the specific year
  ask households [
    set income get-sector-gdp sector-employed year1
  ]
end

to-report calculate-sector-proportions [year]
  ;; Clear previous sector-spending data
  set sector-spending []

  ;; Initialize total GDP for the specified year
  let total-gdp 0

  foreach sectors [sector-name ->
    let sector-data table:get sector-gdp sector-name
    foreach sector-data [data-point ->
      if item 0 data-point = year [
        set total-gdp total-gdp + item 1 data-point
      ]
    ]
  ]

  ;; Report total GDP for debugging (optional)
  ;print (word "Total GDP for year " year ": " total-gdp)

  ;; Calculate spending shares based on total GDP
  foreach sectors [sector-name ->
    let sector-data table:get sector-gdp sector-name
    let gdp-in-year 0  ;; Initialize GDP for the year to 0

    foreach sector-data [data-point ->
      if item 0 data-point = year [
        set gdp-in-year item 1 data-point  ;; Get the GDP value for the specified year
      ]
    ]

    ;; Calculate spending share if total GDP is not zero
    let spending-share ifelse-value (total-gdp != 0) [gdp-in-year / total-gdp] [0]

    ;; Append the sector name and spending share to sector-spending
    set sector-spending lput (list sector-name spending-share) sector-spending
  ]

  report (list sector-spending total-gdp)
end

to receive-palliative [amount]
  ;; Update household income with the palliative amount
  set income income + amount

  ;; Print message for debugging or information
  print (word "Household " who " received " amount " in palliative measures.")
end


;government actions start here
to government-actions
  ;; Initialize or reset the stolen money list for this iteration
  set stolen-money-list []

  ;; Calculate sector spending shares for the given year
  adjust-taxation-rates
  collect-taxes
  adjust-revenue-based-on-monetary-policy
  let sector-proportions first calculate-sector-proportions year1

  ;; Adjust fiscal policy based on inflation rate
  ifelse inflation-rate > 3 [  ;; If inflation is high, reduce spending to cool down the economy
    set fiscal-policy fiscal-policy * 0.9  ;; Decrease fiscal policy by 10%
  ] [
    set fiscal-policy fiscal-policy * 1.1  ;; Increase fiscal policy by 10% if inflation is low
  ]

  ;; Add revenue to fiscal policy
  set fiscal-policy fiscal-policy + revenue

  ;; Get the adjusted government budget
  let total-government-budget fiscal-policy

  ;; Iterate over each sector to allocate spending based on proportions
  foreach sector-proportions [ sector-pair ->
    let sector-name item 0 sector-pair  ;; Extract sector name
    let spending-share item 1 sector-pair  ;; Extract spending share

    ;; Calculate the allocated spending for the sector
    let allocated-spending spending-share * total-government-budget

    ;; Check if there is enough budget to allocate
    ifelse total-government-budget >= allocated-spending [
      ;; Distribute the allocated spending to SMEs and large businesses in the sector
      ask smes with [ member? sector-name industry-sector ] [
        set capital capital + allocated-spending * 0.3  ;; Example allocation to SMEs
      ]
      ask large-businesses with [ member? sector-name sectors ] [
        set capital capital + allocated-spending * 0.6  ;; Example allocation to large businesses
      ]

      ;; Deduct the allocated spending from the government budget
      set total-government-budget total-government-budget - allocated-spending
      ;update-wealth-and-animations
    ]
    [
      ;; Handle cases where the budget is insufficient
      print (word "Insufficient budget for sector " sector-name ". Diverting to corruption.")
      ;; Divert insufficient budget to corruption
      set corruption corruption + allocated-spending
      set stolen-money-list lput allocated-spending stolen-money-list
    ]
  ]

  ;; Update government deficit if any budget is used beyond available funds
  if total-government-budget < 0 [
    set deficit (- total-government-budget)
  ]

  ;; Set new budget for the next year

  ;; Optionally call a procedure to collect taxes or update financials
  collect-taxes
  give-sector-loans
  give-palliative-measures
  determine-next-year-budget
  ;update-wealth-and-animations
end

to give-palliative-measures ;factor in corruption here
  ;; Define the total palliative budget available for distribution
  let palliative-budget 15000000  ;; Example amount, adjust as needed

  ;; Check if there is enough budget to provide palliative measures
  ifelse fiscal-policy >= palliative-budget [
    ;; Calculate the amount to distribute per household
    let households-count count households
    let amount-per-household palliative-budget / households-count

    ;; Distribute palliative measures to households
    ask households [
      receive-palliative amount-per-household
    ]

    ;; Deduct the palliative budget from the total government budget
    set fiscal-policy fiscal-policy - palliative-budget

    ;; Print message for debugging or information
    ;print (word "Distributed " palliative-budget " in palliative measures to " households-count " households.")
  ]
  [
    ;; Handle cases where the budget is insufficient
    ;print "Insufficient budget for palliative measures. No distribution occurred."
  ]
end

to collect-taxes
  ask households [
    let tax-amount income * household-taxation-rate
    set income income - tax-amount
    ;update-wealth-and-animations
  ]
  ask smes [
    let sme-tax-amount revenue-generated * sme-taxation-rate
    set revenue-generated revenue-generated - sme-tax-amount
    ;update-wealth-and-animations
  ]
  ask governments [
      let household-revenue sum [income * household-taxation-rate] of households
      let sme-revenue sum [revenue-generated * sme-taxation-rate] of smes
      set fiscal-policy revenue + household-revenue + sme-revenue
      ;update-wealth-and-animations
  ]
end

to adjust-revenue-based-on-monetary-policy
  ;; Assume `monetary-policy` is the current interest rate.
  ;; Lowering interest rates boosts economic activity
  let total-gdp calculate-gdp

  if monetary-policy < 0.05 [
    ;; Boost investments, spending, and thus revenue
    let investment-boost (0.1 / monetary-policy)
    set revenue-generated revenue-generated + (investment-boost * total-gdp * taxation-rate)
  ]

  ;; Raising interest rates controls inflation and attracts investment
  if monetary-policy >= 0.05 [
    ;; Assume a small increase in revenue due to inflation control
    let inflation-control-benefit (monetary-policy * 0.01)
    set revenue-generated revenue-generated + (inflation-control-benefit * total-gdp * taxation-rate)

    ;; Boost bond revenue if applicable
    let bond-revenue (monetary-policy * 10000000)  ;; Example fixed amount
    set revenue-generated revenue-generated + bond-revenue
  ]

  set fiscal-policy fiscal-policy + revenue-generated
  ;update-wealth-and-animations
end

to adjust-taxation-rates
  ;; Example rule: Increase household taxation rate if government deficit is high
  if deficit > some-threshold [
    set household-taxation-rate household-taxation-rate + 0.01  ;; Increase by 1%
  ]

  ;; Example rule: Decrease SME taxation rate to stimulate economic growth
  if economic-growth < desired-growth-rate [
    let new-rate (sme-taxation-rate - 0.01)  ;; Calculate new rate
    set sme-taxation-rate max (list new-rate 0.01)
  ]

  ;; Example rule: Adjust based on overall government revenue needs
  if revenue < desired-revenue-target [
    set household-taxation-rate household-taxation-rate + 0.005  ;; Increase by 0.5%
    set sme-taxation-rate sme-taxation-rate + 0.005  ;; Increase by 0.5%
  ]

  ;; Clamp household-taxation-rate between 0.0 and 0.5
  set household-taxation-rate min (list 0.5 max (list 0.0 household-taxation-rate))

;; Clamp sme-taxation-rate between 0.0 and 0.5
  set sme-taxation-rate min (list 0.5 max (list 0.0 sme-taxation-rate))

end

to give-sector-loans
  ;; Define the total loan amount available for distribution
  let total-loan-amount 1500000000  ;; Example amount, adjust as needed

  let sector-proportions first calculate-sector-proportions year1

  ;; Iterate over each sector-proportions to distribute loans
  foreach sector-proportions [ sector-pair ->
    let sector-name item 0 sector-pair  ;; Extract sector name
    let gdp-share item 1 sector-pair  ;; Extract GDP share

    ;; Calculate the allocated loan amount for the sector
    let allocated-loan gdp-share * total-loan-amount

    ;; Check if there is enough loan amount to allocate
    ifelse total-loan-amount >= allocated-loan [
      ;; Distribute the allocated loan to SMEs and large businesses in the sector
      ask large-businesses with [ member? sector-name sectors ] [
        let loan-for-business allocated-loan * 0.6  ;; Example allocation to large businesses
        set loan-amount loan-for-business
        set capital capital + loan-for-business
        set loan-source "government"

        ;; Deduct the loan amount from the allocated amount for the sector
        set allocated-loan allocated-loan - loan-for-business
        ;print (word "Large business " who " in sector " sector-name " received a loan of " loan-for-business " from the government.")
      ]

      ;; If there is still loan amount left, allocate to SMEs
      if allocated-loan > 0 [
        ask smes with [ member? sector-name industry-sector ] [
          let loan-for-sme allocated-loan * 0.3  ;; Example allocation to SMEs
          set loan-amount loan-for-sme
          set capital capital + loan-for-sme
          set loan-source "government"

          ;; Deduct the loan amount from the allocated amount for the sector
          set allocated-loan allocated-loan - loan-for-sme
          ;print (word "SME " who " in sector " sector-name " received a loan of " loan-for-sme " from the government.")
        ]
      ]

      ;; Deduct the allocated loan amount from the total loan amount
      set total-loan-amount total-loan-amount - (15000000000 - total-loan-amount)
    ]
    [
      ;; Handle cases where the budget is insufficient
      ;;print (word "Insufficient loan budget for sector " sector-name ". Diverting to corruption.")
      ;; Divert insufficient loan budget to corruption
      set corruption corruption + allocated-loan
      set stolen-money-list lput allocated-loan stolen-money-list
    ]
  ]
end

to determine-next-year-budget
  ;; Collect taxes and adjust revenue
  collect-taxes
  adjust-revenue-based-on-monetary-policy

  ;; Consider previous year's deficit and surplus
  let budget-increase-factor 1.0
  if deficit > 0 [
    ;; Reduce budget if there was a deficit
    set budget-increase-factor 0.95
  ]
  if deficit <= 0 [
    ;; Increase budget if there was no deficit or a surplus
    set budget-increase-factor 1.05
  ]

  ;; Calculate initial budget based on current fiscal policy and economic conditions
  let projected-budget fiscal-policy * budget-increase-factor

  ;; Allocate the budget across sectors
  let sector-proportions first calculate-sector-proportions year1
  foreach sector-proportions [ sector-pair ->
    let sector-name item 0 sector-pair
    let spending-share item 1 sector-pair
    let allocated-spending spending-share * projected-budget

    ;; Distribute allocated spending to SMEs and large businesses in the sector
    ifelse projected-budget >= allocated-spending [
      ask smes with [ member? sector-name industry-sector ] [
        set capital capital + allocated-spending * 0.3
      ]
      ask large-businesses with [ member? sector-name sectors ] [
        set capital capital + allocated-spending * 0.6
      ]
      set projected-budget projected-budget - allocated-spending
    ] [
      ;; Handle insufficient budget by allocating to corruption
      set corruption corruption + allocated-spending
      set stolen-money-list lput allocated-spending stolen-money-list
    ]
  ]

  ;; Allocate loans and palliative measures if budget permits
  if projected-budget > 0 [
    give-sector-loans
    give-palliative-measures
  ]

  ;; Update fiscal policy with the remaining budget
  set fiscal-policy projected-budget

  ;; Collect taxes for the next year to prepare for the budget cycle
  collect-taxes
end


;sme actions start here
to smes-actions
  sme-receive-revenue
  collect-taxes
  spend-capital
  seek-bank-and-open-account
  sme-seek-loans bank-sets
  repay-loans
end

to sme-receive-revenue
  ;; SMEs collect the money spent by households
  ask smes [
    ;; Add the generated revenue to the SME's capital
    set capital capital + revenue-generated
    ;; Reset revenue-generated for the next cycle
    set revenue-generated 0
  ]
end

to spend-capital
  ;; Combine all businesses (SMEs and large businesses) into a single list.
  let all-businesses (list smes large-businesses)
  let flat-businesses reduce sentence all-businesses

  ;; Decide how many businesses the agent will invest in
  let num-businesses-to-visit random (length flat-businesses) + 1

  ;; Select a random subset of businesses to invest in
  let chosen-businesses n-of num-businesses-to-visit flat-businesses

  ;; Iterate over each chosen business
  foreach chosen-businesses [current-business ->
    ;; Determine the amount to spend based on the business type
    let amount-to-spend 0
    if member? current-business smes [
      ;; Calculate investment for SMEs
      set amount-to-spend [capital] of current-business * [investment-rate] of current-business
    ]
    if member? current-business large-businesses [
      ;; Calculate investment for large businesses
      set amount-to-spend [capital] of current-business * [investment-rate] of current-business
    ]

    ;; Ensure the spending amount does not exceed the available capital
    if amount-to-spend > [capital] of current-business [
      set amount-to-spend [capital] of current-business
    ]

    ;; Choose a sector to invest in from the `sectors` list
    let chosen-sector one-of sectors

    ;; Update the business's investment or other relevant attributes for the chosen sector
    ask current-business [
      ;; Deduct the invested amount from capital
      set capital capital - amount-to-spend

      ;; Optionally, update investment
      set investment investment + amount-to-spend

      ;; Optionally, store the amount spent and sector in the business's attributes
      ;; e.g., set sector-investment chosen-sector (amount-to-spend)
    ]

    ;; Log or update spending information
    ;print (word "Business investing " amount-to-spend " in sector " chosen-sector)
  ]
end

to sme-seek-loans [bank-set]
  ;; SMEs decide whether to apply for a loan based on their capital needs and sector demand.
  ask smes [
    ;; Define SME-specific variables
    let loan-needed? false
    let loan-offer 0
    let chosen-bank one-of bank-set  ;; Randomly choose a bank from the set
    let desired-capital-level capital * 3
    let total-loan-amount 0
    ;; SMEs evaluate their current capital and determine if they need a loan
    if capital < desired-capital-level [
      set loan-needed? true
    ]

    ;; If the SME needs a loan, it calculates the potential loan offer
    if loan-needed? [
      ;; Calculate the sector demand and loan offer
      let sector-demand sum [income] of households with [sector-employed = industry-sector]
      set loan-offer sector-demand * 0.05  ;; For example, 5% of sector demand

      ;; Ask the chosen bank for a loan if it meets the lending criteria
      ask chosen-bank [
        let acceptance-chance random-float 100
        let acceptance-threshold 75

        ;; Check if the SME meets the lending criteria and if the bank has enough funds
        if (lending-criteria > random 100) and (acceptance-chance < acceptance-threshold) [
          ;; Ensure that the loan offer does not exceed the bank's available loan budget
          if (total-loan-amount + loan-offer) <= bank-balance [
            ;; Grant the loan to the SME
            set capital capital + loan-offer
            set loan-amount loan-offer
            set loan-source "bank"
            set total-loan-amount total-loan-amount + loan-offer
            ;; Deduct the loan amount from the bank's balance
            set bank-balance bank-balance - loan-offer
          ]
        ]
      ]
    ]
  ]
end

to repay-loans
  ask smes [
    let repayment-amount loan-amount * repayment-rate
    if capital >= repayment-amount [
      set loan-amount loan-amount - repayment-amount
      set capital capital - repayment-amount
      if loan-source = "bank" [
        ask banks [set bank-balance bank-balance + repayment-amount]
      ]
      if loan-source = "government" [
        ask governments [set fiscal-policy fiscal-policy + repayment-amount]
      ]
      if loan-amount <= 0 [
        set loan-amount 0
        set loan-source ""
      ]
    ]
  ]
end

to seek-bank-and-open-account
  ;; Iterate over all SMEs
  ask smes [
    ;; Check if the SME has a bank account
    if not has-account? [
      ;; Find a bank to open an account with
      let chosen-bank one-of banks  ;; Assuming 'banks' is a list of bank agents

      ;; Open a bank account with the chosen bank
      ask chosen-bank [
        ;; Update the bank to reflect the new account
        set bank-balance bank-balance + capital * savings-percentage  ;; Deposit the savings
        ;; Optionally, update SME's record
        ask myself [
          set has-account true
          set capital capital - capital * savings-percentage  ;; Deduct the deposit from SME’s capital
        ]
      ]
    ]
  ]
end

;large-buisness actions start here
to large-businesses-actions
  large-businesses-receive-revenue
  large-business-seek-loans bank-sets
  spend-capital
  lb-repay-loans
  lb-seek-bank-and-open-account
end

to large-businesses-receive-revenue
  ;; SMEs collect the money spent by households
  ask large-businesses [
    ;; Add the generated revenue to the SME's capital
    set capital capital + revenue-generated
    ;; Reset revenue-generated for the next cycle
    set revenue-generated 0
  ]
end

to large-business-seek-loans [bank-set]
  ;; Large businesses decide whether to apply for a loan based on their capital needs and sector demand.
  ask large-businesses [
    ;; Define large business-specific variables
    let loan-needed? false
    let loan-offer 0
    let chosen-bank one-of bank-set  ;; Randomly choose a bank from the set
    let desired-capital-level capital * 2  ;; Adjust desired level as needed
    let total-loan-amount 0

    ;; Determine if the business needs a loan
    if capital < desired-capital-level [
      set loan-needed? true
    ]

    ;; Calculate potential loan offer if needed
    if loan-needed? [
      let sector-demand sum [income] of households with [sector-employed = industry-sector]
      set loan-offer sector-demand * 0.03  ;; For example, 3% of sector demand

      ;; Ask the chosen bank for a loan if it meets the criteria
      ask chosen-bank [
        let acceptance-chance random-float 100
        let acceptance-threshold 70  ;; Lower threshold for large businesses

        if (lending-criteria > random 100) and (acceptance-chance < acceptance-threshold) [
          if (total-loan-amount + loan-offer) <= bank-balance [
            set capital capital + loan-offer
            set loan-amount loan-offer
            set loan-source "bank"
            set total-loan-amount total-loan-amount + loan-offer
            set bank-balance bank-balance - loan-offer
          ]
        ]
      ]
    ]
  ]
end

to lb-repay-loans
  ask large-businesses [
    let repayment-amount loan-amount * repayment-rate
    if capital >= repayment-amount [
      set loan-amount loan-amount - repayment-amount
      set capital capital - repayment-amount
      if loan-source = "bank" [
        ask banks [set bank-balance bank-balance + repayment-amount]
      ]
      if loan-source = "government" [
        ask governments [set fiscal-policy fiscal-policy + repayment-amount]
      ]
      if loan-amount <= 0 [
        set loan-amount 0
        set loan-source ""
      ]
    ]
  ]
end

to lb-seek-bank-and-open-account
  ;; Iterate over all large businesses
  ask large-businesses [
    if not has-account? [
      let chosen-bank one-of banks  ;; Assuming 'banks' is a list of bank agents

      ;; Open a bank account with the chosen bank
      ask chosen-bank [
        set bank-balance bank-balance + capital * savings-percentage  ;; Deposit the savings
        ask myself [
          set has-account true
          set capital capital - capital * savings-percentage  ;; Deduct deposit from large business’s capital
        ]
      ]
    ]
  ]
end


to decide-bank-actions
  ask banks [

    ;; Create a list of agentsets: households, SMEs, and large businesses.
    let agentsets (list households smes large-businesses)

    ;; Loop through each agentset (households, SMEs, and large businesses).
    foreach agentsets [agentset ->
      ask agentset [
        ;; Check if the agent has an account with the bank.
        if has-account? [
          ;; Calculate the amount to collect based on a percentage of their savings.
          let amount-to-collect savings * savings-percentage

          ;; Reduce the agent's savings by the amount collected.
          set savings savings - amount-to-collect

          ;; Increase the total amount collected by the bank.
          ask one-of banks [
            set bank-balance bank-balance + amount-to-collect
          ]
        ]
      ]
    ]
    distribute-loans sectors
  ]
end

to distribute-loans [sector]
  ;; Define the interest rate
  set lending-criteria max list 0 (lending-criteria - 5 + (ifelse-value (gdp > previous-gdp) [10] [-10]))

  ;; Update the bank's balance with total collected
  set bank-balance bank-balance
  ;update-wealth-and-animations

  ;; Define the maximum percentage of the bank's balance that can be spent on loans.
  let max-loan-spend-percentage 0.4 ;; For example, 40%
  let max-loan-spend bank-balance * max-loan-spend-percentage
  let total-loan-amount 0

  ;; Loop through each sector in the sector-lending list to distribute loans.
  foreach sector-lending [sector-pair ->
    let sector-name item 0 sector-pair  ;; Get the sector name from the sector-pair.
    let lending-share item 1 sector-pair  ;; Get the lending share for the sector.

    ;; Calculate the total sector demand by summing the income of households employed in the sector.
    let sector-demand sum [income] of households with [sector-employed = sector-name]

    ;; Large businesses in the sector receive loans.
    ask large-businesses with [member? sector-name sectors] [
      ;; Determine if the business accepts the loan offer
      let acceptance-chance random-float 100  ;; Generate a random float between 0 and 100
      let acceptance-threshold 70  ;; Set a threshold for acceptance (e.g., 70%)

      ;; Check if the business accepts the loan
      if acceptance-chance < acceptance-threshold [
        ;; Calculate the loan amount based on the lending share and sector demand.
        set loan-amount (lending-share / 1000) * sector-demand
        let repayable-amount loan-amount * (1 + lending-criteria) ;; Calculate repayable amount with interest

        ;; Ensure that the total loan amount does not exceed the allowed spending.
        if (total-loan-amount + loan-amount) <= max-loan-spend [
          ;; Increase the business's capital by the loan amount.
          ask myself [
            set capital capital + loan-amount
            set repayable-amount repayable-amount
          ]
          set total-loan-amount total-loan-amount + loan-amount
        ]
      ]
    ]

    ;; SMEs in the sector receive loans if they meet the lending criteria.
    ask smes with [member? sector-name industry-sector] [
      ;; Determine if the SME accepts the loan offer based on lending criteria
      let acceptance-chance random-float 100  ;; Generate a random float between 0 and 100
      let acceptance-threshold 75  ;; Set a threshold for acceptance (e.g., 75%)

      ;; Check if the SME meets the lending criteria and accepts the loan
      if (lending-criteria > random 100) and (acceptance-chance < acceptance-threshold) [
        ;; Calculate the loan amount based on the lending share and sector demand.
        set loan-amount (lending-share / 1000) * sector-demand
        let repayable-amount loan-amount * (1 + lending-criteria) ;; Calculate repayable amount with interest

        ;; Ensure that the total loan amount does not exceed the allowed spending.
        if (total-loan-amount + loan-amount) <= max-loan-spend [
          ;; Increase the SME's capital by the loan amount.
          ask myself [
            set capital capital + loan-amount
            set repayable-amount repayable-amount
          ]
          set total-loan-amount total-loan-amount + loan-amount
        ]
      ]
    ]
  ]

  ;; Update the bank's balance after distributing loans.
  set bank-balance bank-balance - total-loan-amount
  ;update-wealth-and-animations
end

to-report has-account?
  report (bank-account? self)
end

;; Define a procedure to determine if an agent has a bank account
to-report bank-account? [agent]
  ;; Assuming each agent has an attribute `has-account` which is true if they have an account
  report [has-account] of agent
end

; overall economic variables GDP=C+I+G where: C = Consumption I = Investment G = Government Spending
to-report calculate-gdp
;; Store the previous GDP value
  set previous-gdp gdp

  ;to calculate-gdp
  let total-consumption sum [consumption] of households
  let total-investment sum [investment] of smes + sum[investment] of large-businesses
  let total-government-spending sum [fiscal-policy] of governments

  set gdp total-consumption + total-investment + total-government-spending

  ;; Check if GDP has increased
  if gdp > previous-gdp [
    let growth-factor 1.03

    ;; Adjust growth factor if GDP has increased significantly
    if gdp > (previous-gdp * 10) [
      set growth-factor 1.01
    ]

    ;; Update household income based on the growth factor
    ask households [
      let new-income income * growth-factor
      if new-income > 1000000 [
        set new-income 1000000
      ]
      set income new-income
    ]
  ]
  report gdp
end


to-report calculate-lending-share [year]
  ;; Clear previous sector-lending data
  set sector-lending []

  ;; Initialize total GDP for the specified year
  let total-gdp 0

  foreach sectors [sector-name ->
    let sector-data table:get sector-gdp sector-name
    foreach sector-data [data-point ->  ; Correctly define the variable here
      if item 0 data-point = year1 [
        set total-gdp total-gdp + item 1 data-point
      ]
    ]
    report total-gdp
  ]

  ;; Optionally, you can print the total GDP for debugging
  ;print (word "Total GDP for year " year ": " total-gdp)

  foreach sectors [sector-name ->
    let sector-data table:get sector-gdp sector-name
    ;; Find GDP for the year of interest
    let gdp-in-year 0  ;; Initialize GDP for the year to 0
    foreach sector-data [data-point ->
      if item 0 data-point = year1 [
        set gdp-in-year item 1 data-point  ;; Get the GDP value for the specified year
      ]
    ]

    ;; Calculate lending share if total GDP is not zero
    let lending-share ifelse-value (total-gdp != 0) [gdp-in-year / total-gdp] [0]

    ;; Append the sector name and lending share to sector-lending
    set sector-lending lput (list sector-name lending-share) sector-lending
  ]

end


;;genetic algorithm code starts here
to initialize-population
  set population []  ;; Initialize an empty list for the population
  repeat 100 [
    let individual table:make
    table:put individual "government-spending" (random-float 1000000000)
    table:put individual "consumption" (random-float 1000000)
    table:put individual "investment" (random-float 5000000)
    set population lput individual population
  ]
end

to mutate
  foreach population [individual ->
    if random-float 1 < mutation-rate [
      ;; Mutate government spending
      let gov-spending (table:get individual "government-spending")
      table:put individual "government-spending" max list 0 (gov-spending + random-normal 0 10000)

      ;; Mutate consumption
      let cons (table:get individual "consumption")
      table:put individual "consumption" max list 0 (cons + random-normal 0 0.015)

      ;; Mutate investment
      let invest (table:get individual "investment")
      table:put individual "investment" max list 0 (invest + random-normal 0 0.020)
    ]
  ]
end

to evaluate-population
  let fitnesses-table table:make  ;; Create an empty table to store fitnesses

  ;; Loop through each individual in the population
  foreach population [ individual ->
    ;; Access individual parameters from the table
    let government-spending table:get individual "government-spending"
    let new-consumption-level table:get individual "consumption"
    let new-investment-level table:get individual "investment"

    ;; Calculate fitness using a custom fitness function
    let fitness run-fitness-function government-spending consumption investment

    ;; Store the calculated fitness in the fitnesses-table with the individual as the key
    table:put fitnesses-table individual fitness
  ]

  ;; Determine the best solution
  let max-fitness 0
  let best-individual nobody

  ;; Iterate through the table to find the max fitness
  foreach table:keys fitnesses-table [individual ->
    let fitness table:get fitnesses-table individual
    if fitness > max-fitness [
      set max-fitness fitness
      set best-individual individual
    ]
  ]

  set best-fitness max-fitness  ;; Sets the best fitness to the maximum fitness
  set best-solution best-individual  ;; Sets the best solution to the individual with the maximum fitness
end

to select-parents
  let selected []  ;; Initializes an empty list to store the selected individuals

  ;; Loop through each individual in the population
  foreach population [ individual ->
    ;; Access individual parameters from the table
    let government-spending table:get individual "government-spending"
    let new-consumption table:get individual "consumption"
    let new-investment table:get individual "investment"

    ;; Calculate fitness using a custom fitness function
    let fitness run-fitness-function government-spending consumption investment

    ;; Selection logic
    ifelse fitness > best-fitness / 2 [
      set selected lput individual selected  ;; Adds the individual to the selected list if fitness is greater than half the best fitness
    ] [
      if random-float 1 < 0.2 [ set selected lput individual selected ]  ;; Increased selection chance for lower fitness
    ]
  ]

  ;; If there are any selected individuals, update the population
  if length selected > 0 [
    set population selected  ;; Sets the population to the selected individuals
  ]
end

to crossover
  let new-population []  ;; Initialize an empty list for the new population
  let target-size length population  ;; Dynamically determine the population size

  while [length new-population < target-size] [
    let parent1 one-of population  ;; Select the first parent
    let parent2 one-of population  ;; Select the second parent

    if parent1 != parent2 [  ;; Ensure the parents are not the same
      ;; Create offspring by combining genes from both parents
      let offspring1 table:make
      table:put offspring1 "government-spending" table:get parent1 "government-spending"
      table:put offspring1 "consumption" table:get parent1 "consumption"
      table:put offspring1 "investment" table:get parent1 "investment"
      set new-population lput offspring1 new-population  ;; Add offspring1 to the new population

      let offspring2 table:make
      table:put offspring2 "government-spending" table:get parent2 "government-spending"
      table:put offspring2 "consumption" table:get parent2 "consumption"
      table:put offspring2 "investment" table:get parent2 "investment"
      set new-population lput offspring2 new-population  ;; Add offspring2 to the new population
    ]
  ]

  set population new-population  ;; Replace the old population with the new one
end

to-report run-fitness-function [government-spending consumptions investments]
  ask one-of governments [
    set fiscal-policy government-spending
  ]
  ask one-of households [
    set consumption consumption
    ]
  ask one-of smes [
      set investment investment
    ]
  ask one-of large-businesses[
      set investment investment
    ]

  ;; Simulate a step and calculate the GDP
  let newgdp calculate-gdp

;;Penalize fitness for high inflation with adaptive penalty
  let penalty 0
  if inflation-rate > 5 [
    set penalty penalty + ((inflation-rate - 5) * 200)  ; Scaled penalty based on inflation level
  ]
  report (newgdp - penalty)
end

to evolve
  evaluate-population
  select-parents
  crossover
  mutate

  ;; Apply the best fiscal and monetary policies found
  ask governments [
    set fiscal-policy item 0 best-solution
  ]

  ask households [
    set consumption item 1 best-solution
  ]

  ask smes [
    set investment item 2 best-solution
  ]

  ask large-businesses [
    set investment item 2 best-solution
  ]

  ;; Initialize best-fitness-history if it hasn't been already
  if best-fitness-history = 0 or best-fitness-history = [] [
    set best-fitness-history []
  ]

  ;; Track best fitness
  set best-fitness-history lput best-fitness best-fitness-history
end

to set-plots
  set best-fitness-history []  ; Initialize the history of best fitness values
  clear-all-plots  ; Clear any existing plots
  setup-plot  ; Set up the plot to track GA convergence
end

to setup-plot
  set-current-plot "GA Convergence"  ; Create a plot for GA convergence
  set-current-plot-pen "Best Fitness"  ; Create a pen to draw the best fitness
  set-plot-x-range 0 num-generations  ; X-axis for generations
  set-plot-y-range 0 1  ; Y-axis for fitness (normalized)
end

to plot-best-fitness
  set-current-plot "GA Convergence"
  plot best-fitness  ; Plot the best fitness for the current generation
end


; create a procedure that keeps track of governement spending and theiving in which sectors
; do the same thing for each agent and there sector
; how much they spend which sector and by which agent



;to update-sector-tax-revenue
;  ; Example logic to update tax revenue based on transactions
;  ask turtles [
;    let sector-id sector
;    let transaction-tax tax ; Tax amount from a transaction
;    set sector-tax-revenue replace-item sector-id sector-tax-revenue (item sector-id sector-tax-revenue + transaction-tax)
;  ]
;end

;
;to update-sector-expenditures
;  ; Example logic to update expenditures based on agents
;  ask turtles [
;    let sector-id sector
;    let spending amount ; Amount spent by the agent
;    set sector-expenditures replace-item sector-id sector-expenditures (item sector-id sector-expenditures + spending)
;  ]
;end


;animation code starts here
to update-size
  ask turtles [
    let agent-wealth 0
    let scaling-factor 1
    let size-cap 10  ;; Cap maximum size

    if breed = households [
      set agent-wealth income + savings
      set scaling-factor 100
    ]
    if breed = banks [
      set agent-wealth bank-balance
      set scaling-factor 10000000
    ]
    if breed = governments [
      set agent-wealth fiscal-policy
      set scaling-factor 1000000000
    ]
    if breed = smes [
      set agent-wealth revenue-generated
      set scaling-factor 100000
    ]
    if breed = large-businesses [
      set agent-wealth revenue-generated
      set scaling-factor 50000000
    ]

    ;; Set size based on wealth and scaling factor, capped by size-cap
    set size min list (agent-wealth / scaling-factor) size-cap
  ]
end

to animate-effect
  hatch 1 [
    setxy random-xcor random-ycor
    set color green
    set size 0.5
    wait 0.2
    die
  ]
end

to spend-animation
    repeat 10 [ animate-effect ]
    let shrink-factor (calculate-spend-amount * 0.01) ; Ensure this is not zero
    set size size * shrink-factor
end

to apply-animations
  ask turtles [ animate-effect ]
end

to-report calculate-spend-amount
  let amount 0
  if breed = households [ set amount consumption ]
  if breed = banks [ set amount loans ]
  if breed = smes or breed = governments or breed = large-businesses [
    set amount expenditure
  ]
  report amount
end

to-report determine-receive-growth-factor
  let factor 1.1
  if breed = households [
    set factor 1 + (income + savings) / 10000
  ]
  if breed = banks [
    set factor 1 + bank-balance / 100000
  ]
  if breed = governments [
    set factor 1 + fiscal-policy / 100000
  ]
  if breed = smes or breed = large-businesses [
    set factor 1 + revenue-generated / 100000
  ]
  report factor
end

to receive-animation
    animate-receive-effect
    let growth-factor determine-receive-growth-factor
    set size size * growth-factor
    wait 0.3
end

to animate-receive-effect
  hatch 1 [
    setxy random-xcor random-ycor
    set color green
    set size 0.5
    repeat 10 [
      wait 0.1
      set size size * 1.05
    ]
    die
  ]
end

to update-wealth-and-animations
  ask turtles [
    let current-wealth 0
    let scaling-factor 1
    let size-cap  100

    if breed = households [
      set current-wealth income + savings
      set scaling-factor 100
    ]
    if breed = banks [
      set current-wealth bank-balance
      set scaling-factor 10000000
    ]
    if breed = governments [
      set current-wealth fiscal-policy
      set scaling-factor 1000000000
    ]
    if breed = smes [
      set current-wealth revenue-generated
      set scaling-factor 100000
    ]
    if breed = large-businesses [
      set current-wealth revenue-generated
      set scaling-factor 50000000
    ]

    ;; Set size and apply animations based on wealth changes
    set size min list (current-wealth / scaling-factor) size-cap

    if current-wealth < previous-wealth [ spend-animation ]
    if current-wealth > previous-wealth [ receive-animation ]

    set previous-wealth current-wealth
  ]
end

to create-sectors
  let grid-size 52  ; Define the grid size to accommodate at least 52 sectors
  let sector-count 52  ; Total number of sectors
  let label-size 0
  let label-offset-x  0 ; Adjust label position as needed
  let label-offset-y -0
  ;; Define sector names and colors
  set primary-sectors ["Agriculture" "CropProduction" "Livestock" "Forestry" "Fishing"]
  set secondary-sectors ["Industry" "MiningAndQuarrying" "Manufacturing" "OilRefining"
                         "Cement" "FoodBeverageAndTobacco" "TextileApparelAndFootwear"
                         "WoodAndWoodProducts" "PulpPaperAndPaperProducts"
                         "ChemicalAndPharmaceuticalProducts" "NonMetallicProducts"
                         "PlasticAndRubberProducts" "ElectricalAndElectronics"
                         "BasicMetalIronAndSteel" "MotorVehiclesAndAssembly"
                         "OtherManufacturing" "ElectricityGasSteamAndAirCon"
                         "WaterSupplySewageWaste" "Construction"]
  set tertiary-sectors ["Services" "Trade" "AccommodationAndFoodServices"
                        "TransportationAndStorage" "InformationAndCommunication"
                        "FinanceAndInsurance" "RealEstate"
                        "ProfessionalScientificAndTechnicalServices"
                        "AdministrativeAndSupportServicesBusinessServices"
                        "PublicAdministration" "Education"
                        "HumanHealthAndSocialServices" "OtherServices"]

  ;; Flatten the list of sector names
  let sector-names (sentence primary-sectors secondary-sectors tertiary-sectors)
  let sector-colors (map [i -> scale-color red i 0 (length sector-names - 1)] (range (length sector-names)))

  ;; Clear previous labels and colors on patches
  ask patches [
    set pcolor black  ; Reset patch color to black
    set plabel ""     ; Clear any existing labels
  ]

  ;; Create grid of sectors
  let half-grid-size (grid-size / 2)

  ask patches [
    let x-sector max list 0 (floor ((pxcor + half-grid-size) / grid-size))
    let y-sector max list 0 (floor ((pycor + half-grid-size) / grid-size))
    let sector-index (x-sector + y-sector * (grid-size / 2)) mod (length sector-names)  ; Adjust grid indexing

    let sector-name item sector-index sector-names
    let sector-color item sector-index sector-colors
    set pcolor sector-color  ; Set color for the sector
    set plabel sector-name
    set label-size 3  ; Adjust label size as needed
    set label-offset-x -2  ; Adjust label position as needed
    set label-offset-y -2  ; Adjust label position as needed
  ]
end

@#$#@#$#@
GRAPHICS-WINDOW
210
10
932
481
-1
-1
14.0
1
20
1
1
1
0
1
1
1
-25
25
-16
16
1
1
1
ticks
24.0

BUTTON
52
47
116
80
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

@#$#@#$#@
## WHAT IS IT?

(A Model Of The Nigerian Economy that uses Yearly sectoral GDP data and Genetic Algorithms to Optimise Economic Parameters)

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
1
@#$#@#$#@
