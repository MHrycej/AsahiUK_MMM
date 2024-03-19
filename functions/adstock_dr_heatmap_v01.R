


heatmap = function(
  #  Define dataset to be used for the analysis
  dataset, # = taxonomy
  #  Define model's formula WITHOUT ANALYZED CHANNEL
  formula.input, # = "dep_Volume ~ dep_distribution_w + 
  #  Specify required expense channel to analyze
  expense_channel, 
  #  Define list of adstocks
  adstocks, # = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9)
  #  Define diminishing returns
  dr_type, # = "exp" # available variants are 'exp' for negative exponential & 'atan' for inverse of tangens
  dr_divisors, # = c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)
  #  Select which criteria to compare (can be list of multiples)
  criteria # = c("R2", "t-stat",...)
  
){
  
  # We start with outer loop running for each criterion
  for(crit.i in 1:length(criteria)){

    # 1 We create time variable to order everything accordingly
    dataset$time = paste(dataset$Year, dataset$Week, sep = "-") # dataset$Date
    dataset = dataset[order(dataset$time), ]
    
    # 2 We create virtual output matrices to store everything
    criterion = as.data.frame(matrix(nrow = length(adstocks), ncol = length(dr_divisors)))
    rownames(criterion) = adstocks
    colnames(criterion) = dr_divisors
    
    # Output df for ggplot
    gg.criterion = as.data.frame(matrix(
      nrow = length(adstocks) * length(dr_divisors), 
      ncol = 5
    ))
    colnames(gg.criterion) = c("adstock", "dr_divisor", "row", "col", "criterion")
    
    
    # 3 Now we go through double-loop to fill in the matrix
    counter = 1
    for(adstock.i in 1:length(adstocks)){
      for(dr_divisor.i in 1:length(dr_divisors)){
        adstock = adstocks[adstock.i]
        dr_divisor = dr_divisors[dr_divisor.i]

        # We perform expense transformation via our selected adstock & dr_divisor
        sub = dataset
        expense = sub[, c("time", expense_channel)]
        max_expense = max(sub[, c(expense_channel)], na.rm = T)

        # We apply the adstock within loop starting with the beginning of our time axis
        for(time.var in 2:nrow(expense)){
          expense[time.var, 2] = sum(expense[time.var, 2], na.rm = T) + sum(expense[time.var - 1, 2], na.rm = T) * adstock
        }
        
        expense = expense[order(expense$time), ]

        # Now we apply the diminishing returns within loop starting with the beginning of our time axis
        if(dr_type == "exp"){
          expense[, 2] = expense[, 2] # please define exponential diminishing return
        }else if(dr_type == "atan"){
          expense[, 2] = atan(expense[, 2] / (dr_divisor * max_expense))
        }else{

        }

        # Now we merge our new column into original sub-dataset
        sub = sub[, -c(match(expense_channel, colnames(sub)))]
        sub = merge(x = sub, y = expense, by.x = "time", by.y = "time", all.x = T, all.y = T)

        # We put together the formula we need
        formula.final = paste(formula.input, expense_channel, sep = " + ")

        # Now we model and save our datapoint
        model.temp = lm(formula = formula.final, data = sub)

        criterion[adstock.i, dr_divisor.i] = summary(model.temp)$r.squared
        
        # ggplot2 - filling df with outputs
        require(ggplot2)
        
        gg.criterion[counter, 1] = adstock
        gg.criterion[counter, 2] = dr_divisor
        gg.criterion[counter, 3] = match(adstock, adstocks)
        gg.criterion[counter, 4] = match(dr_divisor, dr_divisors)
        if(criteria[crit.i] == "R2"){
          gg.criterion[counter, 5] = round(summary(model.temp)$r.squared * 100, 4)
          print(gg.criterion[counter, 5])
        }else if(criteria[crit.i] == "t-stat"){
          gg.criterion[counter, 5] = round(summary(model.temp)$coefficients[c(expense_channel), 3], 3)
          print(gg.criterion[counter, 5])
        }
        
        counter = counter + 1
        remove(adstock, dr_divisor, sub, expense, time.var, formula.final, model.temp)
      }
    }

    # Actual ggplot
    max_dr = max(dataset[, c(expense_channel)], na.rm = T)
    
    print(
      ggplot(
        data = gg.criterion,
        mapping = aes(
          x = col,
          y = row,
          color = criterion
        )
      ) + geom_rect(
        aes(
          xmin = col - .5,
          xmax = col + .5,
          ymin = row - .5,
          ymax = row + .5,
          fill = criterion
        ),
        linetype = 0
      ) + geom_text(
        aes(
          label = criterion
        ),
        color = "black"
      ) + labs(
        x = paste("DR divisors: ", min(dr_divisors), " - ", max(dr_divisors), " x max(spend)", sep = ""),
        y = paste("Adstocks: ", min(adstocks), " - ", max(adstocks), sep = "")
      ) + coord_cartesian(
        xlim = c(1, length(dr_divisors)),
        ylim = c(1, length(adstocks))
      ) + scale_x_continuous(
        breaks = c(1:length(dr_divisors)),
        labels = dr_divisors * max_dr
      ) + scale_y_continuous(
        breaks = c(1:length(adstocks)),
        labels = adstocks
      ) + ggtitle(
        criteria[crit.i]
      )
    )
    
    remove(criterion, adstock.i, dr_divisor.i, counter, max_dr, gg.criterion)
  }
  
  # return(gg.criterion)
}


# crit.out = heatmap(
#   dataset = import_file,
#   formula.input = "mod_vol_multiples_pna_glass_330ml_10pack ~
#     mod_bp_multiples_pna_glass_330ml_10pack +
#     mod_discount_multiples_pna_glass_330ml_10pack +
#     mod_featdisp_multiples_pna_glass_330ml_10pack +
#     s_christmas +
#     s_spring_bank_holiday +
#     s_good_friday +
#     covid_mobility_residential +
#     events_peroni_uefa_21 +
#     events_peroni_all_racing +
#     events_peroni_fifa_world_cup_22 +
#     c_discount_multiples_stella_artois_btl_284_ml_12_pack +
#     c_discount_multiples_corona_btl_330_ml_24_pack",
#   expense_channel = "m_ooh_peroni_digital_imp",
#   adstocks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9),
#   dr_type = "atan",
#   dr_divisors = c(.4, .5, .6, .7, .8, .9, 1, 1.1, 1.3, 1.5, 1.7, 1.9),
#   criteria = c("t-stat"))






