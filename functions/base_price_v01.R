#### BASE PRICE FUNCTION                                                        ####

# This function t



base_price = function(
  # This function takes in two arguments:
  # 1/ dataset (dataframe)
  input.dataset, 
  # 2/ substring to be found within col-names of dataset; for those columns, base price will be calculated and included within output dataframe
  price.colname.substring, 
  # 3/ percentage threshold to look for
  threshold, 
  # 4/ plot = t/f
  plot
){
  # hard exemplary values
  # input.dataset = nielsen
  # price.colname.substring = "Price pl"
  # threshold = 0.001
  # plot = TRUE
  
  output.dataset = input.dataset
  
  col.ns = grep(price.colname.substring, colnames(nielsen), ignore.case = TRUE)
  par(mfrow = c(3, 3))
  
  #### 1 Function to create base-price column                                   ####
  new.column.f = function(
    # As an input we take specific column name to transform
    col.name, 
    ascending
  ){
    # We create subset of data, which we then order by time
    sub = input.dataset[, match(c("_Key_Period", "Year", "Week", col.name), colnames(input.dataset))]
    
    if(ascending == TRUE){
      sub = sub[order(sub$Year, sub$Week), ]
    }else{
      sub = sub[order(sub$Year, sub$Week, decreasing = FALSE), ]
    }

    sub$bp = NA
    
    # We use counter of sucessive bp uses (when ap is totally off) to create some decay to catchup with away wandering series
    bp.active.counter = 0
    # Now we go through the data from top-to-bottom (start-to-end) and we fill bp
    
    sub[1, "bp"] = sub[1, col.name]
    
    for(rr in 2:nrow(sub)){
      prev.bp = sub[rr - 1, "bp"]
      prev.ap = sub[rr - 1, col.name]
      act.ap = sub[rr, col.name]
      
      if((is.na(act.ap) == T) & (is.na(prev.bp) == F)){         # if actual price is NA & previous base price is not NA, use previous BP 
        bp.active.counter = bp.active.counter + 1
        out = prev.bp
      }else if((is.na(act.ap) == T) & (is.na(prev.ap) == F)){   # else if actual price is NA & previous actual price is not NA, use previous AP
        bp.active.counter = bp.active.counter + 1
        out = prev.ap
      }else if(is.na(act.ap) == T){                             # rest of cases when actual price is NA: we return NA; we simply do not know
        out = NA
      }else if(is.na(prev.bp) == F){                            # now when actual price exists & previous base price also exists, we calculate deviation
        
        bp.active.counter = bp.active.counter + 1

        if(((prev.bp - act.ap) / prev.bp) >= (threshold * sqrt(bp.active.counter))){
          out = prev.bp
        }else{
          out = act.ap
        }
      }else if(is.na(prev.ap) == F){                            # now when actual price exists, previous base price does not but previous actual price exists, we calculate deviation
        if(((prev.ap - act.ap) / prev.ap) >= threshold){
          out = prev.ap
          bp.active.counter = 1
        }else{
          out = act.ap
          bp.active.counter = 0
        }
      }else if(is.na(act.ap) == F){
        out = act.ap
      }else{
        out = NA
      }
      
      sub[rr, "bp"] = out
      
      remove(prev.bp, prev.ap, act.ap, out)
    }
    
    sub = sub[, c("_Key_Period", "bp")]
    colnames(sub)[2] = gsub(price.colname.substring, "baseprice", col.name)
    
    return(sub)
  }
  
  
  #### 2 Now we apply the function, going through all of our columns to populate, one by one
  for(col.n in 1:length(col.ns)){
    # we let the function run from both start and end; it is defined by 'descending' parameter
    # then we compare, in which cases the value attains higher values
    new.column = new.column.f(colnames(input.dataset)[col.ns[col.n]], TRUE)
    save.colname = colnames(new.column)[2]
    
    start.to.end = new.column.f(colnames(input.dataset)[col.ns[col.n]], TRUE)
    end.to.start = new.column.f(colnames(input.dataset)[col.ns[col.n]], FALSE)
    
    colnames(start.to.end)[2] = "ste"
    colnames(end.to.start)[2] = "ets"
    
    new.column = merge(x = new.column, y = start.to.end, by.x = "_Key_Period", by.y = "_Key_Period", all.x = T, all.y = F)
    new.column = merge(x = new.column, y = end.to.start, by.x = "_Key_Period", by.y = "_Key_Period", all.x = T, all.y = F)
    
    minimizer = function(input){
      return(min(input[3], input[4], na.rm = T))
    }
    
    new.column$final = apply(X = new.column, MARGIN = 1, FUN = minimizer)
    
    new.column = new.column[, c("_Key_Period", "final")]
    new.column[is.infinite(new.column$final) == T, ] <- NA
    colnames(new.column)[2] = save.colname
    

    remove(start.to.end, end.to.start)
    
    output.dataset = merge(x = output.dataset, y = new.column, by.x = c("_Key_Period"), by.y = c("_Key_Period"), all.x = T, all.y = F)
    
    ## Calculate discount
    output.dataset$discount = abs(output.dataset[, ncol(output.dataset)] / output.dataset[, col.ns[col.n]] - 1)
    colnames(output.dataset)[ncol(output.dataset)] = gsub("baseprice", "discount", save.colname)

    output.dataset = output.dataset[order(output.dataset$Year, output.dataset$Week), ]
    
    # Plot if you want
    if(plot == TRUE & (max(output.dataset[, col.ns[col.n]], na.rm = T) > 0) & (max(output.dataset[, col.ns[col.n]], na.rm = T) < 999)){
      ylimvals = c(0, 0.01 + max(output.dataset[, col.ns[col.n]], na.rm = T))
      
      plot(x = output.dataset$`_Key_Period`, y = output.dataset[, col.ns[col.n]], main = save.colname, 
               type = "l", ylim = ylimvals, xlab = "", ylab = "")
      par(new = T)
      plot(x = output.dataset$`_Key_Period`, y = output.dataset[, ncol(output.dataset) - 1], main = "", col = "red", 
               type = "l", ylim = ylimvals, xlab = "", ylab = "")
      par(new = T)
      plot(x = output.dataset$`_Key_Period`, y = output.dataset[, ncol(output.dataset)], main = "", col = "blue", 
           type = "l", ylim = c(0, 1), xlab = "", ylab = "", yaxt = "n")
    }
    
    print(paste(col.n, " out of ", length(col.ns), " runs", sep = ""))
    remove(new.column, save.colname)
  }

  par(mfrow = c(1, 1))
  return(output.dataset)
}








