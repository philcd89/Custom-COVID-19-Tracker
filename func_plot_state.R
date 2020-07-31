plot.state = function(stateAbb, currentCovidData = today_covid_data2, newCaseData = new_case_data, yesterdayDate = yesterday_date_MDY, todayDate = today_date_MDY){
  #browser()
  states <- plot_usmap(include = stateAbb, "states", color = "black", fill = alpha(0.01), size = 1)
  
  counties_total <- plot_usmap(include = stateAbb, data = currentCovidData, values = "cases", color = "black", size = 0.5)
  
  map_total = ggplot() +
    counties_total$layers[[1]] + #counties needs to be on top of states for this to work
    states$layers[[1]] +
    counties_total$theme + 
    coord_equal()+
    scale_fill_viridis(limits = c(0, 3000), oob = squish, breaks = c(0, 1000, 2000, 3000), labels = c("0", "1000", "2000", ">3000"))+
    #scale_fill_gradientn(colors = rainbow(5), limits = c(0, 1000), oob = squish)+
    #scale_fill_gradient(low = "blue", high = "red", limits = c(0, 1000), oob = squish)+
    #scale_fill_gradient(low = "white", high = "purple", limits = c(0, 1000), oob = squish)+
    #scale_fill_gradient2(low = "blue", mid = "green", high = "red", midpoint = 500, limits = c(0, 1000), oob = squish)+
    labs(fill = "Total cases")+
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, size = 12))+
    labs(title = paste("Total Cases:\n", today_date_MDY, sep = ""))
  
  
  # Create USA MAP: NEW CASES
  counties_new <- plot_usmap(include = stateAbb, data = newCaseData, values = "new_cases", color = "black", size = 0.5)
  
  map_new = ggplot() +
    counties_new$layers[[1]] + #counties needs to be on top of states for this to work
    states$layers[[1]] +
    counties_new$theme + 
    coord_equal()+
    scale_fill_viridis(limits = c(0, 25), oob = squish, breaks = c(0, 5, 10, 15, 20, 25), labels = c("0", "5", "10", "15", "20",  ">25"))+
    #scale_fill_gradientn(colors = rainbow(5), limits = c(0, 1000), oob = squish)+
    #scale_fill_gradient(low = "blue", high = "red", limits = c(0, 1000), oob = squish)+
    #scale_fill_gradient(low = "white", high = "purple", limits = c(0, 1000), oob = squish)+
    #scale_fill_gradient2(low = "blue", mid = "green", high = "red", midpoint = 500, limits = c(0, 1000), oob = squish)+
    labs(fill = "New cases (per 100k)")+
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, size = 12))+
    labs(title = paste('New Cases per 100,000 residents:\n', yesterday_date_MDY, "-", today_date_MDY, sep = ""))
  
  
  return(map_total + map_new)
  
}