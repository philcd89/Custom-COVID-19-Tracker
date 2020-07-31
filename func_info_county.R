info.county = function(state_in, county_in, fips, curr_date = today_date, data = cum_covid_data) {
  #browser()
  countyData = filter(data, state == state_in & county == county_in)
  
  # Compute new cases
  countyData$newcases = 0
  for (i in (2:nrow(countyData))) {
    countyData$newcases[i] = countyData$cases[i] - countyData$cases[i-1]
  }
  
  countyData_pop = countypop$pop_2015[which(countypop$fips == fips)]
  
  countyData$newcases100k = countyData$newcases/countyData_pop*100000
  
  # Compute 7-day moving average.  Averages for the first six days will be set to zero
  countyData$SevenDayAvg = 0
  countyData$SevenDayAvg100k = 0
  for (i in (7:nrow(countyData))) {
    countyData$SevenDayAvg[i] = mean(countyData$newcases[(i-6):i])
    countyData$SevenDayAvg100k[i] = mean(countyData$newcases100k[(i-6):i])
  }
  
  countyData_NewCases = countyData$newcases[nrow(countyData)-1] #Need to double check this depending on NYT updates
  countyData_NewCaseAvg = round(countyData$SevenDayAvg[nrow(countyData)-1], 1)
  countyData_NewCaseAvg100k = round(countyData$SevenDayAvg[nrow(countyData)-1]/countypop$pop_2015[which(countypop$fips == fips)]*100000, 1)
  
  if(countyData_NewCaseAvg100k >= 25) {
    risk = "SEVERE"
    color_out = "red"
  } else if (countyData_NewCaseAvg100k >= 10) {
    risk = "HIGH"
    color_out = "orange"
  } else if (countyData_NewCaseAvg100k >= 1) {
    risk = "MODERATE"
    color_out = "yellow"
  } else if (countyData_NewCaseAvg100k < 1) {
    risk = "LOW"
    color_out = "green"
  } else {
    risk = "Undetermined"
    color_out = "gray"
  }
    
  #browser()
  
  county_plot_last30 = ggplot(data = filter(countyData, date > as.Date(curr_date) - 31 & date <= as.Date(curr_date)))+
    scale_x_date(date_labels = "%B %d", breaks = seq(as.Date(curr_date - 30), as.Date(curr_date), by = "1 day"))+
    geom_bar(aes(x = date, y = newcases100k), stat = "identity", color = "black", fill = "#69B1E0")+
    geom_line(aes(x = date, y = SevenDayAvg100k), stat = "identity", color = "#3244A8", size = 2)+
    geom_point(aes(x = date, y = SevenDayAvg100k), stat = "identity", color = "black", fill = "#3244A8", size = 3, shape = 21)+
    ylab("New cases per 100,000 residents")+
    xlab("Date")+
    theme_bw()+
    theme(axis.text = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 60, size = 10, hjust=1),
          plot.title = element_text(hjust = 0.5, size = 20))+
    ggtitle(paste(county_in, " County, MA: New cases past 30 days"))
  
  county_plot_last30 = ggplotly(county_plot_last30)
  
  county_plot_full = ggplot(data = countyData)+
    scale_x_date(date_labels = "%B %d")+
    geom_bar(aes(x = date, y = newcases100k), stat = "identity", color = "black", fill = "#69B1E0")+
    geom_line(aes(x = date, y = SevenDayAvg100k), stat = "identity", color = "#3244A8", size = 2)+
    geom_point(aes(x = date, y = SevenDayAvg100k), stat = "identity", color = "black", fill = "#3244A8", size = 3, shape = 21)+
    ylab("New cases per 100,000 residents")+
    xlab("Date")+
    theme_bw()+
    theme(axis.text = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 60, size = 10, hjust=1),
          plot.title = element_text(hjust = 0.5, size = 20))+
    ggtitle(paste(county_in, " County, MA: since start of pandemic"))
  
  county_plot_full = ggplotly(county_plot_full)
  
  return(list(countyData_NewCases, countyData_NewCaseAvg, countyData_NewCaseAvg100k, county_plot_full, county_plot_last30, risk, color_out))
  
}