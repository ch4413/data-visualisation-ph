library(UsingR)
Plotting_years_function <- function(slideYear, Years_chosen = Year_range, Regions_indices = All_regions, Chosen_items = Item_indices){
  
  Regions_chosen <- All_regions[Regions_indices]  #Subsetting the data by sregion for plotting
  Number_of_regions <- length(Regions_chosen)
  
  List_food_totals_years <- list()
  List_GDP_years <- list()
  List_urban_years <- list()
  
  for (r in 1: Number_of_regions){
    
    Region <- (GDP_df[GDP_df$sregion %in% Regions_chosen[r] , ])
    
    Countries_in_region <- unique(Region$country) # All countries in region and years
    Max_GDP = max(GDP_df$gdp)
    
    # All GDP and FAO data for countries in region
    
    Countries_in_region_GDP_df <- subset(GDP_df, country %in% Countries_in_region)
    Countries_in_region_FAO_df <- subset(FAO_df, Country %in% Countries_in_region)
    
    Number_of_Countries_in_region <- length(Countries_in_region)   		#Create an iterative subset
    Number_of_years <- length(Years_chosen)
    
    my_list_GDP <- list()                                          #create an empty list
    
    for (i in 1: Number_of_Countries_in_region){
      New <- Countries_in_region_GDP_df[Countries_in_region_GDP_df$country %in% Countries_in_region[i] , ]        #subset on country name from country_list
      my_list_GDP[[i]] <- New 
      #Add new subset dataframe to the list
    }
    
    my_list_FAO <- list()    
    
    for (i in 1: Number_of_Countries_in_region){
      
      New <- Countries_in_region_FAO_df[Countries_in_region_FAO_df$Country %in% Countries_in_region[i] , ]    #subset on country name from country_list
      my_list_FAO[[i]] <- New                                   #Add new subset dataframe to the list
    }
    list_3 <- list()
    Food_totals <- array()
    Food_totals_years <- matrix(0, Number_of_years, Number_of_Countries_in_region)
    for (j in 1: Number_of_years){
      for (i in 1: Number_of_Countries_in_region){
        A = my_list_FAO[[i]]$Year %in% Years_chosen[j]
        B = which(A %in% TRUE)
        C = my_list_FAO[[i]][B, ]
        D = sum(C$Value[Chosen_items])
        
        list_3[[i]] <- C
        Food_totals[i] <- D
        Food_totals_years[j, i] <- D
      }
    }
    
    List_food_totals_years[[r]] <- Food_totals_years
    
    list_4 <- list()
    GDP_years <- matrix(0, Number_of_years, Number_of_Countries_in_region)
    for (j in 1: Number_of_years){
      for (i in 1: Number_of_Countries_in_region){
        A = my_list_GDP[[i]]$data_year %in% Years_chosen[j]
        B = which(A %in% TRUE)
        C = my_list_GDP[[i]][B, ]
        D = C$gdp
        
        GDP_years[j, i] <- D
      }
    }	
    
    List_GDP_years[[r]] <- GDP_years
    
    list_5 <- list()
    Urban_years <- matrix(0, Number_of_years, Number_of_Countries_in_region)
    for (j in 1:Number_of_years){
      for (i in 1: Number_of_Countries_in_region){
        A = my_list_GDP[[i]]$data_year %in% Years_chosen[j]
        B = which(A %in% TRUE)
        C = my_list_GDP[[i]][B, ]
        D = C$perurb
        
        Urban_years[j, i] <- D
        Urban_years <- Urban_years
      }
    }
    
    List_urban_years[[r]] <- Urban_years
  }
  
  names(Food_totals_years) = Countries_in_region
  names(GDP_years) = Countries_in_region
  
  Number_of_Countries_in_region <- length(Countries_in_region)
  #number of years = 1: 1 instead of 1:years
  a <- 1970:2011
  YEAR <- which(a %in% slideYear)
  
  for(i in YEAR){
    Year <- toString(Years_chosen[i])
    x= seq(0, 89850, 150)
    y = seq(1001, 4600, 6)
    
    plot(x, y, type="n", xlab= "", ylab="")
    palette(rainbow(Number_of_regions))
    for(n in 1:Number_of_regions){
      points(List_GDP_years[[n]][i, ], List_food_totals_years[[n]][i, ], pch=21, cex=5*List_urban_years[[n]][i, ], col=array(n, length(Urban_years[1, ])))
      text(100, 4250, Year, col= "black", pos=4, cex=1.5)
      text(57000, (3000 - n*200), Regions_chosen[n], col=n, pos=4)
    }
    title(xlab="Gross Domestic Product ($ per capita)", ylab="Total Caloric Availability (per capita calories)", main= c("Gross Domestic Product against Total Caloric Availability \n between 1970 and 2011"), cex.lab=1.5, cex.main=1.6, cex.axis=3)
    legend("topright", c("60% population who live in urban areas", "40% population who live in urban areas" ,"20% population who live in urban areas"), pch=21, pt.cex=c(3, 2, 1))
  }
}


shinyServer(
  function(input, output) {
    output$newplot <- renderPlot({
      slideYear <- input$slideYear
      Plotting_years_function(slideYear)
      
    })
  }
)
