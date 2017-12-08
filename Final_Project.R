#Nick Schell
#Final Project R Code
#________________________

install.packages("TSP")
library(TSP)
data(USCA312)
USCA312
m_OG<-as.matrix(USCA312)

#____________________________________________________________________________________
#Removing unwanted cities from the list so we can solve this problem faster and it is easier to follow
#Clean the rows first
m <- m_OG[12:241,]; labels(m)     #Atlanta to San Fran   
m <- m[-2:-46,]; labels(m)        #ATL = 1, #chicago = 2
m <- m[-3:-77,]; labels(m)        #ATL = 1, chicago = 2, KC = 3
m <- m[-4:-48,]; labels(m)        #ATL, CHicago, KC, NO
m <- m[-6:-29,]; labels(m)        #ATL, Chicago, KC, NO, NY, Portland (NO and NY are consecutive in matrix so no cities needed to be deleted between them)
m <- m[-7:-40,]; labels(m)        #deletes cities between portland and san fran 

#Now we have desired number of rows and we need to get the same number of columns
#Clean columns
m <- m[,12:241]; labels(m) #Get list from ATL to San Fran 
m <- m[,-2:-46]; labels(m) #deletes rows between ATL and chicago
m <- m[,-3:-77]; labels(m) #deletes rows between chicago and KC
m <- m[,-4:-48]; labels(m) #deletes rows between KC and NO
m <- m[,-6:-29]; labels(m) #(NO and NY are consecutive in matrix so no cities needed to be deleted between them) but this deletes rows between NY and Portland
m <- m[,-7:-40]; labels(m) #deletes cities between portland and san fran 

#Now we have the matrix we want to be able to do this problem using only the desired cities.
NS_Top_6<- as.matrix(m)   #renaming the matrix to my top 6 cities I want to visit

#_________________________________
#To force KC to be my starting city...
atsp <- as.ATSP(NS_Top_6)
kc <- which(labels(atsp) == "Kansas City, MO")
atsp[, kc] <- 0
initial_tour <- solve_TSP(atsp, method = "nn")
print(initial_tour)

#To improve this tour using 2-opt Moves and a cut at kc to create a path
tour <- solve_TSP(atsp, method = "two_opt", control = list(tour = initial_tour))
print(tour)
labels(atsp)

path <- cut_tour(tour, kc, exclude_cut = FALSE)

#_______________________________________
#Package and function for if we want to see a picture of the path we are taking 
install.packages("maps")
install.packages("sp")
install.packages("maptools")
library(maps)
library(sp)
library(maptools)

data("USCA312_map")
plot_path <- function(path){
  plot(as(Top6_coords, "Spatial"), axes = TRUE)
  plot(USCA312_basemap, add = TRUE, col = "gray")
  points(Top6_coords, pch = 3, cex = 0.4, col = "red")
  
  path_line <- SpatialLines(list(Lines(list(Line(Top6_coords[path,])),
                                      ID="1")))
  plot(path_line, add=TRUE, col = "black")
  points(Top6_coords[c(head(path,1),tail(path,1)),], pch = 19,
         col = "black")
}

#try to edit the coords values to get an accurate map
USCA312_coords <- USCA312_coords[12:241,]
Top6_coords <- USCA312_coords[-2:-46,]
Top6_coords <- Top6_coords[-3:-77,]
Top6_coords <- Top6_coords[-4:-48,]
Top6_coords <- Top6_coords[-6:-29,]
Top6_coords <- Top6_coords[-7:-40,]
Top6_coords                               

#now finally we can see the picture of the path and we can print the order of the path
plot_path(path)
labels(path)

