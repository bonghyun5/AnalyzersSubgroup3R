# The following code is adapted from the example of th etas package
library(ETAS)

# fitting the ETAS model to an Iranian catalog

plot(iran.quakes) #g1


# specifiying the geographical region
win <- owin(c(41, 66), c(24, 42))

plot(iran.quakes$data[, 2:3])
plot(win, add=TRUE) #g2


# projecting log-lat coordinates into flat map coordinates
proj <- long2flat(iran.quakes, win)
# specifying time period
tperiod <- c(25000, 40329)
# initial parameters values
param01 <- c(0.4339678,
             0.1988628,
             0.0345206,
             1.6290137,
             1.1286776,
             0.0072539,
             2.1705884,
             0.5706402)
 
res <- etas(proj$X, proj$win, tperiod, m0=4.5, param0=param01, no.itr=1)

#g3
#g4



# fitting the ETAS model to a Japanese catalog

plot(jap.quakes)

# specifiying the geographical region
jwin <- owin(poly=list(x=c(134.0, 137.9, 143.1, 144.9, 
                           147.8, 137.8, 137.4, 135.1, 130.6),
                       y=c(31.9, 33.0, 33.2, 35.2, 41.3, 
                           44.2, 40.2, 38.0, 35.4)))

plot(jap.quakes$data[, 2:3])
plot(jwin, add=TRUE) #g5


# projecting log-lat coordinates into flat map coordinates
proj <- long2flat(jap.quakes, jwin)
# specifying time period
tperiod <- c(10000, 23376)
# initial parameters values
param00 <- c(0.592844590,
             0.204288231,
             0.022692883,
             1.495169224,
             1.109752319,
             0.001175925,
             1.860044210,
             1.041549634)

res <- etas(proj$X, proj$win, tperiod, m0=4.5, param0=param00, no.itr=11)

