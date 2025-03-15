##THESIS SCRIPTS##

##GETTING DATA##

massdat.a150 <- Import.mass(150, "a")
massdat.a300 <- Import.mass(300, "a")
massdat.c150 <- Import.mass(150, "c")
massdat.c300 <- Import.mass(300, "c")

#Removing a 3% sucrose video from the set
massdat.a300minus <- massdat.a300[-1]               

#Combining into category lists: A quality vs. C quality, 150 ants vs. 300 ants, 20cm vs. 30cm
massdat.a <- c(massdat.a150, massdat.a300minus)
massdat.c <- c(massdat.c150, massdat.c300)

massdat.150 <- c(massdat.a150, massdat.c150)
massdat.300 <- c(massdat.a300minus, massdat.c300)

massdat.20 <- c(massdat.a150[grep("3020", names(massdat.a150))],
                massdat.a300[grep("3020", names(massdat.a300))],
                massdat.c150[grep("3020", names(massdat.c150))],
                massdat.c300[grep("3020", names(massdat.c300))])

massdat.30 <- c(massdat.a150[grep("3030", names(massdat.a150))],
                massdat.a300[grep("3030", names(massdat.a300))],
                massdat.c150[grep("3030", names(massdat.c150))],
                massdat.c300[grep("3030", names(massdat.c300))])
