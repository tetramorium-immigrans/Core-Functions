##THESIS SCRIPTS##

##GETTING DATA##

massdat.a150 <- Import.mass(150, "a30", extended.mass = TRUE)
massdat.a300 <- Import.mass(300, "a30", extended.mass = TRUE)
massdat.c150 <- Import.mass(150, "c30", extended.mass = TRUE)
massdat.c300 <- Import.mass(300, "c30", extended.mass = TRUE)

#Removing a 3% sucrose video from the set
#massdat.a300minus <- massdat.a300[-1]               

#Combining into category lists: A quality vs. C quality, 150 ants vs. 300 ants, 20cm vs. 30cm
massdat.a <- c(massdat.a150, massdat.a300)
massdat.c <- c(massdat.c150, massdat.c300)

massdat.150 <- c(massdat.a150, massdat.c150)
massdat.300 <- c(massdat.a300, massdat.c300)

massdat.20 <- c(massdat.a150[grep("3020", names(massdat.a150))],
                massdat.a300[grep("3020", names(massdat.a300))],
                massdat.c150[grep("3020", names(massdat.c150))],
                massdat.c300[grep("3020", names(massdat.c300))])

massdat.30 <- c(massdat.a150[grep("3030", names(massdat.a150))],
                massdat.a300[grep("3030", names(massdat.a300))],
                massdat.c150[grep("3030", names(massdat.c150))],
                massdat.c300[grep("3030", names(massdat.c300))])



##FUNCTIONS FROM INTERNET##
#Wilcox's Q
#https://jmasm.com/index.php/jmasm/article/view/987
#Code edits: https://www.reddit.com/r/statistics/comments/nur2r1/q_strange_results_calculating_a_robust_cohens_d_q/

#Alternative to Wilcox's Q
#https://aakinshin.net/posts/nonparametric-effect-size/