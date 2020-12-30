Species_A_Distribution <- vector(mode = "numeric", length = 100) #create vector for species A
Species_B_Distribution <- vector(mode = "numeric", length = 100) #create vector for species B
Species_A_Distribution[1] <- 1.0 #populate locality 1 with species A
Species_B_Distribution[100] <- 1.0 #populate locality 100 with species B
Selection_A1 <- 1.0 # adjust fitness values for extreme localities
Selection_A2 <- 0.9
Selection_B1 <- 0.5
Selection_B2 <- 1.0
r <- 0.1 #adjust growth rate for logistic growth
Selection_Vector_A <- seq(from = Selection_A1, to = Selection_A2, length = 100)#calculate fitness values for intermediate localities
Selection_Vector_B <- seq(from = Selection_B1, to = Selection_B2, length = 100)
rA <- r * Selection_Vector_A #calculate r as function of Fitness
rB <- r * Selection_Vector_B
propagation_A <- vector(length = 100)#create vectors for calculation of dispersed spores
propagation_B <- vector(length = 100)
propagation_A <- Species_A_Distribution #calculate spore production at each locality. Base value of spore production is equal to number of existing plants
propagation_B <- Species_B_Distribution
Dispersion <- vector(mode = "numeric", length = 100) #create vector for disppersion values
for (i in 1:100) Dispersion[i] <- 1/(10*i*i)#calculate actual dispersion values according to formula (may be changed)
Dispersion[1] <- 1.0 #dispersion at same locality set ton 1
Spore_Arrival_A <- matrix(nrow = 100, ncol = 100) #create matrix to calculate speore arrival at each locality from each locality
Spore_Arrival_B <- matrix(nrow = 100, ncol = 100)
Spore_Arrival_A_Sum <- vector(length = 100) #create vector to calculate spore arrival at each locality
Spore_Arrival_B_Sum <- vector(length = 100)
Hyp_Growth_A <- vector(length = 100)# Vector for hypothetical maximal grwoth. Is needed if spore arrival exceeds capacity of locality
Hyp_Growth_B <- vector(length = 100)
Hyp_Growth_Total <- vector_length = 100)
library(av)#video library
library(ggplot2)
df <- data.frame(species = rep(c("B", "A"), each=100),
                locality = rep(1:100,2),
                proportion = c(Species_B_Distribution, Species_A_Distribution),
		    fitness = c(Selection_Vector_B, Selection_Vector_A)) #create dataframe for graphics calculations
picture <- ggplot(data=df, aes(x=locality, y=proportion, fill=species)) +  geom_bar(stat="identity")
picture <- picture + geom_line(aes(y = fitness, group = species, color = species), lwd = 1.30)
picture
ggsave("C:/Elena/simulation_1_0_8_beginning.svg") #save picture of initial situation

makeplot <- function(){ #function for video
	for (m in 1:5000){
		for (i in 1:100){
			for (j in 1:100){#calculate spore arrival art each locality from each locality
				Spore_Arrival_A[i,j] <- Dispersion[abs(i-j)+1]*propagation_A[j]
				Spore_Arrival_B[i,j] <- Dispersion[abs(i-j)+1]*propagation_B[j]
			}
		}#calculations for spore dispersal at each locality
		Spore_Arrival_A_Sum <- apply(Spore_Arrival_A,1, sum)
		Spore_Arrival_B_Sum <- apply(Spore_Arrival_B,1, sum)
		Spore_Arrival_Total <- Spore_Arrival_A_Sum + Spore_Arrival_B_Sum
		Hyp_Growth_A <- Spore_Arrival_A_Sum * (1 + rA)
		Hyp_Growth_B <- Spore_Arrival_B_Sum * (1 + rB)
		Hyp_Growth_Total <- Hyp_Growth_A + Hyp_Growth_B
		Growth_Rate_A <- ifelse(Spore_Arrival_Total > 1, -1, 1 + (rA * Spore_Arrival_Total - Spore_Arrival_Total * Spore_Arrival_Total * rA)) #calculate growth rate for spore _arrival < 1, else put -1
		Growth_Rate_B <- ifelse(Spore_Arrival_Total > 1, -1, 1 + (rB * Spore_Arrival_Total - Spore_Arrival_Total * Spore_Arrival_Total * rB))
		Species_A_Distribution <- ifelse(Spore_Arrival_Total > 1, Hyp_Growth_A/Hyp_Growth_Total, Spore_Arrival_A_Sum * Growth_Rate_A) #calculate new values for each locality, if spore arrival > 1 adjust sum of A and B to 1
		Species_B_Distribution <- ifelse(Spore_Arrival_Total > 1, Hyp_Growth_B/Hyp_Growth_Total, Spore_Arrival_B_Sum * Growth_Rate_B)
		propagation_A <-  Species_A_Distribution 
		propagation_B <-  Species_B_Distribution 

		df$proportion <- c(Species_B_Distribution, Species_A_Distribution)
		picture <- ggplot(data=df, aes(x=locality, y=proportion, fill=species)) +  geom_bar(stat="identity")
		print(picture)
	}	
}

video_file <- file.path("C:/Elena",'simulacion_1_0_8.mp4')
av_capture_graphics(makeplot(), video_file, 1920, 1080, res = 144, framerate=10)
av::av_media_info(video_file)

for (m in 1:5000){ #as above for graphics production
		for (i in 1:100){
			for (j in 1:100){
				Spore_Arrival_A[i,j] <- Dispersion[abs(i-j)+1]*propagation_A[j]
				Spore_Arrival_B[i,j] <- Dispersion[abs(i-j)+1]*propagation_B[j]
			}
		}
		Spore_Arrival_A_Sum <- apply(Spore_Arrival_A,1, sum)
		Spore_Arrival_B_Sum <- apply(Spore_Arrival_B,1, sum)
		Spore_Arrival_Total <- Spore_Arrival_A_Sum + Spore_Arrival_B_Sum
		Hyp_Growth_A <- Spore_Arrival_A_Sum * (1 + rA)
		Hyp_Growth_B <- Spore_Arrival_B_Sum * (1 + rB)
		Hyp_Growth_Total <- Hyp_Growth_A + Hyp_Growth_B
		Growth_Rate_A <- ifelse(Spore_Arrival_Total > 1, -1, 1 + (rA * Spore_Arrival_Total - Spore_Arrival_Total * Spore_Arrival_Total * rA))
		Growth_Rate_B <- ifelse(Spore_Arrival_Total > 1, -1, 1 + (rB * Spore_Arrival_Total - Spore_Arrival_Total * Spore_Arrival_Total * rB))
		Species_A_Distribution <- ifelse(Spore_Arrival_Total > 1, Hyp_Growth_A/Hyp_Growth_Total, Spore_Arrival_A_Sum * Growth_Rate_A)
		Species_B_Distribution <- ifelse(Spore_Arrival_Total > 1, Hyp_Growth_B/Hyp_Growth_Total, Spore_Arrival_B_Sum * Growth_Rate_B)
		propagation_A <-  Species_A_Distribution 
		propagation_B <-  Species_B_Distribution 
	}
df$proportion <- c(Species_B_Distribution, Species_A_Distribution)
picture <- ggplot(data=df, aes(x=locality, y=proportion, fill=species)) +  geom_bar(stat="identity")
print(picture)
ggsave("C:/Elena/simulation_1_0_8_end.svg")
