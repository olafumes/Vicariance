# Vicariance
R script to simulate partial vicariance of two competing species in an environmental gradient.
The basic assumptions of the simulation are that a linear geographic gradient exists along 100 equidistant localities. The fitness value of the two species is given by values between “0” and “1”. This fitness value is multiplied by the relative abundance of the species to calculate the offspring number. A fitness value of “0” means that no offspring is produced at this locality and “1” is the maximum number. The fitness values for the two species at locality one and locality 100 are specified at the beginning and the values for the intermediate localities are calculated as equidistant steps along the gradient. This adaptive value is multiplied by the relative abundance of the species at each locality to calculate the offspring number. Each generation a certain number of new individuals reach the neighboring localities where their parents lived. The function that describes this behavior is: 1/(10*(d+1)2) (d=distance units). This means that at a distance of “1” the number of new individuals that arrives is 1/40 of the spores that arrive at the original locality, at a distance of “2” 1/90 of the spores of the original locality arrive, and so on. The growth at each locality is given by the logistic formula N1 = rN0*((K-N0)/K) (N1 =number of individuals at time 1, r = growth parameter, N0 = number of individuals one generation before time 1, K = capacity of locality), which results in the typical sigmoid graph. The two species are competing, because the sum of the number of individuals of both species at one locality is given by the capacity at this locality. Or in other words, if the locality is completely occupied, the number of individuals of one species can only increase if the other species decreases in number. At the beginning of the simulation, locality one and locality 100 are each occupied completely by the species with the highest fitness value there, all intermediate localities are empty. The results of the first and last generation are are saved as “svg” files. Additionally, a video in “mp4” format with the sequence of all generations is saved to show the results. Please adapt the paths to the files created by the program to your directories.
The three R scripts are slightly different version of the simulation. The first script calculates the new values for each locality by multiplying the present number of individuals by the fitness value. This behaviour can cause the effect that at certain localities the populations do not reach the full capacity, if the fitness value for both species are to low. The second version accelerates the growth, even if the local fitness is low, in the case that the present number of individuals is below the full capacity by introducing an correction factor. The third script multiplies the r-values by the fitness value. This way a low fitness value does not reduce the number of individuals unless the compiting species has an advantage caused by higher spore arrival or higher fitness value.
