#Author: Marco/Tarnus88
#Simulates and visualizes the Monty Hall Problem, using two test-"persons", one person who will always switch and another person who will stay in front of the same door.
#1 = Winning ("Car"), = 0 Losing ("goat").

matrixlaenge=500 #this variable determines how many testcases are generated
Ziegendaten<-matrix(0, nrow=matrixlaenge,ncol=3,dimnames=list(c(1:matrixlaenge),c("A","B","C")))

nichtwechsler<-c(1:matrixlaenge)
wechsler<-c(1:matrixlaenge)

fuellertest=0

#fills matrix randomly with 0 and 1
for (i in 1:matrixlaenge){
	while(fuellertest!=1)
	{
		fueller <- sample(c(0,1),3,replace=TRUE)
		fuellertest<-sum(fueller)
	}
	fuellertest=0
	
	Ziegendaten[i,1:3]<-fueller
}

#Person who never switches
for (i in 1:matrixlaenge) {
	tuerentscheidung<-sample(c(1:3),1)
	if (Ziegendaten[i,tuerentscheidung]==1) {
		nichtwechsler[i]<-1
	}else {
		nichtwechsler[i]<-0
	}
}

#Person who always switches
for (i in 1:matrixlaenge) {
	tuerentscheidung<-sample(c(1:3),1)
	if (Ziegendaten[i,tuerentscheidung]==0) {
		wechsler[i]<-1
	} else {
		wechsler[i]<-0
	}
}
ytitel<-paste("Amount of wins at", matrixlaenge, "passes")	
png(file="montyhallproblem.png", width=800,height=800)
barplot(c(sum(wechsler),sum(nichtwechsler)),names.arg=c("Person who switches every time","Person who stays every time"),ylab=ytitel,col=c("green","blue"))
dev.off()
