### Calculate Habitat Time Series using flow.matrix and wua.matrix
# Run "readUSGSgageAll.r" and "readWUAtableAll.r" before this script...
# output will be "flow.matrix" and "wua.matrix" - lists of 23 matrices

# Initialize results matrix
habitat.matrix <- vector("list", dim(raw.info)[1]) #set up list to store all WUA tables
habitat.P.matrix <- vector("list", dim(raw.info)[1])

## Generate Habitat Time Series
for (m in 1:length(flow.matrix)) {

	# Print status message
	print(paste("Calculating habitat times series for reach ", reachname[m], "...", sep=""))

	# Extract flow time series from flow.matrix
	flow.ts <- flow.matrix[[m]]
	q <- as.numeric(as.vector(flow.ts[,2])) #store just the flows in vector q
	# Extract WUA table from wua.matrix
	wua.table <- wua.matrix[[m]]

	# Initialize individual habitat time series matrix
	habitat.ts <- matrix(0, nrow=length(q), ncol=ncol(wua.table))
	colnames(habitat.ts) <- colnames(wua.table)
	rownames(habitat.ts) <- as.vector(flow.ts[,1])
	habitat.ts[,1] <- as.vector(as.numeric(q))

	for (n in 1:length(q)) {
		currentq <- q[n] #keep track of what flow the loop is on
		
		# Check if currentq is NA
		if (is.na(currentq) == "TRUE") {
			habitat.ts[n,] <- NA
			next
		}
		
		# Check if currentq is beyond range of WUA table
		min.q <- min(wua.table[,1])
		max.q <- max(wua.table[,1])

		if (currentq < min.q) {
			habitat.ts[n,2:dim(habitat.ts)[2]] <- NA
			next
		} else if (currentq > max.q) {
			habitat.ts[n,2:dim(habitat.ts)[2]] <- NA
			next
		}
		
		rowkey <- which.min(abs(as.numeric(wua.table[,1]) - currentq)) #find flow closest to desired q
		
		# Determine the given flows (and row indices) that bound the desired flow
		if ((as.numeric(wua.table[rowkey,1]) - currentq) > 0) {
			uplim.key <- which.min(abs(as.numeric(wua.table[,1]) - currentq)) #uplim.key = rowkey
			lowlim.key <- uplim.key - 1 #lowlim.key = rowkey - 1
		} else if ((as.numeric(wua.table[rowkey,1]) - currentq) < 0) {
			lowlim.key <- which.min(abs(as.numeric(wua.table[,1]) - currentq)) #lowlim.key=rowkey
			uplim.key <- lowlim.key + 1 #uplim.key = rowkey + 1
		} else if ((as.numeric(wua.table[rowkey,1]) - currentq) == 0) {
			lowlim.key <- which.min(abs(as.numeric(wua.table[,1]) - currentq))
			uplim.key <- which.min(abs(as.numeric(wua.table[,1]) - currentq))
		}
		
		# Interpolate (linear) btwn WUA values for bounding flows
		targets <- colnames(wua.table)[-1]
		for (p in 1:length(targets)) {
			if (lowlim.key == uplim.key) {
				habitat.ts[n,p+1] <- wua.table[lowlim.key, p+1]
			} else {
			#linear interpolation: Yq = Ylow + ((Yup-Ylow)*(Xq-Xlow)/(Xup-Xlow))
				Xup <- wua.table[uplim.key, 1]
				Yup <- wua.table[uplim.key, p+1]
				Xlow <- wua.table[lowlim.key, 1]
				Ylow <- wua.table[lowlim.key, p+1]
				habitat.ts[n,p+1] <- Ylow + ((Yup-Ylow)*(currentq-Xlow)/(Xup-Xlow))
			}
		}
	}
	
	# Store current reach's "habitat.ts" matrix in "habitat.matrix"
	habitat.matrix[[m]] <- habitat.ts

	### Calculate Habitat Time Series as % of Maximum WUA
	# Find maximum WUA for each species of current wua.table
	max.wua <- c()
	for (c in 1:dim(wua.table)[2]) {
		max.wua[c] <- max(wua.table[,c], na.rm=TRUE)
	}
	
	# Calculate WUA as the % of Maximum WUA (from WUA table)
	# Initialize individual % habitat time series matrix
	habitat.P.ts <- matrix(0, nrow=length(q), ncol=ncol(wua.table))
	colnames(habitat.P.ts) <- colnames(wua.table)
	rownames(habitat.P.ts) <- as.vector(flow.ts[,1])
	habitat.P.ts[,1] <- as.vector(as.numeric(q))
	
	for (c in 2:dim(wua.table)[2]) {
		max.wua.c <- max.wua[c]
		habitat.P.ts[,c] <- habitat.ts[,c]/max.wua.c
	}
	
	#Store current reach's "habitat.P.ts" matrix in "habitat.P.matrix"
	habitat.P.matrix[[m]] <- habitat.P.ts
}