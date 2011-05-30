# treecm
`treecm` is an R package to compute and plot the centre of mass of the tree itself. 

## Centre of mass
Estimating the coordinates of the centre of mass (barycentre) of a tree is crucial to judge its static stability. The centre of mass of a perfectly static tree lies inside its circumference at its base, that is the x and y coordinates of the barycentre lie inside the pi * r^2 surface where r is the radius of tree base.

The more distant the centre of mass from tree base the higher the constrains the tree poses on the soil through its roots. When concerns about tree stability are raised and  the tree needs to be consolidated a proper cabling system has to be put in place. Knowing in advance the direction the tree would fall in case of breakage at its base is necessary to properly engineer the cabling system.

Nevertheless, the estimate of barycentre position is useful to assess the effects of different pruning schemes as far as tree balance is concerned.

## Slenderness ratio
A simple plot of slenderness ratio of branches is also provided. Too thin/long branches and logs (i.e. whose SRc > 70) are considered risky in the Visual Tree Assessment protocol, where SRc is the authors attempt to apply Mattheck's tree slenderness ratio to branches (details in the package vignette). The plot aids in pointing out which branches exceed the SRc70 rule and need to be pruned.

## Get started 
### Centre of mass

To get started, try:

    install.packages("treecm")
    library(treecm)
    data(treeData)
	vectors  <- treeVectors(treeData)
	CM       <- centreOfMass(vectors)
	plot.vectors(vectors, 
	  CM = CM,
	  main = "A stone pine centre of mass"
	)

<img src="https://github.com/mbask/treecm/raw/master/paper/treecm-ex2.png" alt="A centre of mass plot" title="Plot of CM" />

### Slenderness ratio

To get started, try:

	data(treeData)
	# assign length to branches
	treeData$fieldData$length <- c(10.2, 3.9, 7, 7, 7, 7, 7, 7, 3.95, 7, 7, 3.95, 7, 7, 3.95, 7, 7, 7, 3.95, 7, 7, 3.95, 3.95, 7, 7, 3.00)
	vectors <-treeVectors(treeData)
	SR      <- treeSR(treeData,vectors)
	plot.SR(SR, main = "Branches slenderness ratio", xaxt='n', yaxt = 'n', xlab = "", ylab = "")

<img src="https://github.com/mbask/treecm/raw/master/paper/treecm-exSR.png" alt="A slenderness ratio plot" title="Plot of SR" />
