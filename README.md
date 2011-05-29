# treecm
`treecm` is an R package to compute and plot the centre of mass of the tree itself. 

## Centre of mass
Estimating the coordinates of the centre of mass (barycentre) of a tree is crucial to judge its static stability. The centre of mass of a perfectly static tree lies inside its circumference at its base, that is the x and y coordinates of the barycentre lie inside the pi * r^2 surface where r is the radius of tree base.

The more distant the centre of mass from tree base the higher the constrains the tree poses on the soil through its roots. When concerns about tree stability are raised and  the tree needs to be consolidated a proper cabling system has to be put in place. Knowing in advance the direction the tree would fall in case of breakage at its base is necessary to properly engineer the cabling system.

Nevertheless, the estimate of barycentre position is useful to assess the effects of different pruning schemes as far as tree balance is concerned.

## Coefficient of slenderness
A simple plot of coefficient of slenderness of branches is also provided. Too thin/long branches and logs (i.e. whose CoS > 50) are considered risky in the Visual Tree Assessment protocol. The plot aids in pointing out which branches exceed the CoS50 rule and need to be pruned.

## Get started 

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

<img src="https://github.com/mbask/treecm/blob/master/paper/treecm-ex2.png" alt="A centre of mass plot" title="Plot of CM" />