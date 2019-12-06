# cabootcrs

Constructs bootstrap elliptical confidence regions for the plotted points in simple and multiple correspondence analysis.
These differ from other approaches in that they are explicitly designed as
confidence regions for the locations of the population points when projected onto the observed sample axes.
The algorithm for multiple CA contains a new method to correct for the well-known 
distortions caused by the diagonal of the Burt matrix.


## Use 

For 2-way data presented in this case as a contingency table, 
perform simple CA and produce plots with confidence regions

theresults <- cabootcrs(thedata)

For p-way data, presented in this case as an n individuals by p variables matrix 
giving category memberships for each individual, 
perform multiple CA and produce plots with confidence regions

theresults <- cabootcrs(thedata,catype="mca")


## Installing

From CRAN, with

install.packages("cabootcrs")









