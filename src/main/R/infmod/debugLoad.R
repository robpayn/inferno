library(devtools);
document("inftools");

remove.packages("infmod");
install.packages(pkgs = "./infmod", repos = NULL, type = "source");

source(file = "./infmod/R/RandomVariable.R");
source(file = "./infmod/R/ObjectiveFunction.R");
source(file = "./infmod/R/Likelihood.R");
source(file = "./infmod/R/MCMCSampler.R");
