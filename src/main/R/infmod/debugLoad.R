library(devtools);
document("inftools");

remove.packages("infmod");
install.packages(pkgs = "./infmod", repos = NULL, type = "source");

loadpath <- "./infmod/R";
source(file = paste(loadpath, "RandomVariable.R", sep = "/"));
source(file = paste(loadpath, "ObjectiveFunction.R", sep = "/"));
source(file = paste(loadpath, "Likelihood.R", sep = "/"));
source(file = paste(loadpath, "MCMCSampler.R", sep = "/"));
