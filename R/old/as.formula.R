# These are the variable names:
measurevar <- "y"
groupvars  <- c("x1","x2","x3")

# This creates the appropriate string:
paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ ")
# "y ~ x1 + x2 + x3"

# This returns the formula:
as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
# y ~ x1 + x2 + x3