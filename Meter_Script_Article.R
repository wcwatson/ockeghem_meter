# William Watson
# Yale University
# Modeling Ockeghemian Meter
# Last edit: 03/04/2017
#
# 	This program constructs a logistic regression that classifies meter as either triple
# or duple (i.e., Circle or Cut-C) in a given corpus (in this case, Ockeghem's masses).
# Data is read from a pre-prepared .csv file with the following format:
#
#     section | triple |    x1  |  x2   |   y1   | final3 | final2
# --------------------------------------------------------------------
#		.	.	.	.	.	.	.	.	.	.	.	.	.	.	.
#       MM_K1 |    1   |   0.5  |   0   |   0.5  |    0   |    0 
#      MM_Chr |    0   |    1   |   0   |    1   |    1   |    1 
#		.	.	.	.	.	.	.	.	.	.	.	.	.	.	.
#
# Section gives an identifying label (MM_K1 = Missa My my, Kyrie I), triple encodes
# whether the section is in circle (1) or cut-c (0), x/y columns record the proportion
# of cadences in a section that fall on the first and second semibreves of a perfect
# breve and the first semibreve of an imperfect breve, as per the identifying labels,
# and final3 and final2 indicate (1/0) whether or not the section ends at the
# beginning of a breve.

# Filenames for import and export
INFILE = "REPLACE WITH ADDRESS OF .csv FILE CONTAINING DATA FOR ANALYSIS"
OUTFILE_COEF = "REPLACE WITH ADDRESS OF .csv FILE TO RECORD COEFFICIENTS"
OUTFILE_PREDICT = "REPLACE WITH ADDRESS OF .csv FILE TO RECORD PREDICTIONS"

# Constant that adjusts relative size of training and test sets
TRAIN_PCT = 0.75

# Constant that adjusts the number of iterations
ITER = 25

# Import dataset
c <- read.csv(INFILE, header = TRUE)

# Initialize data storage structures
coefficients <- vector()
predictions <- vector()

# Set size of training and test sets
trainSize <- floor(TRAIN_PCT * nrow(d))

# To make specific segmentation replicable
# set.seed(1)

# Loop for iterating the model
for(i in 1:ITER)	{

	# Segment data into test and training sets
	trainIndex <- sample(nrow(d), trainSize, replace = FALSE)
	trainSet <- d[trainIndex, ]
	testSet <- d[-trainIndex, ]

	# Build model
	prob_model <- glm(triple ~ x1 + y1, data = trainSet, family = binomial)

	# Validate the model using the test set
	pred <- predict(prob_model, newdata = testSet, type = "response")

	# Add results to data storage structures
	coefIter <- rep.int(i, 3)
	predIter <- rep.int(i, nrow(testSet))
	isTriple <- testSet$triple
	coefficients <- rbind(coefficients, cbind(coefIter, summary(prob_model)$coefficients))
	predictions <- rbind(predictions, cbind(predIter, pred, isTriple))

}

# Write results to .csv
write.csv(coefficients, file=OUTFILE_COEF)
write.csv(predictions, file=OUTFILE_PREDICT)