# Team 1: Michael Munje et al.
# Calculates evaluation metrics of multiclassification problems based on confusion matrix

get_class_scores <- function(conf_m, i)
{
	# calculate evaluation metrics for single class
	tp = conf_m[i,i] # a true positive value is the diagonal cell 
	fp = sum(conf_m[,i]) - tp # false positive is sum of column minus true positive cell
	fn = sum(conf_m[i,]) - tp # false negative is sum of row minus true positive cell
	tn = sum(conf_m[,]) - tp - fp - fn # tn is simply the sum of all cells, minus tp, fp, & fn
	return(c(tp, fp, tn, fn))
}

get_average_scores <- function(conf_m) {
	result_m = conf_m
	num_classes = nrow(conf_m)
	for (i in 1:num_classes)
		result_m[i,] = get_class_scores(conf_m, i) # calculates evaluation metrics for each class

	tp_all = result_m[,1] # extract eval. metrics
	fp_all = result_m[,2]
	tn_all = result_m[,3]
	fn_all = result_m[,4]

	accuracy_all = (tp_all + tn_all) / (tp_all + tn_all + fp_all + fn_all) # prepare to get averages
	precision_all = tp_all / (tp_all + fp_all)
	recall_all = tp_all / (tp_all + fn_all)
	f1_all = (2 * precision_all * recall_all) / (precision_all + recall_all)

	accuracy_avg = sum(accuracy_all)/num_classes # get eval. metric averages
	precision_avg = sum(precision_all)/num_classes
	recall_avg = sum(recall_all)/num_classes
	f1_avg = sum(f1_all)/num_classes

	return(cbind(accuracy_avg, precision_avg, recall_avg, f1_avg)) # return eval. metric averages
}

run_model <- function(model) {
	print(model) # prints confusion matrix and prints average evaluation metrics
	print(get_average_scores(model))
	cat("\n")
}

model1 = t(cbind(c(10, 0, 0, 0), c(0, 5, 3, 2), c(0, 1, 8, 1), c(0, 1, 0, 9))) # confusion matrix entered row by row
model2 = t(cbind(c(8, 2, 0, 0), c(1, 7, 0, 2), c(0, 0, 9, 1), c(2, 3, 0, 5))) # here we define each model as a confusion matrix
model3 = t(cbind(c(100, 80, 10, 10), c(0, 9, 0, 1), c(0, 1, 8, 1), c(0, 1, 0, 9)))
model4 = t(cbind(c(198, 2, 0, 0), c(7, 1, 0, 2), c(0, 8, 1, 1), c(2, 3, 4, 1)))

run_model(model1)
run_model(model2)
run_model(model3)
run_model(model4)