library('testthat')
source('../general/outliers.R', chdir=TRUE)

#to run from command line, use:
#library('testthat')
#test_file("test_outliers.R")

test_that("analytics_helper: outliers", {
	observations = c(-90, 30, 171, 184, 201, 212, 250, 265, 270, 272, 289, 305, 306, 322, 322, 336, 346, 351, 370, 390, 404, 409, 411, 436, 437, 439, 441, 444, 448, 451, 453, 470, 480, 482, 487, 494, 495, 499, 503, 514, 521, 522, 527, 548, 550, 559, 560, 570, 572, 574, 578, 585, 592, 592, 607, 616, 618, 621, 629, 637, 638, 640, 656, 668, 707, 709, 719, 737, 739, 752, 758, 766, 792, 792, 794, 802, 818, 830, 832, 843, 858, 860, 869, 918, 925, 953, 991, 1000, 1005, 1068, 1441)

	thresholds = calculate_outlier_thresholds(observations)
	expect_that(thresholds[['lower']], equals(-48.25, tolerance=0.001))
	expect_that(thresholds[[1]], equals(-48.25, tolerance=0.001))
	expect_that(thresholds[['upper']], equals(1209.85, tolerance=0.001))
	expect_that(thresholds[[2]], equals(1209.85, tolerance=0.001))
	
	# verify we have 2 outliers in data
	expect_that(length(observations), equals(91))
	expect_that(length(which(observations < thresholds['lower'])), equals(1)) # should be 1 outlier below threshold
	expect_that(length(which(observations > thresholds['upper'])), equals(1)) # should be 1 outlier above threshold
	
	no_outliers = remove_outliers(observations)
	# verify we have removed outliers
	expect_that(length(no_outliers), equals(91)) # should be same length, just with 2 NAs
	expect_that(length(which(is.na(no_outliers))), equals(2))
	expect_true(all(no_outliers > thresholds['lower'], na.rm=TRUE)) # ignoring NA's, all data should be above lower and below upper
	expect_true(all(no_outliers < thresholds['upper'], na.rm=TRUE))
})
