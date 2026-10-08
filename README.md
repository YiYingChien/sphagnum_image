sphagnum image raw data
Top_negative_analysis.csv : negative data for top
column : "day1name","day1value","day1log","day1serial","day56name","day56value","day56log","day56serial","ratio","log_ratio"
day1log for log10 value of day1value, serial for line up day1 and day56 data, ratio for day56value/day1value, log_ratio for day56log/day1log
Top_analysis.csv : experimental data for top
column : same as Top_negative_analysis.csv
column meaning same as Top_negative_analysis.csv

side_negative_analysis.csv : negative data for side
column : "day1name","day1value","day1log","day56name","day56value","day56log","type","side","ratio","log_ratio"
day1log for log10 value of day1value, type for strain type, ratio for day56value/day1value, log_ratio for day56log/day1log
side_analysis.csv : experimental data for side
column : same as side_negative_analysis.csv
column meaning same as side_negative_analysis.csv

side_average_negative_analysis.csv : negative data for side, average value of side1, side2, side3, side4
column : "name","type","side1","side2","side3","side4","log_side1","log_side2","log_side3","log_side4","ratio_average","log_ratio_average"
type for strain type, side1 for side1 value, log_side1 for log10 side1 value, 
ratio_average for average of (side1 value + side2 value + side3 value + side4 value) , log_ratio_average for average of (side1 log value + side2 log value + side3 log value + side4 log value) 
side_average_analysis.csv : experimental data for side, average value of side1, side2, side3, side4
column : same as side_average_negative_analysis.csv
column meaning same as side_average_negative_analysis.csv

side_ratio_negative_analysis.csv : negative data for side, summarized value of day1 and day56, ratio of summarized value
column : "name","type","day1average","day56average","ratio","day1log","day56log","log_ratio"
type for strain type, day1average for summarized value of day1, ratio for day56average/day1average, ratio for log10 value of day1average, day1log for  summarized value of log10 side value, 
log_ratio for day56log/day1log
side_ratio_analysis.csv
column : same as side_ratio_negative_analysis.csv
column meaning same as side_ratio_negative_analysis.csv
