set datafile separator ","
set terminal pngcairo enhanced font 'Verdana,10'
set output 'regression_plot.png'

# Set titles and labels
set title "2D Data with Linear Regression"
set xlabel "Living area"
set ylabel "Price"

#set xrange [0:10]

# Set grid
set grid

# Load the data
datafile = '../datasets/houseds.csv'
predictions = '../datasets/predictions.csv'

# Choose a fitting function, linear in this case: y = mx + b
# f(x) = m*x + b

# Initial guess for the parameters
#m = 1
#b = 1

# Perform the fit
# fit f(x) datafile via m, b

# Plot the data and the fit
# plot datafile with points pt 7 title 'Data Points', \
#     f(x) with lines lt 2 lw 2 title sprintf("Fit: y = %.2fx + %.2f", m, b)

plot datafile every ::1 using 47:81 with points pointtype 2 title "Data", \
     predictions using 1:2 with points pt 7 title "Homework predictions"
