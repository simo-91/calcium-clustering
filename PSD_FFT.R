# Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Calculate Fourier transform of each row
fft <- apply(spks, 1, fft)

# Calculate power spectrum for each time series
psd <- abs(fft)^2/ncol(spks) # normalized by length of acquisition
fs = 0.5 #sampling freq in Hertz; we take a sample every two seconds (1 sample/2 seconds = 0.5)
nyquist <- fs/2
freq <- seq(0, nyquist, length.out=nrow(psd)) # calculate frequency range

psd.melt$frequency <- freq
psd.melt <- melt(psd, id.vars = "frequency", variable.name = "cell", value.name = "PSD")
psd.mean <- summarise(psd.melt, ave = Mode(PSD), .by = "frequency")
# Plot
ggline(psd.mean, x = "frequency", y = "ave",
       plot_type = "l")


# plot(freq, psd[,1], type = "l", main = "Power Density Spectrum", xlab = "Hz", ylab = "Power")
#             # loop through each column and add a line to the plot
#             for (i in 2:ncol(psd)) {
#               lines(freq, psd[,i], col = "grey")
#             }
# psd.plt <- recordPlot()

# Using Mode as average to count frequency of frequencies in the whole population?
tab <- table(psd)
freq_mode <-as.numeric(names(tab)[which.max(tab)])