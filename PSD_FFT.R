## Calculate Fourier transform of each row
fft <- apply(spks, 1, fft)

# Calculate power spectrum for each time series
psd <- abs(fft)^2/ncol(spks) # normalized by length of acquisition
fs = 0.5 #sampling freq in Hertz; we take a sample every two seconds (1 sample/2 seconds = 0.5)
nyquist <- fs/2

psd <- round(psd, digits = 1)
freq <- seq(0, nyquist, length.out=nrow(psd)) # calculate frequency range
psd.melt <- as.data.frame(psd)
psd.melt$frequency <- freq
psd.melt <- melt(psd.melt, id.vars = "frequency", variable.name = "cell", value.name = "PSD")

ID0147.psd.mean <- summarise(psd.melt, ID0147.mean = mean(PSD), .by = "frequency")


# Plot
ggplot(ID0147.psd.mean, aes(x = frequency, y = `ID0147.mean`))+
  geom_line()


# ggpar(plt, ticks = TRUE, xticks.by = 1)


# plot(freq, psd[,1], type = "l", main = "Power Density Spectrum", xlab = "Hz", ylab = "Power")
#             # loop through each column and add a line to the plot
#             for (i in 2:ncol(psd)) {
#               lines(freq, psd[,i], col = "grey")
#             }
# psd.plt <- recordPlot()

