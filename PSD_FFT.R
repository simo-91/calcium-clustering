```{r Calculate Fourier transform of each row}
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
```

ID0147.psd.mean <- summarise(psd.melt, ID0147.mean = mean(PSD), .by = "frequency")


# Plot
ggplot(ID0147.psd.mean, aes(x = frequency, y = `ID0147.mean`))+
  geom_line()



```{r for RFP cells only}
fft.RFP <- apply(RFP, 1, fft)

# Calculate power spectrum for each time series
psd.RFP <- abs(fft.RFP)^2/ncol(RFP) # normalized by length of acquisition
fs = 0.5 #sampling freq in Hertz; we take a sample every two seconds (1 sample/2 seconds = 0.5)
nyquist <- fs/2

psd.RFP <- round(psd.RFP, digits = 1)
freq <- seq(0, nyquist, length.out=nrow(psd.RFP)) # calculate frequency range
psd.RFP.melt <- as.data.frame(psd.RFP)
psd.RFP.melt$frequency <- freq
psd.RFP.melt <- melt(psd.RFP.melt, id.vars = "frequency", variable.name = "cell", value.name = "PSD")
```

