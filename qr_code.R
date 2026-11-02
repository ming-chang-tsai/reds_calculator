# install.packages("qrcode")   # run once
library(qrcode)

# Stable GitHub Pages URL. It now serves the browser app directly.
# Keep this address so previously printed QR codes continue working.
pages_url <- "https://ming-chang-tsai.github.io/reds_calculator/"

# Generate QR code
qr <- qr_code(pages_url)

png("reds_calculator_qr.png", width = 1600, height = 1600, res = 300)
plot(qr)
dev.off()
message("QR code saved for: ", pages_url)
