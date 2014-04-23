# hs-cvcodes

Haskell computer vision example code for the University of Jyväskylä TIES411
computer vision course; the examples are based on the CV library.

Some examples require datasets. They can be downloaded from the following
Dropbox links:

* uwa-faces: University of Western Australia face recognition dataset (selected)
    - <https://www.dropbox.com/s/l61klbzy6u3tq6w/uwa-faces.zip>
* plant: Spectral imaging data for leaves of a real plant and a plastic plant
    - <https://www.dropbox.com/s/rud2k9j9yslu4xo/plant.zip>

The following are example codes and programs for individual lectures, presented
in the order they appear on the lectures:

File                  Description
--------------------  ----------------------------------------------------------
l01-signals.hs        Observing 1D signals and their frequency components.
l01-interpolation.hs  Sampling and interpolating signals.
l01-imagemath.hs      Basic mathematical operations on images.
l01-thresholding.hs   Simple thresholding of images.
l02-filtering1d.hs    Filtering of 1D signals using convolution.
l02-filtering2d.hs    Filtering of 2D signals (images) using convolution.
l02-morphology.hs     Morphological operations on binary (Float) images.
l02-hitormiss.hs      Structural analysis with hit-or-miss operations.
l03-fourier1d.hs      Naive Fourier transforms of 1D signals.
l03-fourier2d.hs      Naive Fourier transforms of 2D signals (small images).
l03-dft.hs            Fourier operations on images using FFT.
l03-gabor.hs          Applying Gabor filters on images.
l04-regionstat.hs     Statistical analysis of image regions.
l04-histogram.hs      Image histograms.
l04-otsu.hs           Image thresholding with Otsu's method.
l04-eigenimages.hs    Simple object identification using PCA and kNN.
l05-spectral.hs       Spectral image analysis.
l07-corners.hs        Corner detection with Harris and Hessian.
l08-normcut.hs        Normalized cuts for image segmentation.

The following are utility modules containing commonly used functions and types.

File              Description
----------------  --------------------------------------------------------------
BasicUtils.hs     Basic utility functions (conversions etc.)
DrawingUtils.hs   Utilities for drawing things on images
Filters.hs        Creating filters for 1D and 2D signals.
Fourier.hs        Naive Fourier transform and related operations for 1D and 2D.
Gabor.hs          Gabor functions, for creating Gabor filters.
Gaussian.hs       Gaussian functions, for creating Gaussian filters.
Histogram.hs      Accumulating histograms of values, comparing histograms (soon)
Images.hs         Generating various test images.
IOUtils.hs        Utilities for IO, such as reading and writing files.
Morphology.hs     Morphological and hit-or-miss operations for Float images.
Random.hs         Utilities for creating random vectors and drawing samples.
Shapes.hs         Utilities for generating images of basic shapes.
Signals.hs        Generating and sampling 1D signals.
Thresholding.hs   Thresholding operations for images.
