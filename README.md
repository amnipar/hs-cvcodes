# hs-cvcodes

Haskell computer vision example code for the University of Jyväskylä TIES411
computer vision course; the examples are based on the CV library.

Install the CV library using cabal with
    cabal install c2hs CV
Installing requires the OpenCV library; building it from source is recommended,
but you can try installing it on Ubuntu with
    apt-get install libopencv-dev
OpenCV versions 2.3 and 2.4 are supported at the moment; installing CV may
require specifying which one is installed by using the flags
    cabal install CV -fopencv23
or
    cabal install CV -fopecv24

Some examples require installing the gsl-random library:
    cabal install gsl-random
Some examples require installing the hmatrix library:
    cabal install hmatrix
Installing requires libraries gsl and lapack/blas, on Ubuntu install them with
    apt-get install libgsl0-dev liblapack-dev

Some examples require datasets. They can be downloaded from the following
Dropbox links:

* uwa-faces: University of Western Australia face recognition dataset (selected)
    - <https://www.dropbox.com/s/l61klbzy6u3tq6w/uwa-faces.zip>
* plant: Spectral imaging data for leaves of a real plant and a plastic plant
    - <https://www.dropbox.com/s/rud2k9j9yslu4xo/plant.zip>

Neural network examples require the SCOMA artificial neural network library. Get
it from git@yousource.it.jyu.fi:~amjayee/jyu-machine-learning/mje-scoma-ann.git
and follow the instructions to install. Requires the Atlas library.

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
l07-edges.hs          Edge detection with derivatives.
l07-corners.hs        Corner detection with Harris and Hessian.
l08-clustering.hs     Image segmentation/quantization with k-means clustering.
l08-graph.hs          Image segmentation with attribute graphs.
l08-normcut.hs        Image segmentation with normalized cuts.
l09-scalespaces.hs    Analyzing images in multiple scales.
l10-contours.hs       Extracting and describing component contours.
l10-moments.hs        Describing component shape with image moments.
l10-zernike.hs        Describing component shape with Zernike moments.
l11-knn.hs            K neares neighbors classification.
l11-neuralnet.hs      Neural network classification with MLP.

The following are utility modules containing commonly used functions and types.

File              Description
----------------  --------------------------------------------------------------
BasicUtils.hs     Basic utility functions (conversions etc.)
Clustering.hs     Clustering pixel features with k-means.
DrawingUtils.hs   Utilities for drawing things on images
Filters.hs        Creating filters for 1D and 2D signals.
Fourier.hs        Naive Fourier transform and related operations for 1D and 2D.
Gabor.hs          Gabor functions, for creating Gabor filters.
Gaussian.hs       Gaussian functions, for creating Gaussian filters.
Graph.hs          Utilities for using attributed graphs for image analysis.
Histogram.hs      Accumulating histograms of values, comparing histograms (soon)
Images.hs         Generating various test images.
IOUtils.hs        Utilities for IO, such as reading and writing files.
Moments.hs        Calculating moments for components.
Morphology.hs     Morphological and hit-or-miss operations for Float images.
Neighborhoods.hs  Utilities for handling pixel neighborhoods.
Random.hs         Utilities for creating random vectors and drawing samples.
Shapes.hs         Utilities for generating images of basic shapes.
Signals.hs        Generating and sampling 1D signals.
Thresholding.hs   Thresholding operations for images.
