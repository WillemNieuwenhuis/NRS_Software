#! /usr/bin/env python
# from os import chdir; chdir('D://working'); execfile('pyTIMESAT_v11.py')
'''
pyTIMESAT --> TIMESAT implentation for python 2.5 
AUTHOR: Jose M. Beltran(gemtoolbox@gmail.com)
PURPOSE: Applies a modification of the adaptive Savitzky-Golay filter 
        following the TIMESAT v2.3 implementation with optional upper envelope forcing.
        to the SPOT VEG NDVI 10-day synthesis. 
NOTE:    TIMESAT v2.3. A package for processing time-series of satellite sensor data. 
         Authors: Per Jonsson, Malmo University, Sweden, e-mail per.jonsson@ts.mah.se
         Lars Eklundh, Lund University, Sweden,  e-mail lars.eklundh@nateko.lu.se )

REQUIRES: numpy, GDAL, Gnuplot
WARNING: Only complete yearly stack of images should be used as an input.
        e.g. 10 years, 9 years; but no 9 years with 8 months.

pyTIMESAT version log.
----------------------------------------------------------------------------------------         
Version 3. Changed the dtype of w to float to be float instead of uint8
        - Corrected element by element basic operations like A + B
Version 4. Changed dtype for y to float
        - Original vector y will be kept as it is. Any changes will occur on a copy of it.
        python do not create copies of vectors or matrices, it will create a view of it
        when you assign B=A, B is a view of A, so any changes in B will be reflected on A.
        I needed to have A without change. So I create B=A.copy().
Version 5. Ready to run without the debugging variables.
Version 6. Version 5 BUT DID NOT WORKED so I return to Version 4 and tried again.
        Comments were added and ploting has been disabled.
Version 7. There was a bug in the assignment of wmatrix and w. wmatrix has been removed 
        and w is initialised every time for each column.
Version 8. Will try to reduce processing time.
        yearsInProfile function NOT in USE so it has been removed...
        getPixelProfile function NOT in USE so it has been removed...
        get3DFullProfiles function NOT in USE so it has been removed...
        return spikes in function spike has been removed
        plot capabilities had been removed
        weighting factor removed
        modweight function capabilities has been removed- This one was having the problem with performance 
Version 9. Incorporates the GEMToolBox
Version 10. Version to perform the upper envelope on all the pixels and then write the array.
        Should be used with small 3D arrays.
Version 11. (Nov 03,2008) User input window size array and plotting capabilities by pixel. return spikes enabled.
Version 12. (april 2009) Cleanup code, removed unnecessary stuff, extracted some code from main loop into functions
''' 

import osgeo.gdal as og, osgeo.gdal_array as oga
import numpy as np
import osgeo.gdalconst as ogc
from scipy import r_ as r_
from scipy import c_ as c_
import time
import GEMtoolBox as gem
#import Gnuplot

t0=time.clock()
# ---
def spike(y,w,spikecutoff = 0.5):
	''' 
	A spikecutoff will be used as: spikecutoff * y[y>0].std(). A value of 2 is the normal value
	for TIMESAT. But in TIMESAT they used spikecutoff * y.std(). I suggest to use only the values
	of y>0 to calculate the distance. A value lower than 1 will pick up more spikes.
	'''
	w0 = np.ravel(w.copy()) # Preserving the old weight values. This could be done directly over w.
	spikes = np.zeros(nb)
	ymean = y[y>0].mean()
	ystd = y[y>0].std()
	wmax = w0.max()
	dist = spikecutoff*ystd
	swinmax = int(np.floor(nptperyear/7)) # 5,7,10??????
	leftSlice = slice(nb-swinmax,nb)
	rightSlice = slice(0,swinmax)
	
	wext = np.ravel(r_[w0[leftSlice],w0,w0[rightSlice]])
	yext = np.ravel(r_[y[leftSlice],y,y[rightSlice]])
	# find single spikes and set weights to zero
	for i in range(swinmax,nb+swinmax):
		m1 = i - swinmax
		m2 = i + swinmax + 1
	   
		idx_wext_nonzero = wext[slice(m1,m2)].nonzero()
		index = m1 + idx_wext_nonzero[0]
		med = np.median(yext[index])
		if abs(y[m1] - med) >= dist and ((y[m1] < (float(yext[i-1])+float(yext[i+1]))/2 - dist) or (y[m1] > max(yext[i-1],yext[i+1])+dist)):
			w0[m1] = 0
			spikes [m1] = 1
			pass
		pass
	return w0,spikes
# ---

# ---
def savgol(y,w):
	# Preventing modifications on the source profile
	y_ = y.copy()
	w_ = w.copy()
	''' Adapted code from TIMESAT ''' 
	winmax = win.max()    
	# ---Extend data circularity to improve fitting near the boundary of original data
	t = np.arange(-winmax+1,nb + winmax+1)
	leftSlice = slice(nb-winmax,nb)
	rightSlice = slice(0,winmax)
	# ---
	y_ = r_[y_[leftSlice], y_,y_[rightSlice]]
	yfit = np.ravel(y_) # Need it to convert to 1D to be dimensions-compatible with the yfits
	wfit = r_[w_[leftSlice],w_,w_[rightSlice]]
	#
	dataSlice = slice(winmax, nb + winmax) # general slice which points always to the profile data
	dataRange = range(winmax, nb + winmax)
	#
	nenvi = len(win) # number of fitting windows-nenvi)
	yfits = np.zeros((nb,nenvi))
	for ienvi in range(nenvi):
		yfitstd= np.std(yfit[:]) # Compute standard deviation for fitted function
		for i in dataRange:
			# set fitting window        
			failleft, failright, m1, m2 = getfitwindow(i, ienvi, win, wfit, yfit, yfitstd)
			#
			# Fit the polynomial if enough data values with non-zero weight 
			if failleft ==0 and failright == 0:
				# preparing data slices as to construct the design matrix
				s_wfit = np.ravel(wfit[slice(m1,m2)])
				s_t = t[slice(0,m2-m1)]
				s_y = np.ravel(y_[slice(m1,m2)])
				# Construct the design matrix A and the column matrix b
				A = c_[np.matrix(s_wfit).T,np.matrix(s_wfit*s_t).T,np.matrix(s_wfit*s_t**2).T]
				b = np.matrix(s_wfit*s_y).T
				# Solving linear-squares problem A^TAc = A^Tb
				ATA = (A.T)*A
				ATb = (A.T)*b 
				#
				c = np.linalg.solve(ATA, ATb)
				# Evaluating the fitted function
				yfit[i] = c[0] + c[1]*t[i-m1] + c[2]*t[i-m1]**2
			else:
				s_y = np.ravel(y_[slice(m1,m2)])
				yfit[i] = np.median(s_y)
				pass
			#
			if forceUpperEnvelope == True:	# Kees' suggestion
				# All iterations will be forced to the upper envelope
				if lastIterationLikeTIMESATfit == False: 
					if (yfit[i] < y[i-winmax])and wfit[i]==1: yfit[i] = y[i-winmax]
				# All except the last iteration will be forced to the upper envelope  
				else: 
					if (yfit[i] < y[i-winmax])and wfit[i]==1 and ienvi<win.shape[0]-1: yfit[i] = y[i-winmax]
					pass                       
				pass
				#
				yfits[:,ienvi] = yfit[dataSlice]
		pass
	return yfits
# ---
def handle_spectrum(spectrum, nptperyear, spikecutoff):
	nb = spectrum.shape[0]
	w = np.ones(nb)
	y = spectrum.copy()
	w[y < 2] = 0
	missingdata = 0
	if w[w == 0].shape[0] >= np.floor(3 * nb / 4): missingdata = 1
	#
	for k  in range(0,nb+1-int(np.floor(nptperyear/3))):
		sk = slice(k,int(k+np.floor(nptperyear/3)))
		if abs(w[sk].sum()) == 0: missingdata = 1
		pass
	if missingdata == 0:
		# Identify spikes in the time-series and set the corresponding weights to zero
		ws,outlier = spike(y, w, spikecutoff)
		y1 = savgol(y,ws)
		
	else:
		y1[:,:] = 0 # The profile is set to zero.
		pass
	
	return y1
#----
def getfitwindow(i, ienvi, win, wfit, yfit, yfitstd):
	m1 = i - win[ienvi]
	m2 = i + win[ienvi] + 1
	winmax = win.max()
	# Adapting fitting interval. Large variation use a smaller window.  
	adjustWindow = ((yfit[slice(m1,m2)].max() - yfit[slice(m1,m2)].min()) > 1.2 * 2 * yfitstd)
	if adjustWindow == True:
		m1 = m1 + int(np.floor(win[ienvi])/3) # adjusting the left side with views of m1
		m2 = m2 - int(np.floor(win[ienvi])/3) # adjusting the right side with views of m2
		pass
	# Check so that there are enough points, at least 3 at either side
	# with weights different from zero. If not, extend fitting window
	failleft = 0 
	while  (abs(wfit[slice(m1,i+1)]) > 1e-10).sum() < 3 and failleft ==0:
		m1 = m1 -1
		if m1 < 1:
			failleft = 1
			m1 = 1
			pass
		pass               
	
	failright = 0 
	while (abs(wfit[slice(i,m2)]) > 1e-10).sum() < 3 and failright == 0:
		m2 = m2+1
		if m2 > nb + 2*winmax:
			failright = 1
			m2 = nb + 2*winmax
			pass
		pass
		
	return failleft, failright, m1, m2
#----
'''def plot(pRow,pCol):
    # Plot the pixel profile at (pRow,pCol), the pixel-fitted profile and outliers
    gp.clear()
    currentPixel = gem.getPixelProfile(in_dataset, idx_row = pRow ,idx_col = pCol)
    original = Gnuplot.Data(currentPixel)
    gp.title('Iterations window size: '+str(win)+' '+xtraNotes)
    gp.xlabel('Time (10-day intervals{[1st,11th,21th] of each month}')
    gp.ylabel('Scaled NDVI')
    original.set_option(with_='lines lc rgb "gray" lw 1')
    original.set_option(title = 'original data')
    # --- 
    lastIteration = Gnuplot.Data(holder[:,pRow,pCol])
    lastIteration.set_option(with_='lines lc rgb "red" lw 0.5')
    lastIteration.set_option(title = 'Last iteration in window')
    olier = Gnuplot.Data(outliers[:,pRow,pCol])
    olier.set_option(title = 'Outliers')
    #
    gp.plot(original,lastIteration,olier)
    # ---
    pass
  
# ---
'''
t1=time.clock()
# Opening working image

print 'WARNING:'
print 'Only complete yearly stack of images should be used as an input.'
print 'e.g. 10 years, 9 years, etc...'
print 'do not use stacks like 9 years with 8 months,'
print 'this will affect the circularity of the data'
print ''

workingPath = raw_input('PATH of the source image- WORKING PATH ( D://working//path// ) :')
src_filename = raw_input('SOURCE IMAGE filename ( filename.img ): ')
if len(src_filename) == 0:
	quit()

print 'Opening source image ...'
in_dataset = og.Open(workingPath+src_filename)
nr,nc,nb = gem.getImageDim(in_dataset) # nr -- number of rows, nc -- number of columns, nb = number of bands
print 'Dimensions: '+ str(nr)+' rows, '+str(nc)+' cols, '+ str(nb)+' bands'
# out_dataset requires to be set as a global to avoid opening it multiple times in the loop for saving
out_filename = raw_input('Working with image: '+src_filename+', Give OUTPUT IMAGE filename:')
print 'Creating the image holder for image: '+out_filename+' in: '+workingPath
outfilenamePlusPath = workingPath+out_filename
# ------
spikecutoff = 0.5
win = np.array([1,2,3,4]) # Setting the window values for the adaptive Savitzky-Golay filter
try:
    windows = input('Give the Savitzky-Golay window sizes for each of the fitting steps, default values: [1,2,3,4]')
    win = np.array(windows)                 
except:
    print 'Using default values: [1,2,3,4]'
    pass


forceUpperEnvelope = True  # Kees de Bie Suggestion   
try:
    forceUp = input('Force upper envelope [True or False], default value is [True] "case sensitive": ')
    forceUpperEnvelope = forceUp
except:
    print 'Using default value: True'
    pass

lastIterationLikeTIMESATfit = True # Kees de Bie Suggestion
try:
    lastlike = input('Apply regular TIMESAT fit for the last iteration [True or False], default is [True] "case sensitive": ')
    lastIterationLikeTIMESATfit = lastlike
except:
    print 'Using default value: True' 
    pass


print ''
print 'Initiating  variables ...'

displayEstimatedTime = True
nptperyear = 36 

holder = np.zeros((nb,nr,nc))	# Output array
# ---
t2=time.clock()
print t2-t1, 'seconds so far.'
print ' Here we go ... '

for irow in range(nr):
	rmatrix = gem.getRowProfile(in_dataset, idx_row=irow) # Read the time-series for all pixels in row
	#----
	for icol in range(nc):
		t3=time.clock()
		y = rmatrix[:,icol] # [nb,nc]
		y1 = handle_spectrum(y, nptperyear, spikecutoff)
		if displayEstimatedTime == True:
			t4=time.clock()
			timediff = t4-t3
			estimatedHrs = ((timediff * nr * nc) / 60) / 60
			print 'Elapsed time by cleaning one pixel: ',timediff, ' seconds'                
			print 'Estimated time: ', timediff * nr * nc / 60, ' minutes, or: ', estimatedHrs,' hours, to finish'
			displayEstimatedTime = False 
			pass

		holder[:,irow,icol] = y1[:, win.shape[0] - 1]
		pass

	print 'row : ', irow, ' of: ',nr

	pass

gem.array3DToImage(holder,outfilenamePlusPath,in_dataset)
# ---
# Read the time-series for all pixel in the image


t5=time.clock()
print 'Total looping time: ', (t5-t2)/60, 'minutes, ', (t5-t2)/60/60,' hours.' 
# -----
# -----
if forceUpperEnvelope == True:
    if lastIterationLikeTIMESATfit == True:
        xtraNotes = ' SAVGOL-TIMESAT with all but the last, UENV-forced'
        pass
    else:
        xtraNotes = ' SAVGOL-TIMESAT with forced UENV'
        pass
    pass
else:
    xtraNotes = ' Upper Envelope (UENV) Savitzky-Golay filter (SAVGOL) TIMESAT-based approach'
    pass

#
# Ready to plot
#gp = gem.initGraphics()

# ---------------------------------------------------------
#in_dataset = None
print 'Done.'
print 'use the plot(row,col) to plot the pixel profile'
#plot(0,0)
