# -*- coding: utf-8 -*-
"""
Created on Sat Dec  1 19:00:06 2018

@author: tgoulden
"""
import numpy as np
import sys
from matplotlib import pyplot as plt
import h5py, os, copy
import gdal, osr
import rasterio as rio
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rasterio import Affine

def region_shrink(raster,no_data_value):
    size  = raster.shape
    out_raster = np.copy(raster)
    for i in range(2,size[0]-2):
        for j in range(2,size[1]-2):
            temp_array = raster[i-2:i+2,j-2:j+2]
            if no_data_value in temp_array:
                out_raster[i,j]=no_data_value
    return out_raster
			

def plot_raster(band_array,refl_extent,outFile,colorlimit=(0,1),title='',cbar ='on',cmap_title='',colormap='Greys'):

    '''plot_refl_data reads in and plots a single band or 3 stacked bands of a reflectance array
    --------
    Parameters
    --------
        band_array: array of raster values
        refl_extent: extent of reflectance data to be plotted (xMin, xMax, yMin, yMax) 
        colorlimit: optional, range of values to plot (min,max). 
                    - helpful to look at the histogram of reflectance values before plotting to determine colorlimit.
        ax: optional, default = current axis
        title: optional; plot title (string)
        cmap_title: optional; colorbar title 
        colormap: optional (string, see https://matplotlib.org/examples/color/colormaps_reference.html) for list of colormaps
    --------
    Returns 
    --------
        plots flightline array of single band of reflectance data
    --------

    Examples:
    --------
    plot_aop_refl(sercb56,
              sercMetadata['spatial extent'],
              colorlimit=(0,0.3),
              title='SERC Band 56 Reflectance',
              cmap_title='Reflectance',
              colormap='Greys_r') '''
    plt.figure()
    band_array[band_array==-9999] = np.nan	
    plot = plt.imshow(band_array,extent=refl_extent,clim=colorlimit); 
    if cbar == 'on':
        cbar = plt.colorbar(plot,aspect=40); plt.set_cmap(colormap); 
        cbar.set_label(cmap_title,rotation=90,labelpad=20)
    plt.title(title); ax = plt.gca(); 
    ax.ticklabel_format(useOffset=False, style='plain'); #do not use scientific notation for ticklabels
    rotatexlabels = plt.setp(ax.get_xticklabels(),rotation=90); #rotate x tick labels 90 degrees
    plt.savefig(outFile,dpi=300)
    plt.close()
    band_array[np.isnan(band_array)] = -9999		

def resample_raster(inFile,cell_size):
    outFile=inFile[0:-4]+'_resample.tif'
    with rio.open(inFile) as src:
		
        bounds = tuple(src.bounds)
		
        t = src.transform
        
        # rescale the metadata
        transform = Affine(t.a * cell_size/t.a, t.b, np.floor(bounds[0]), t.d, t.e * cell_size/(-1*t.e), np.floor(bounds[3]))
        height = int(src.height /  (cell_size/t.a))
        width = int(src.width / (cell_size/(-1*t.e)))

        profile = src.profile
        profile.update(transform=transform, driver='GTiff', height=height, width=width)

        data = src.read(
            out_shape=(src.count, height, width),
            resampling=Resampling.average,
        )

        with rio.open(outFile, 'w', **profile) as dataset:  # Open as DatasetWriter
            dataset.write(data)
            
    return outFile	

def reproject_raster(inFile,dst_crs):
    
    outFile=inFile[0:-4]+'_reproject.tif'
        #dst_crs = 'EPSG:32613' # CRS for web meractor 

    with rio.open(inFile) as src:
        transform, width, height = calculate_default_transform(
            src.crs, dst_crs, src.width, src.height, *src.bounds)
        kwargs = src.meta.copy()
        kwargs.update({
            'crs': dst_crs,
            'transform': transform,
            'width': width,
            'height': height
        })
    
        with rio.open(outFile, 'w', **kwargs) as dst:
            for i in range(1, src.count + 1):
                reproject(
                    source=rio.band(src, i),
                    destination=rio.band(dst, i),
                    src_transform=src.transform,
                    src_crs=src.crs,
                    dst_transform=transform,
                    dst_crs=dst_crs,
                    resampling=Resampling.nearest)
    
    with rio.open(outFile) as ReprojectedRaster:
        RasterData = ReprojectedRaster.read()
        RasterData[RasterData==0] = 1		
        profile = ReprojectedRaster.profile
        NewOutFile = outFile[0:-4]+'_extents.tif'		
        with rio.open(NewOutFile, 'w', **profile) as dataset:  # Open as DatasetWriter
            dataset.write(RasterData)
        		
			    
    return NewOutFile

def add_nodata_to_tif(input_file,no_data_value):
    DataSet = gdal.Open(input_file, gdal.GA_Update)
    DataSet.GetRasterBand(1).SetNoDataValue(float(no_data_value))
    DataSet = None

def spectral_resampler(Refl,HS_band_centers,HS_FWHM,target_bandpass,file_base):

    
    values_lower = np.where(HS_band_centers>np.min(target_bandpass[0,:]))
    values_upper = np.where(HS_band_centers<np.max(target_bandpass[0,:]))
    values = np.intersect1d(values_lower,values_upper)
    elements = values.shape[0]
        
    target_resample_range = np.arange(np.min(target_bandpass[0,:]),np.max(target_bandpass[0,:]),1)
    target_resample = np.interp(target_resample_range, target_bandpass[0,:], target_bandpass[1,:])

    
    weights = np.zeros_like(values,dtype=np.float32)

    for i in range(elements):
        NEON_guassian_wave_params = np.array([[1,HS_band_centers[values[i]]-target_bandpass[0,0]-(HS_band_centers[values[0]]-target_bandpass[0,0])+2,1.201*HS_FWHM[values[i]]]],dtype = np.float32)
        NEON_gaussian_wave = get_fit_wave(NEON_guassian_wave_params, target_bandpass[0,-1]-target_bandpass[0,0]+1, 1)
        weights[i] = np.convolve(target_resample,NEON_gaussian_wave,mode='valid')
        plt.plot(target_resample_range,NEON_gaussian_wave)
        #plt.savefig(file_base+'_neon_gaussian.png') 
    #plt.plot(HS_band_centers[values],np.flipud(weights)/np.max(weights))
    #plt.plot(target_bandpass[0,0:-1],target_resample/np.max(target_resample))
    #plt.savefig(file_base+'_weights_and_resample.png')    
    return np.divide(np.sum(np.multiply(Refl[:,:,values],np.flipud(weights)),axis=2),np.sum(weights))



def array2raster(newRasterfn,rasterOrigin,pixelWidth,pixelHeight,array,epsg):

    cols = array.shape[1]
    rows = array.shape[0]
    originX = rasterOrigin[0]
    originY = rasterOrigin[1]

    driver = gdal.GetDriverByName('GTiff')
    outRaster = driver.Create(newRasterfn, cols, rows, 1, gdal.GDT_Float32)
    outRaster.SetGeoTransform((originX, pixelWidth, 0, originY, 0, -pixelHeight))
    outband = outRaster.GetRasterBand(1)
    outband.WriteArray(array)
    outRasterSRS = osr.SpatialReference()
    outRasterSRS.ImportFromEPSG(epsg)
    outRaster.SetProjection(outRasterSRS.ExportToWkt())
    outband.FlushCache()

def aop_h5refl2array(refl_filename):
    """aop_h5refl2array reads in a NEON AOP reflectance hdf5 file and returns 
           1. reflectance array (with the no data value and reflectance scale factor applied)
           2. dictionary of metadata including spatial information, and wavelengths of the bands
    --------
    Parameters
        refl_filename -- full or relative path and name of reflectance hdf5 file
    --------
    Returns 
    --------
    reflArray:
        array of reflectance values
    metadata:
        dictionary containing the following metadata:
            bad_band_window1 (tuple)
            bad_band_window2 (tuple)
            bands: # of bands (float)
            data ignore value: value corresponding to no data (float)
            epsg: coordinate system code (float)
            map info: coordinate system, datum & ellipsoid, pixel dimensions, and origin coordinates (string)
            reflectance scale factor: factor by which reflectance is scaled (float)
            wavelength: wavelength values (float)
            wavelength unit: 'm' (string)
    --------
    NOTE: This function applies to the NEON hdf5 format implemented in 2016, and should be used for
    data acquired 2016 and after. Data in earlier NEON hdf5 format (collected prior to 2016) is 
    expected to be re-processed after the 2018 flight season. 
    --------
    Example Execution:
    --------
    sercRefl, sercRefl_metadata = h5refl2array('NEON_D02_SERC_DP3_368000_4306000_reflectance.h5') """

    import h5py

    #Read in reflectance hdf5 file 
    hdf5_file = h5py.File(refl_filename,'r')

    #Get the site name
    file_attrs_string = str(list(hdf5_file.items()))
    file_attrs_string_split = file_attrs_string.split("'")
    sitename = file_attrs_string_split[1]

    #Extract the reflectance & wavelength datasets
    refl = hdf5_file[sitename]['Reflectance']
    reflData = refl['Reflectance_Data']
    reflRaw = refl['Reflectance_Data'].value

    #Create dictionary containing relevant metadata information
    metadata = {}
    metadata['map info'] = refl['Metadata']['Coordinate_System']['Map_Info'].value
    metadata['wavelength'] = refl['Metadata']['Spectral_Data']['Wavelength'].value
    metadata['FWHM'] = refl['Metadata']['Spectral_Data']['FWHM'].value

    #Extract no data value & scale factor
    metadata['data ignore value'] = float(reflData.attrs['Data_Ignore_Value'])
    metadata['reflectance scale factor'] = float(reflData.attrs['Scale_Factor'])
    #metadata['interleave'] = reflData.attrs['Interleave']

    #Apply no data value
    reflClean = reflRaw.astype(float)
    arr_size = reflClean.shape
    if metadata['data ignore value'] in reflRaw:
        #print('% No Data: ',np.round(np.count_nonzero(reflClean==metadata['data ignore value'])*100/(arr_size[0]*arr_size[1]*arr_size[2]),1))
        nodata_ind = np.where(reflClean==metadata['data ignore value'])
        reflClean[nodata_ind]=np.nan 

    #Apply scale factor
    reflArray = reflClean/metadata['reflectance scale factor']

    #Extract spatial extent from attributes
    metadata['spatial extent'] = reflData.attrs['Spatial_Extent_meters']

    #Extract bad band windows
    metadata['bad band window1'] = (refl.attrs['Band_Window_1_Nanometers'])
    metadata['bad band window2'] = (refl.attrs['Band_Window_2_Nanometers'])

    #Extract projection information
    #metadata['projection'] = refl['Metadata']['Coordinate_System']['Proj4'].value
    metadata['epsg'] = int(refl['Metadata']['Coordinate_System']['EPSG Code'].value)

    #Extract map information: spatial extent & resolution (pixel size)
    metadata['Map_Info'] = refl['Metadata']['Coordinate_System']['Map_Info'].value

    hdf5_file.close        

    return reflArray, metadata

def get_fit_wave(params,num_value,num_peak):
    #
    # this callback calculates f(c,x)=exp(-c0*sqr(x0))
    # where x is a position on X-axis and c is adjustable parameter
    
    
    #num_peak = initial_params[0,-1]
    #num_peak = 1
    c = params[0,0:int(num_peak*3)]
       
    x = np.arange(1,num_value)
   
    if num_peak ==1:
        wave = c[0]*np.exp(-((x-c[1])/c[2])**2)
    elif num_peak == 2:
        wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)
    elif num_peak == 3:
        wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)
    elif num_peak == 4:
        wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2) 
    elif num_peak == 5:
        wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2) 
    elif num_peak == 6:
        wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2)+c[15]*np.exp(-((x-c[16])/c[17])**2) 
    elif num_peak == 7:
        wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2)+c[15]*np.exp(-((x-c[16])/c[17])**2)+c[18]*np.exp(-((x-c[19])/c[20])**2) 
        
    return wave 

if __name__ == '__main__':
    
    
    input_NEON_HDF5_file = str(sys.argv[1])

    spectral_response_folder = str(sys.argv[2])
    
    drone_mosaic = str(sys.argv[3])
	
    cell_size = 1
    
    spectral_response_files = [file for file in os.listdir(spectral_response_folder) if file.endswith('.txt')]
    
    print('Reading NEON HDF5 data')
    NEON_AOP_reflectance,NEON_AOP_reflectance_metadata = aop_h5refl2array(input_NEON_HDF5_file)

    print('Started reprojecting raster')   
	
    outReprojectFile=reproject_raster(drone_mosaic,'EPSG:'+str(NEON_AOP_reflectance_metadata['epsg']))
	
    print('Started resampling raster')
	
    outResampleFile=resample_raster(outReprojectFile,cell_size)
	
    ResampledRaster = rio.open(outResampleFile)
    bounds = tuple(ResampledRaster.bounds)
    MapInfo = str(NEON_AOP_reflectance_metadata['Map_Info'])
    MapInfoSplit = MapInfo.split(',')
    startHDF5indexNorth = int((bounds[0])-float(MapInfoSplit[3]))
    startHDF5indexEast = int(float(MapInfoSplit[4])-(bounds[3]))
	
	
    band_index = 1
    total_num_files = len(spectral_response_files)
    band_stack = np.copy(NEON_AOP_reflectance[:,:,0:total_num_files])
    for file in spectral_response_files:
        print('Started spectral resampling and differencing for '+file)
        spectral_response_data = np.loadtxt(spectral_response_folder+'/'+file, delimiter=',')	
    
        NEON_AOP_reflectance_reampled_to_spectral_response = spectral_resampler(NEON_AOP_reflectance,NEON_AOP_reflectance_metadata['wavelength'],NEON_AOP_reflectance_metadata['FWHM'],spectral_response_data,file[0:-4])
  
        NEON_AOP_reflectance_reampled_to_spectral_response = NEON_AOP_reflectance_reampled_to_spectral_response*100
    
        NEON_AOP_reflectance_reampled_to_spectral_response[np.isnan(NEON_AOP_reflectance_reampled_to_spectral_response)] = float(NEON_AOP_reflectance_metadata['data ignore value'])
                              
        array2raster(input_NEON_HDF5_file[0:-3]+'_'+file[0:-4]+'.tif',(NEON_AOP_reflectance_metadata['spatial extent'][0],NEON_AOP_reflectance_metadata['spatial extent'][3]),1,1,(NEON_AOP_reflectance_reampled_to_spectral_response),NEON_AOP_reflectance_metadata['epsg'])

        add_nodata_to_tif(input_NEON_HDF5_file[0:-3]+'_'+file[0:-4]+'.tif',float(NEON_AOP_reflectance_metadata['data ignore value']))
		
        diff_raster = (ResampledRaster.read(band_index))*100-NEON_AOP_reflectance_reampled_to_spectral_response[startHDF5indexEast:startHDF5indexEast+(int(bounds[3]-bounds[1])),startHDF5indexNorth:startHDF5indexNorth+(int(bounds[2]-bounds[0]))]
        
        diff_raster[ResampledRaster.read(band_index)==1] = NEON_AOP_reflectance_metadata['data ignore value']
        
        diff_raster_region_shrink = region_shrink(diff_raster,NEON_AOP_reflectance_metadata['data ignore value'])		
		
        array2raster(input_NEON_HDF5_file[0:-3]+'_'+file[0:-4]+'_difference.tif',(bounds[0],bounds[3]),1,1,diff_raster_region_shrink,NEON_AOP_reflectance_metadata['epsg'])
		
        add_nodata_to_tif(input_NEON_HDF5_file[0:-3]+'_'+file[0:-4]+'_difference.tif',float(NEON_AOP_reflectance_metadata['data ignore value']))
		
        plt.figure()
        plt.hist(diff_raster_region_shrink[diff_raster_region_shrink!= NEON_AOP_reflectance_metadata['data ignore value']].flatten(),1000)
        refl_diff_mean = (np.mean(diff_raster_region_shrink[diff_raster_region_shrink!= NEON_AOP_reflectance_metadata['data ignore value']].flatten()))
        refl_diff_std = (np.std(diff_raster_region_shrink[diff_raster_region_shrink!= NEON_AOP_reflectance_metadata['data ignore value']].flatten()))
        print(refl_diff_mean)
        print(refl_diff_std)
		
        plt.xlim(refl_diff_mean-2*refl_diff_std, refl_diff_mean+2*refl_diff_std)		
        plt.title('Difference histogram of '+file[0:-4])
        plt.xlabel('Reflectance Difference (%)'); plt.ylabel('Frequency')
        plt.savefig(input_NEON_HDF5_file[0:-3]+'_'+file[0:-4]+'_difference_hist.png',dpi=300)
        plot_raster(diff_raster_region_shrink,(bounds[0],bounds[2],bounds[1],bounds[3]),input_NEON_HDF5_file[0:-3]+'_'+file[0:-4]+'_difference_map.png',colorlimit=(refl_diff_mean-2*refl_diff_std, refl_diff_mean+2*refl_diff_std),title='Reflectance Difference',cmap_title='Percent',colormap='seismic')
        plt.close()
        band_stack[:,:,band_index-1] = NEON_AOP_reflectance_reampled_to_spectral_response		
        band_index = band_index+1

    NDVI_NIS = np.divide(band_stack[:,:,4]-band_stack[:,:,2],band_stack[:,:,4]+band_stack[:,:,2])		
    NDVI_drone = np.divide(ResampledRaster.read(5)*(100/255) - ResampledRaster.read(3)*(100/255),ResampledRaster.read(5)*(100/255)+ResampledRaster.read(3)*(100/255))
    array2raster(input_NEON_HDF5_file[0:-3]+'_'+file[0:-4]+'_NDVI_NIS.tif',(NEON_AOP_reflectance_metadata['spatial extent'][0],NEON_AOP_reflectance_metadata['spatial extent'][3]),1,1,NDVI_NIS,NEON_AOP_reflectance_metadata['epsg'])
    array2raster(input_NEON_HDF5_file[0:-3]+'_'+file[0:-4]+'_NDVI_drone.tif',(bounds[0],bounds[3]),1,1,NDVI_drone,NEON_AOP_reflectance_metadata['epsg'])
	
    diff_NDVI_raster_region_shrink = NDVI_drone-NDVI_NIS[startHDF5indexEast:startHDF5indexEast+(int(bounds[3]-bounds[1])),startHDF5indexNorth:startHDF5indexNorth+(int(bounds[2]-bounds[0]))]
    diff_NDVI_raster_region_shrink[diff_raster_region_shrink==NEON_AOP_reflectance_metadata['data ignore value']] = NEON_AOP_reflectance_metadata['data ignore value']
    
    array2raster(input_NEON_HDF5_file[0:-3]+'_'+file[0:-4]+'_NDVI_difference.tif',(bounds[0],bounds[3]),1,1,diff_NDVI_raster_region_shrink,NEON_AOP_reflectance_metadata['epsg'])
    add_nodata_to_tif(input_NEON_HDF5_file[0:-3]+'_'+file[0:-4]+'_NDVI_difference.tif',float(NEON_AOP_reflectance_metadata['data ignore value']))
	
    plt.figure()
    plt.hist(diff_NDVI_raster_region_shrink[diff_NDVI_raster_region_shrink!= NEON_AOP_reflectance_metadata['data ignore value']].flatten(),1000)
    mean_NDVI_diff = np.mean(diff_NDVI_raster_region_shrink[diff_NDVI_raster_region_shrink!= NEON_AOP_reflectance_metadata['data ignore value']].flatten())
    std_NDVI_diff = np.std(diff_NDVI_raster_region_shrink[diff_NDVI_raster_region_shrink!= NEON_AOP_reflectance_metadata['data ignore value']].flatten())
    plt.xlim(mean_NDVI_diff-2*std_NDVI_diff,mean_NDVI_diff+2*std_NDVI_diff)		
    plt.title('Difference histogram of NDVI')
    plt.xlabel('NDVI Difference'); plt.ylabel('Frequency')
    plt.savefig(input_NEON_HDF5_file[0:-3]+'_NDVI_difference_hist.png',dpi=300)
    plot_raster(diff_NDVI_raster_region_shrink,(bounds[0],bounds[2],bounds[1],bounds[3]),input_NEON_HDF5_file[0:-3]+'_NDVI_difference_map.png',colorlimit=(mean_NDVI_diff-2*std_NDVI_diff,mean_NDVI_diff+2*std_NDVI_diff),title='NDVI Difference',cmap_title='Absolute Difference',colormap='rainbow')
    plt.close()
    ResampledRaster.close()
		
        
