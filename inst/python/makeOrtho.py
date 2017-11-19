#!/usr/bin/env python
# -*- coding: utf-8 -*-
# script version 0.2.1
"""
© Copyright 2017, Chris Reudenbach, Sebastian Richter
makeOrtho.py: 
User may use two arguments to override the default values
goal = ["ortho","dense","filter"]
imgPath = "path_to_the_image_data"

ortho: performs a workflow for a standarized generation of ortho images 
based on low altitude uav imagery over rugged surfaces. it adds
images to a new chunk, align them and optimized iteratively by 
filtering the resulting sparse point cloud. The ortho image is build by  
constructing first a smoothed mesh model and second a DSM. 

dense: performs a workflow for a standarized generation of a dense point 
cloud based on low altitude uav imagery over rugged surfaces. It adds
images to a new chunk, align them and optimized iteratively by 
filtering the resulting sparse point cloud. The dense point cloud is 
is build upon this optimized sparse cloud. 

filter: performs a workflow for a standarized filter procedure for all 
chunks that have to contain a sparse point cloud. They will be optimized
iteratively. 

In the end of all workflows an automated report is generated.
Full documentation is provided at https://github.com/gisma/uavRmp
"""

# It can be run directly in Photoscan or can be added to a batch process.

# NOTE: Preset values are choosen to fit most picture settings but might have to be adjusted for specific situations.

### Used references:
# http://www.agisoft.com/forum/index.php?topic=5536.0
# http://www.agisoft.com/pdf/photoscan_python_api_1_3_2.pdf
# http://www.agisoft.com/forum/index.php?topic=3164.0
# http://www.agisoft.com/forum/index.php?topic=2767.0
# http://www.agisoft.com/forum/index.php?topic=5976.315 
# http://www.agisoft.com/forum/index.php?topic=2236.0
# http://www.agisoft.com/forum/index.php?topic=6287.msg30374#msg30374
# http://www.agisoft.com/forum/index.php?topic=1981.0
# http://www.agisoft.com/forum/index.php?topic=1843.0
# http://www.agisoft.com/forum/index.php?topic=1881.0
# https://github.com/dshean/sfm_tools/blob/master/agisoft_all.py
# http://www.agisoft.com/pdf/photoscan-pro_1_3_en.pdf
# http://www.agisoft.com/pdf/photoscan_python_api_1_3_2.pdf   <- Using Python in Photoscan
# https://dinosaurpalaeo.wordpress.com/2015/10/11/photogrammetry-tutorial-11-how-to-handle-a-project-in-agisoft-photoscan/ <- general information for the interaction between the three used values
# http://www.agisoft.com/forum/index.php?topic=738.0  <- Reprojection Error
# http://www.agisoft.com/forum/index.php?topic=2478.0  <- Reconstruction Uncertainty, Photoscan users explaining how they tested/experimented with values and giving advises in which range the single parameters give reasonable results


### Technical Notes:

# Definitions from the Agisoft Photoscan Manual, Professional Edition (version 1.3)
# Reprojection error
# High  reprojection  error  usually  indicates  poor  localization  accuracy  of  the  corresponding  point
# projections at the point matching step. It is also typical for false matches. Removing such points can
# improve accuracy of the subsequent optimization step.
 
# 
# High  reconstruction  uncertainty  is  typical  for  points,  reconstructed  from  nearby  photos  with  small
# baseline. Such points can noticeably deviate from the object surface, introducing noise in the point
# cloud. While removal of such points should not affect the accuracy of optimization, it may be useful
# to remove them before building geometry in Point Cloud mode or for better visual appearance of the
# point cloud.
 
# Image count
# PhotoScan reconstruct all the points that are visible at least on two photos. However, points that are
# visible only on two photos are likely to be located with poor accuracy. Image count filtering enables
# to remove such unreliable points from the cloud.
 
# Projection Accuracy
# This criterion allows to filter out points which projections were relatively poorer localised due to their
# bigger size.
 


# Calibration parameters list:
# 
# f = Focal length measured in pixels.
# cx, cy = Principal point coordinates, i.e. coordinates of lens optical axis interception with sensor plane in pixels.
# b1, b2 = Affinity and Skew (non-orthogonality) transformation coefficients.
# k1, k2, k3, k4 = Radial distortion coefficients.
# p1, p2, p3, p4 = Tangential distortion coefficients.



import PhotoScan
import os
import sys
#import glob
from os.path import expanduser


# preset parameters
<%=goal%> 
<%=imgPath%> 
<%=projName%>
<%=alignQuality%>
<%=orthoRes%>
<%=preset_RU%>
<%=preset_RE%>
<%=preset_PA%>
<%=loop_RU%>
<%=loop_RE%>
<%=loop_PA%>

if sys.argv[1:]:
    goal = sys.argv[1]
if sys.argv[2:]:
    imgPath = sys.argv[2]

if goal == "ortho":    
	# define short variables
	crs =  PhotoScan.CoordinateSystem("EPSG::32632")
	doc = PhotoScan.app.document
	chunk = doc.addChunk()
	PSPCF = PhotoScan.PointCloud.Filter()
	count = 0
	# creating image list
	image_list = glob.glob(imgPath + "/*.JPG")
	print(image_list)
	# load images
	chunk.addPhotos(image_list)
	# load exif data
	chunk.loadReferenceExif()
	# create project
	doc.save(imgPath + "/" + projName)
	# reopen it
	doc.open(imgPath + "/" + projName)
	chunk = doc.chunk
	# align photos
	chunk.matchPhotos(accuracy=alignQuality, preselection=PhotoScan.ReferencePreselection,keypoint_limit=0, tiepoint_limit=0)
	chunk.alignCameras()
	doc.save()	


	# optimize Point Cloud by setting ReconstructionUncertainty
	# Technical NOTE: the process runs several times as the optimizing of the camera results in points that have higher values again than the threshold value that was used before to limit the Reconstruction Uncertainty. 
	# It was found that the more often this process runs the less points will be deleted in each step so that finally the point cloud has the choosen Reconstruction Uncertainty and keeps it after the cameras are optimized

	while count < loop_RU:

		class ReconstructionUncertainty:

			# select points by Reconstruction Uncertainty
			PSPCF.init(chunk, PhotoScan.PointCloud.Filter.ReconstructionUncertainty)		
			PSPCF.selectPoints(float(preset_RU))
		
			# remove points
			chunk.point_cloud.removeSelectedPoints()
		
			# optimize cameras			
			chunk.optimizeCameras(fit_f=True, fit_cxcy=True, fit_aspect=True, fit_skew=True, fit_k1k2k3=True, fit_p1p2=True, fit_k4=False)
		
		count=count+1  

		continue
	
	count = 0


	# optimize Point Cloud by setting ReprojectionError
	# See technical NOTE for Reconstruction Uncertainty why to repeat this process several times
	
	while count < loop_RE:

		class ReprojectionError:
			# select points by Reprojection Error
			PSPCF.init(chunk, PhotoScan.PointCloud.Filter.ReprojectionError)		
			PSPCF.selectPoints(preset_RE)
			# remove points
			chunk.point_cloud.removeSelectedPoints()
			# optimize cameras			
			chunk.optimizeCameras(fit_f=True, fit_cxcy=True, fit_aspect=True, fit_skew=True, fit_k1k2k3=True, fit_p1p2=True, fit_k4=False)
		count=count+1  
		continue
	count = 0
	
	
	# optimize Point Cloud by setting ProjectionAccuracy
	# [TODO: No improve in the second run?? Then set runs to 1, or remove loop]
	
	while count< loop_PA:

		class ProjectionAccuracy:
			# select points by Projection Accuracy
			PSPCF.init(chunk, PhotoScan.PointCloud.Filter.ProjectionAccuracy)		
			PSPCF.selectPoints(preset_PA)
			# remove points
			chunk.point_cloud.removeSelectedPoints()
			# optimize cameras			
			chunk.optimizeCameras(fit_f=True, fit_cxcy=True, fit_aspect=True, fit_skew=True, fit_k1k2k3=True, fit_p1p2=True, fit_k4=False)
		count=count+1  
		continue
	count = 0
	
	# MESH model
	chunk.buildModel(surface=PhotoScan.SurfaceType.HeightField, source = PhotoScan.DataSource.PointCloudData, interpolation = PhotoScan.Interpolation.EnabledInterpolation, face_count = PhotoScan.FaceCount.LowFaceCount)
	# save project before orthomosaicing
	doc.save()
	#build DEM
	chunk.buildDem(source = PhotoScan.DataSource.ModelData, interpolation=PhotoScan.EnabledInterpolation)
		
	# GENERATE ORTHOMOSAIC
	chunk.buildOrthomosaic(surface=PhotoScan.ElevationData,projection=crs,dx=orthoRes,dy=orthoRes)
	doc.save()

	# create Processing Report path 
	file_path = doc.path
	file_path_cut = file_path[0:-4]		
	report_path = (file_path_cut + "_reports")

	if not os.path.exists(report_path):
		os.makedirs(report_path)
			
	# export Processing Report
	chunk.exportReport(report_path + "/" + chunk.label + ".pdf")
		
	print("sparse point clouds optimized by setting:\n Reconstruction Uncertainty = " + "{:.0f}".format(preset_RU) + " >> " + "{:.0f}".format(loop_RU) + " loop(s)\n"+
                                          "         Reprojection Error = " + "{:.0f}".format(preset_RE) + " >> " + "{:.0f}".format(loop_RE) + " loop(s)\n"+ 
                                          "        Projection Accuracy = " + "{:.0f}".format(preset_PA) + " >> " + "{:.0f}".format(loop_PA) + " loop(s)\n")	
	print("Processing Reports were created and saved to " + report_path)
	
if goal == "dense":    
	# define short variables
	crs =  PhotoScan.CoordinateSystem("EPSG::32632")
	doc = PhotoScan.app.document
	chunk = doc.addChunk()
	PSPCF = PhotoScan.PointCloud.Filter()
	count = 0
	# creating image list
	image_list = glob.glob(imgPath + "/*.JPG")
	print(image_list)
	# load images
	chunk.addPhotos(image_list)
	# load exif data
	chunk.loadReferenceExif()
	# create project
	doc.save(imgPath + "/" + projName)
	# reopen it
	doc.open(imgPath + "/" + projName)
	chunk = doc.chunk
	# align photos
	chunk.matchPhotos(accuracy=alignQuality, preselection=PhotoScan.ReferencePreselection,keypoint_limit=0, tiepoint_limit=0)
	chunk.alignCameras()
	doc.save()	


	# optimize Point Cloud by setting ReconstructionUncertainty
	# Technical NOTE: the process runs several times as the optimizing of the camera results in points that have higher values again than the threshold value that was used before to limit the Reconstruction Uncertainty. 
	# It was found that the more often this process runs the less points will be deleted in each step so that finally the point cloud has the choosen Reconstruction Uncertainty and keeps it after the cameras are optimized

	while count < loop_RU:

		class ReconstructionUncertainty:

			# select points by Reconstruction Uncertainty
			PSPCF.init(chunk, PhotoScan.PointCloud.Filter.ReconstructionUncertainty)		
			PSPCF.selectPoints(float(preset_RU))
		
			# remove points
			chunk.point_cloud.removeSelectedPoints()
		
			# optimize cameras			
			chunk.optimizeCameras(fit_f=True, fit_cxcy=True, fit_aspect=True, fit_skew=True, fit_k1k2k3=True, fit_p1p2=True, fit_k4=False)
		
		count=count+1  

		continue
	
	count = 0


	# optimize Point Cloud by setting ReprojectionError
	# See technical NOTE for Reconstruction Uncertainty why to repeat this process several times
	
	while count < loop_RE:

		class ReprojectionError:
			# select points by Reprojection Error
			PSPCF.init(chunk, PhotoScan.PointCloud.Filter.ReprojectionError)		
			PSPCF.selectPoints(preset_RE)
			# remove points
			chunk.point_cloud.removeSelectedPoints()
			# optimize cameras			
			chunk.optimizeCameras(fit_f=True, fit_cxcy=True, fit_aspect=True, fit_skew=True, fit_k1k2k3=True, fit_p1p2=True, fit_k4=False)
		count=count+1  
		continue
	count = 0
	
	
	# optimize Point Cloud by setting ProjectionAccuracy
	# [TODO: No improve in the second run?? Then set runs to 1, or remove loop]
	
	while count< loop_PA:

		class ProjectionAccuracy:
			# select points by Projection Accuracy
			PSPCF.init(chunk, PhotoScan.PointCloud.Filter.ProjectionAccuracy)		
			PSPCF.selectPoints(preset_PA)
			# remove points
			chunk.point_cloud.removeSelectedPoints()
			# optimize cameras			
			chunk.optimizeCameras(fit_f=True, fit_cxcy=True, fit_aspect=True, fit_skew=True, fit_k1k2k3=True, fit_p1p2=True, fit_k4=False)
		count=count+1  
		continue
	count = 0
	
	# dense cloud model
	chunk.buildDenseCloud(quality=PhotoScan.HighQuality, filter=PhotoScan.AggressiveFiltering, keep_depth=True, reuse_depth=False)


	doc.save()

	# create Processing Report path 
	file_path = doc.path
	file_path_cut = file_path[0:-4]		
	report_path = (file_path_cut + "_reports")

	if not os.path.exists(report_path):
		os.makedirs(report_path)
			
	# export Processing Report
	chunk.exportReport(report_path + "/" + chunk.label + ".pdf")
		
	print("sparse point clouds optimized by setting:\n Reconstruction Uncertainty = " + "{:.0f}".format(preset_RU) + " >> " + "{:.0f}".format(loop_RU) + " loop(s)\n"+
                                          "         Reprojection Error = " + "{:.0f}".format(preset_RE) + " >> " + "{:.0f}".format(loop_RE) + " loop(s)\n"+ 
                                          "        Projection Accuracy = " + "{:.0f}".format(preset_PA) + " >> " + "{:.0f}".format(loop_PA) + " loop(s)\n")	
	print("Processing Reports were created and saved to " + report_path)	

if goal == "filter":    

	# define short variables
	doc = PhotoScan.app.document
	chunk = doc.chunk
	PSPCF = PhotoScan.PointCloud.Filter()
	count = 0
	cl = doc.chunks		# list of all chunks of a document
	
	# get an overview of the chunks
	print(cl)
	print(len(cl))
	
	for chunk in cl[:]:
	
	
		# optimize Point Cloud by setting ReconstructionUncertainty
		# Technical NOTE: the process runs several times as the optimizing of the camera results in points that have higher values again than the threshold value that was used before to limit the Reconstruction Uncertainty. 
		# It was found that the more often this process runs the less points will be deleted in each step so that finally the point cloud has the choosen Reconstruction Uncertainty and keeps it after the cameras are optimized
	
		while count < loop_RU:
	
			class ReconstructionUncertainty:
	
				# select points by Reconstruction Uncertainty
				PSPCF.init(chunk, PhotoScan.PointCloud.Filter.ReconstructionUncertainty)		
				PSPCF.selectPoints(preset_RU)
			
				# remove points
				chunk.point_cloud.removeSelectedPoints()
			
				# optimize cameras			
				chunk.optimizeCameras(fit_f=True, fit_cxcy=True, fit_aspect=True, fit_skew=True, fit_k1k2k3=True, fit_p1p2=True, fit_k4=False)
			
			count=count+1  
	
			continue
		
		count = 0
	
	
		# optimize Point Cloud by setting ReprojectionError
		# See technical NOTE for Reconstruction Uncertainty why to repeat this process several times
		
		while count < loop_RE:
	
			class ReprojectionError:
	
				# select points by Reprojection Error
				PSPCF.init(chunk, PhotoScan.PointCloud.Filter.ReprojectionError)		
				PSPCF.selectPoints(preset_RE)
			
				# remove points
				chunk.point_cloud.removeSelectedPoints()
			
				# optimize cameras			
				chunk.optimizeCameras(fit_f=True, fit_cxcy=True, fit_aspect=True, fit_skew=True, fit_k1k2k3=True, fit_p1p2=True, fit_k4=False)
			
				
			count=count+1  
	
			continue
		
		count = 0
		
		
		# optimize Point Cloud by setting ProjectionAccuracy
		# [TODO: No improve in the second run?? Then set runs to 1, or remove loop]
		
		while count< loop_PA:
	
			class ProjectionAccuracy:
	
				# select points by Projection Accuracy
				PSPCF.init(chunk, PhotoScan.PointCloud.Filter.ProjectionAccuracy)		
				PSPCF.selectPoints(preset_PA)
			
				# remove points
				chunk.point_cloud.removeSelectedPoints()
			
				# optimize cameras			
				chunk.optimizeCameras(fit_f=True, fit_cxcy=True, fit_aspect=True, fit_skew=True, fit_k1k2k3=True, fit_p1p2=True, fit_k4=False)
			
			count=count+1  
	
			continue
		
		count = 0
		
	
		# create Processing Report path 
		file_path = doc.path
		file_path_cut = file_path[0:-4]		
		report_path = (file_path_cut + ".reports")
	
		if not os.path.exists(report_path):
			os.makedirs(report_path)
				
		# export Processing Report
		chunk.exportReport(report_path + "/" + chunk.label + ".pdf")
			
				
	print("sparse point clouds optimized by setting:\n Reconstruction Uncertainty = " + "{:.0f}".format(preset_RU) + " >> " + "{:.0f}".format(loop_RU) + " loop(s)\n"+
	                                          "         Reprojection Error = " + "{:.0f}".format(preset_RE) + " >> " + "{:.0f}".format(loop_RE) + " loop(s)\n"+ 
	                                          "        Projection Accuracy = " + "{:.0f}".format(preset_PA) + " >> " + "{:.0f}".format(loop_PA) + " loop(s)\n")	
	print("Processing Reports were created and saved to " +report_path)

	

