# Run through pre-trained visual neural network models, see https://github.com/kidzik/deep-features

extractVideoFeatures <- function(featurescriptDir, imageDir, outputzipFile){
  
  origDir <- getwd()
  setwd(featurescriptDir)
  # we will need to do this through docker (torch is installed there, probably)
  # IMPORTANT: from the dl-docker image, we have to install loadcaffe. Execute:
  # sudo docker run -it -p 8888:8888 -p 6006:6006 -v /media/sf_shared:/root/sharedfolder floydhub/dl-docker:cpu bash
  # luarocks install loadcaffe
  # In another terminal, do:
  # sudo docker ps # note the hash, to be used in the next command
  # Now, commit the installation of loadcaffe with (will occupy 3GB!): 
  # sudo docker commit <<bb0db19ae795>> lprisan/dl-docker-loadcaffe:cpu
  # sudo docker run -it -p 8888:8888 -p 6006:6006 -v /media/sf_shared:/root/sharedfolder lprisan/dl-docker-loadcaffe:cpu th /root/sharedfolder/deep-features/extract.lua -images_path /root/sharedfolder/deep-features/testimage -output_csv /root/sharedfolder/deep-features/output.csv
  # Execute the Lua script to extract the last layer of features  
  #cmd <- paste("th extract.lua -images_path",imageDir,"-output_csv",interim)
  cmd <- "sudo docker run -it -p 8888:8888 -p 6006:6006 -v /media/sf_shared:/root/sharedfolder lprisan/dl-docker-loadcaffe:cpu th /root/sharedfolder/deep-features/extract.lua -images_path /root/sharedfolder/paper-JLA-deep-teaching-analytics/data/interim/videoframes -output_csv /root/sharedfolder/deep-features/output.csv -proto_file /root/sharedfolder/deep-features/models/VGG_ILSVRC_19_layers_deploy.prototxt -model_file /root/sharedfolder/deep-features/models/VGG_ILSVRC_19_layers.caffemodel"
  # We may need to separate the frames into several folders, in case there are problems with insufficient memory
  #system(cmd, wait=T,input=readline("Enter your password: "))
  
  # Put the output.csv file into a videofeatures-lastlayer.csv.zip file in the data/interim dir (or wherever specified)
  #outputzipFile <- videofeaturesfile
  #featurescriptDir <- "/home/lprisan/workspace/deep-features"
  files <- list.files(featurescriptDir,pattern = "*.csv")
  
  zip(outputzipFile,paste(featurescriptDir,files,sep=.Platform$file.sep))
  
  setwd(origDir)
  
  outputzipFile
  
}  