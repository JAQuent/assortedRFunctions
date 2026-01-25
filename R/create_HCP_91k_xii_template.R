# Function to create a HCP-style 91k xifti object
create_HCP_91k_xii_template <- function(){
  # Create load ones_1k.dscalar.nii to get necessary stuff for subcortical data
  sub_data <- read_cifti(ciftiTools.files()$cifti["dscalar_ones"])

  # Create a new xifti with 0, 1 and 2
  template_xii <- as.xifti(cortexL = rep(0, 29696),
                           cortexR = rep(1, 29716),
                           subcortVol = as.matrix(rep(2, 31870)),
                           subcortMask = sub_data$meta$subcort$mask,
                           subcortLabs = sub_data$meta$subcort$labels)

  # Return
  return(template_xii)
}
