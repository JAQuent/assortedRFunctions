duplicateScript <- function(path2Folder, newFileName = "scriptHistory"){
  # Duplicate a script that is a run via Rscript in console/terminal to save history
  # Source https://stackoverflow.com/questions/64575206/saving-history-for-script-run-rscript-through-terminal-console
  script_name <- getFilename()
  backup_name <- paste0(path2Folder, datedFileNam(newFileName, '.R'))
  file.copy(script_name, backup_name)
}
