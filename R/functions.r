#' Filter raw folders
#'
#' This function filter out folders containing the .raw files that should not be copied. I.e. sonar files
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
filter_rawfolder <- function(raw_folders){

  #Remove folders from list
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SX90",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SX93",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SH90",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SU90",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("MS90",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("ME90",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SN90",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("PHYSICS",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("KORONA",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("LSSS_FILES",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("CALIBRATION",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("OTHER_DOCUMENTS",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("WBAT",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("OBSERVATION_PLATFORMS",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("OTHER_DATA",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SX93",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("CRUISE_DOCUMENTS",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("TRAWL",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("Trawl",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("ADCP",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("Kalibrering",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("5 vertical beams",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("pretoktdata",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("categories",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("EXPERIMENTS",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("lsss",x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("2013842_sh90",x,value=FALSE))) == 0]


  #special cases
  raw_folders<-raw_folders[lapply(raw_folders,
                                  function(x) length(grep("\\\\ces.hi.no/cruise_data/2017/S2017840_PVENDLA_3206/201840/2017840",
                                                          x,value=FALSE))) == 0]
  raw_folders<-raw_folders[lapply(raw_folders,
                                  function(x) length(grep("\\\\ces.hi.no/cruise_data/2017/S2017840_PVENDLA_3206/2017840",
                                                          x,value=FALSE))) == 0]


  return(raw_folders)
}




#' Filter lsss folders
#'
#' This folders filter out folders that should not be included in the download
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
filter_lsssfolder <- function(lsss_folders){
  lsss_folders<-lsss_folders[lapply(lsss_folders,function(x) length(grep("categories",x,value=FALSE))) == 0]
  lsss_folders<-lsss_folders[lapply(lsss_folders,function(x) length(grep("Export",x,value=FALSE))) == 0]
  lsss_folders<-lsss_folders[lapply(lsss_folders,function(x) length(grep("lsss_DB",x,value=FALSE))) == 0]
  return(lsss_folders)
}





#' Filter work folders
#'
#' This function filter out folders containing .work files that should not be included in the download
#' I.e. profos and promos files
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
filter_workfolder <- function(work_folders){
  work_folders<-work_folders[lapply(work_folders,function(x) length(grep("EXPERIMENTS",x,value=FALSE))) == 0]
  work_folders<-work_folders[lapply(work_folders,function(x) length(grep("profos",x,value=FALSE))) == 0]
  work_folders<-work_folders[lapply(work_folders,function(x) length(grep("promos",x,value=FALSE))) == 0]
  return(work_folders)
}




#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
data_diagnose <- function(dir = '//ces.hi.no/mea/scratch/CRIMAC_survey_data',survey = NULL){



  for (i in 1:nrow((survey))){


    year <- survey[i,]$Year
    Cruise <- survey[i,]$Cruise


    #Get path of the survey
    if(any((grepl(Cruise, list.dirs(path=paste0(ces_folder,year),recursive = F))))){
      CES_path <- (grep(Cruise, list.dirs(path=paste0(ces_folder,year),recursive = F), value=TRUE))
    }else if(any((grepl(Cruise, list.dirs(path=paste0(ces_folder,year,'/staging'),recursive = F))))){
      CES_path <- (grep(Cruise, list.dirs(path=paste0(ces_folder,year,'/staging'),recursive = F), value=TRUE))
    }else{CES_path <- NULL}




    listoffiles <- list.files(CES_path,recursive = TRUE,full.names = T)

    work_folders <- unique(dirname(listoffiles[grep('.work', listoffiles)]))
    work_folders <- filter_workfolder(work_folders)

    lsss_folders <- unique(dirname(listoffiles[grep('.lsss', listoffiles)]))
    lsss_folders <- filter_lsssfolder(lsss_folders)

    raw_folders <- unique((unique(dirname(listoffiles[grep('.raw', listoffiles)]))))
    raw_folders <- filter_rawfolder(raw_folders)

    print(survey[i,])
    print('RAW:')
    print(raw_folders)
    print('LSSS:')
    print(lsss_folders)
    print('WORK:')
    print(work_folders)


  }


}



#' copy data
#'
#' This function copies the data from a tape server to a scratch disc in a clean structure
#'
#' @param dir Path to the scratch disc
#' @export
copydata <- function(dir = '//ces.hi.no/mea/scratch/CRIMAC_survey_data',survey = NULL){


  #if this is a linux

  ces_folder <- '//ces.hi.no/cruise_data/'

  if(Sys.info()['sysname']!='Windows'){
    dir<-gsub("//ces.hi.no", "//data", dir)

    ces_folder <- '//data/cruise_data/'
  }




  library(reshape2)
  library(Rstox)
  if(is.null(survey))time_series<-melt(getNMDinfo("cs"))



  copyRaw_data <- function(CES_path,Acoustic_dir){
    #Process to copy the raw data

    #Get list of files
    print('scanning for raw files')
    listoffiles <- list.files(CES_path,recursive = TRUE,full.names = T)
    print('Finnished scanning')




    #get unique folders, and filter
    raw_folders <- unique(dirname(listoffiles[grep('.raw', listoffiles)]))
    raw_folders <- filter_rawfolder(raw_folders)




    #Loop through all raw folders
    for(raw in raw_folders){



      #Print msg for the user
      print(paste0('Copying from: ',raw))



      #Special case
      if(Cruise=='2017840'){


        #Make EK60 Folder
        if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/'))){
          dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))
        }

        #Make EK60 raw folder
        if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/'))){
          dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))
        }

        #SKip duplicate folder
        if(raw != "\\\\ces.hi.no/cruise_data/2017/S2017840_PVENDLA_3206/201840"){

          #Copy each file
          for(file in list.files(raw,full.names = T)){
            if(file.exists(file) && !dir.exists(file)){
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))){
                file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))
              }
            }
          }
        }
      }

      else if(Cruise=='2013842'){


        #Make EK60 Folder
        if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/'))){
          dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))
        }

        #Make EK60 raw folder
        if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/'))){
          dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))
        }

        #SKip duplicate folder

        #Copy each file
        for(file in list.files(raw,full.names = T)){
          if(file.exists(file) && !dir.exists(file)){
            if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))){
              file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))
            }
          }
        }
      }


      #Special case
      else if(any(Cruise%in%c('2020818','2019809','2020802','2020803'))){


        #Make EK80 Folder
        if(grepl("EK80_ORIGINALRAWDATA",raw,fixed=TRUE)){
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))

          #Make EK80_RAWDATA folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/')))

          #Copy each file
          for(file in list.files(raw,full.names = T)){
            if(file.exists(file) && !dir.exists(file)){
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))){
                file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))
              }
            }
          }
        }
        else if(grepl("EK60_ORIGINALRAWDATA",raw,fixed=TRUE)){
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))

          #Make EK80_RAWDATA folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))

          #Copy each file
          for(file in list.files(raw,full.names = T)){
            if(file.exists(file) && !dir.exists(file)){
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK60_RAWDATA',basename(file),sep='/'))){
                file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))
              }
            }
          }
        }
      }



      #Special case
      else if(Cruise=='2015844'){

        if(grepl("EK60_RAWDATA",raw,fixed=TRUE)){

          #Make EK60 Folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))

          #Make EK60_RAWDATA Folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',sep='/')))

          #Loop through each file
          for(file in list.files(raw,full.names = T)){
            if(file.exists(file) && !dir.exists(file)){
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',basename(file),sep='/'))){
                file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',basename(file),sep='/'))
              }
            }

          }


        }


        else if(grepl("EK60_ORIGINALRAWDATA",raw,fixed=TRUE)){

          #Make EK60 Folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))

          #Make EK60_RAWDATA Folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))

          #Loop through each file
          for(file in list.files(raw,full.names = T)){
            if(file.exists(file) && !dir.exists(file)){
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))){
                file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))
              }
            }

          }


        }

      }

      #Special case
      else if(Cruise=='2018830'){


        #make EK80 folder
        if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/'))){
          dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))
        }

        #Make EK80_RAWDATA folder
        if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/'))){
          dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/')))
        }

        #Loop through each file
        for(file in list.files(raw,full.names = T)){
          if(file.exists(file) && !dir.exists(file)){
            if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))){
              file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))
            }
          }
        }
      }


      #General case
      else{


        if(grepl("EK60_RAWDATA",raw,fixed=TRUE)){

          #Make EK60 Folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))

          #Make EK60_RAWDATA Folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))

          #Loop through each file
          for(file in list.files(raw,full.names = T)){
            if(file.exists(file) && !dir.exists(file)){
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))){
                file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))
              }
            }

          }


        }

        else if(grepl("EK80_RAWDATA",raw,fixed=TRUE)){

          #Make EK80 folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))

          #Make EK80_RAWDATA folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/')))


          #Copy each file
          for(file in list.files(raw,full.names = T)){
            if(file.exists(file) && !dir.exists(file)){
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))){
                file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))
              }
            }

          }
        }

        else if(grepl("EK60_ORIGINALRAWDATA",raw,fixed=TRUE)){

          #Make EK60 folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/'))){
            dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))
          }

          #Make EK60_ORIGINALDATA folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',sep='/'))){
            dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',sep='/')))
          }


          #Copy each file
          for(file in list.files(raw,full.names = T)){
            if(file.exists(file) && !dir.exists(file)){
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',basename(file),sep='/'))){
                file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',basename(file),sep='/'))
              }
            }

          }


        }

        else if(grepl("EK80_ORIGINALRAWDATA",raw,fixed=TRUE)){

          #Make EK80 Folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/'))){
            dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))
          }

          #Make EK80_ORIGINALRAWDATA folder
          if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_ORIGINALRAWDATA',sep='/'))){
            dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_ORIGINALRAWDATA',sep='/')))
          }

          #Copy each file
          for(file in list.files(raw,full.names = T)){
            if(file.exists(file) && !dir.exists(file)){
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_ORIGINALRAWDATA',basename(file),sep='/'))){
                file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_ORIGINALRAWDATA',basename(file),sep='/'))
              }
            }

          }


        }

        else{ print('Did not copy')}
      }
    }

  }



  #Make main dir
  print('Make main dir')
  if(!file.exists(dir))dir.create(file.path(dir))


  # Loop through each survey
  for (i in 1:nrow((survey))){

    print(paste0('processing survey number: ',i))


    #year
    year <- survey[i,]$Year
    Cruise <- survey[i,]$Cruise


    #Make year folder
    print('make year folder')
    if(!dir.exists(paste0(dir,'/',year))) dir.create(paste0(dir,'/',year))




    #Get path of the survey
    print('get path of the survey')
    if(any((grepl(Cruise, list.dirs(path=paste0(ces_folder,year),recursive = F))))){
      CES_path <- (grep(Cruise, list.dirs(path=paste0(ces_folder,year),recursive = F), value=TRUE))
    }else if(any((grepl(Cruise, list.dirs(path=paste0(ces_folder,year,'/staging'),recursive = F))))){
      CES_path <- (grep(Cruise, list.dirs(path=paste0(ces_folder,year,'/staging'),recursive = F), value=TRUE))
    }else{CES_path <- NULL}




    #Make survey folder
    print('make survey folder')
    if(!dir.exists(paste0(dir,'/',year,'/',basename(CES_path)))) dir.create(paste0(dir,'/',year,'/',basename(CES_path)))


    #Store directory of survey folder
    survey_dir <- paste0(dir,'/',year,'/',basename(CES_path))

    #Acoustic dir
    Acoustic_dir <- paste(survey_dir,'ACOUSTIC',sep='/')

    #Create acoustic dir
    print('Make acoutic folder')
    if(!dir.exists(Acoustic_dir)) dir.create(Acoustic_dir)


    #Copy raw data
    print('Copy raw data')
    copyRaw_data(CES_path,Acoustic_dir)


    #Link to the lsss and work dir from path
    lsss_dir <- paste(Acoustic_dir,'LSSS',sep='/')
    Work_dir <- paste(Acoustic_dir,'LSSS','WORK',sep='/')


    #Create in the folder structure
    if(!dir.exists(lsss_dir)) dir.create(lsss_dir)
    if(!dir.exists(Work_dir)) dir.create(Work_dir)

    lsss_dir <- paste(lsss_dir,'LSSS_FILES',sep='/')
    if(!dir.exists(lsss_dir)) dir.create(lsss_dir)


    #For bookkeeping
    exist_remote = F
    exist_local = F

    #Set working dir to work_dir (makes the code simpler)
    setwd(Work_dir)


    #Check if git is avaliable on server and on data
    if(system(sprintf("git ls-remote \"%s\"",paste0('https://git.imr.no/lsss_interpretationmask/',basename(CES_path),'.git')))==0)exist_remote <- T
    if(system('git branch')==0) exist_local <- T


    #List all files on CES
    print('Scanning all files')
    listoffiles <- list.files(CES_path,recursive = TRUE,full.names = T)
    print('Finnished scanning')


    #Grab all folders for the work files and filter out special cases that should not be moved
    work_folders <- unique(dirname(listoffiles[grep('.work', listoffiles)]))
    work_folders <- filter_workfolder(work_folders)



    #Git handling
    if(!all(exist_local,exist_remote)){
      print('Adding new git to both local and remote')
      system('git init ')
    }else{

      if(!any(exist_local,exist_remote)){
        if(exist_local){
          print('Local git exist')
        }
        else if(exist_remote){
          print('Remote git exist. Cloning to local')
          system(paste0('git clone ', paste0('https://git.imr.no/lsss_interpretationmask/',basename(CES_path),'.git'),' ./'))
        }
      }
      else{
      print('Git exist in both versions')
      }
      }


    #Loop through each work dir
    for( work_folder in work_folders){

      #Get log of git
      got_log <- system(paste('git log --pretty=format:%s '), intern = TRUE)

      #If this versjon has not been committed do the process
      if(!any(work_folder%in%got_log)){
        print('Version does not exist in git. Adding new version!')

        #Remove all files from folder first.
        for (file in list.files(Work_dir,full.names = T)){
                    Sys.chmod(file, "777", use_umask = FALSE)
                    file.remove(file)
        }

        #Copy all files
        file.copy(list.files(work_folder,full.names = T),Work_dir)

        #Comitt changes to local repository
        system('git add .')
        system(paste0('git commit -m ',work_folder))

        if(!exist_remote){
          print('Creating new remote repository')
          system(paste0("git remote add ",basename(CES_path) , ' ',paste0('https://git.imr.no/lsss_interpretationmask/',basename(CES_path),'.git') ))
        }

        #Commit changes to remote
        print('Push to remote')
        system(paste0('git push ', basename(CES_path),' master'))
      }

    }





    #For bookkeeping
    exist_remote = F
    exist_local = F


    #LSSS handling
    lsss_folders <- unique(dirname(listoffiles[grep('.lsss', listoffiles)]))
    lsss_folders <- filter_lsssfolder(lsss_folders)

    setwd(lsss_dir)


    #Check if git is avaliable on server and on data
    if(system(sprintf("git ls-remote \"%s\"",paste0('https://git.imr.no/lsss_lsss_config/',basename(CES_path),'.git')))==0)exist_remote <- T
    if(system('git branch')==0) exist_local <- T


    #Git handling
    exist_local
    exist_remote
    if(!all(exist_local,exist_remote)){
      print('Adding new git to both local and remote')
      system('git init ')
    }else{

      if(!any(exist_local,exist_remote)){
        if(exist_local){
          print('Local git exist')
        }
        else if(exist_remote){
          print('Remote git exist. Cloning to local')
          system(paste0('git clone ', paste0('https://git.imr.no/lsss_lsss_config/',basename(CES_path),'.git'),' ./'))
        }
      }
      else{
      print('Git exist in both versions')
      }
      }



    #Loop through each work dir
    for(lsss_folder in lsss_folders){

      #Get log of git
      got_log <- system(paste('git log --pretty=format:%s '), intern = TRUE)


    #   #If this versjon has not been committed do the process
      if(!any(lsss_folder%in%got_log)){
        print('Version does not exist in git. Adding new version!')

        #Remove all files from folder first.
        for (file in list.files(lsss_dir  ,full.names = T)){
                    Sys.chmod(file, "777", use_umask = FALSE)
                    file.remove(file)
        }


    #     #Copy all files
        file.copy(list.files(lsss_folder,full.names = T, pattern = '.lsss'),lsss_dir)

        #Comitt changes to local repository
        system('git add .')
        system(paste0('git commit -m ',lsss_folder))

        if(!exist_remote){
          print('Creating new remote repository')
          system(paste0("git remote add ",basename(CES_path) , ' ',paste0('https://git.imr.no/lsss_lsss_config/',basename(CES_path),'.git') ))
        }

        #Commit changes to remote
        print('Push to remote')
        system(paste0('git push ', basename(CES_path),' master'))
      }
    #
      #' Filter raw folders
      #'
      #' This function filter out folders containing the .raw files that should not be copied. I.e. sonar files
      #'
      #' @param infile Path to the input file
      #' @return A matrix of the infile
      #' @export
      filter_rawfolder <- function(raw_folders){

        #Remove folders from list
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SX90",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SX93",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SH90",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SU90",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("MS90",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("ME90",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SN90",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("PHYSICS",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("KORONA",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("LSSS_FILES",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("CALIBRATION",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("OTHER_DOCUMENTS",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("WBAT",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("OBSERVATION_PLATFORMS",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("OTHER_DATA",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("SX93",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("CRUISE_DOCUMENTS",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("TRAWL",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("Trawl",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("ADCP",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("Kalibrering",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("5 vertical beams",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("pretoktdata",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("categories",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("EXPERIMENTS",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("lsss",x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,function(x) length(grep("2013842_sh90",x,value=FALSE))) == 0]


        #special cases
        raw_folders<-raw_folders[lapply(raw_folders,
                                        function(x) length(grep("\\\\ces.hi.no/cruise_data/2017/S2017840_PVENDLA_3206/201840/2017840",
                                                                x,value=FALSE))) == 0]
        raw_folders<-raw_folders[lapply(raw_folders,
                                        function(x) length(grep("\\\\ces.hi.no/cruise_data/2017/S2017840_PVENDLA_3206/2017840",
                                                                x,value=FALSE))) == 0]


        return(raw_folders)
      }




      #' Filter lsss folders
      #'
      #' This folders filter out folders that should not be included in the download
      #'
      #' @param infile Path to the input file
      #' @return A matrix of the infile
      #' @export
      filter_lsssfolder <- function(lsss_folders){
        lsss_folders<-lsss_folders[lapply(lsss_folders,function(x) length(grep("categories",x,value=FALSE))) == 0]
        lsss_folders<-lsss_folders[lapply(lsss_folders,function(x) length(grep("Export",x,value=FALSE))) == 0]
        lsss_folders<-lsss_folders[lapply(lsss_folders,function(x) length(grep("lsss_DB",x,value=FALSE))) == 0]
        return(lsss_folders)
      }





      #' Filter work folders
      #'
      #' This function filter out folders containing .work files that should not be included in the download
      #' I.e. profos and promos files
      #'
      #' @param infile Path to the input file
      #' @return A matrix of the infile
      #' @export
      filter_workfolder <- function(work_folders){
        work_folders<-work_folders[lapply(work_folders,function(x) length(grep("EXPERIMENTS",x,value=FALSE))) == 0]
        work_folders<-work_folders[lapply(work_folders,function(x) length(grep("profos",x,value=FALSE))) == 0]
        work_folders<-work_folders[lapply(work_folders,function(x) length(grep("promos",x,value=FALSE))) == 0]
        return(work_folders)
      }




      #' Load a Matrix
      #'
      #' This function loads a file as a matrix. It assumes that the first column
      #' contains the rownames and the subsequent columns are the sample identifiers.
      #' Any rows with duplicated row names will be dropped with the first one being
      #' kepted.
      #'
      #' @param infile Path to the input file
      #' @return A matrix of the infile
      #' @export
      data_diagnose <- function(dir = '//ces.hi.no/mea/scratch/CRIMAC_survey_data',survey = NULL){



        for (i in 1:nrow((survey))){


          year <- survey[i,]$Year
          Cruise <- survey[i,]$Cruise


          #Get path of the survey
          if(any((grepl(Cruise, list.dirs(path=paste0(ces_folder,year),recursive = F))))){
            CES_path <- (grep(Cruise, list.dirs(path=paste0(ces_folder,year),recursive = F), value=TRUE))
          }else if(any((grepl(Cruise, list.dirs(path=paste0(ces_folder,year,'/staging'),recursive = F))))){
            CES_path <- (grep(Cruise, list.dirs(path=paste0(ces_folder,year,'/staging'),recursive = F), value=TRUE))
          }else{CES_path <- NULL}




          listoffiles <- list.files(CES_path,recursive = TRUE,full.names = T)

          work_folders <- unique(dirname(listoffiles[grep('.work', listoffiles)]))
          work_folders <- filter_workfolder(work_folders)

          lsss_folders <- unique(dirname(listoffiles[grep('.lsss', listoffiles)]))
          lsss_folders <- filter_lsssfolder(lsss_folders)

          raw_folders <- unique((unique(dirname(listoffiles[grep('.raw', listoffiles)]))))
          raw_folders <- filter_rawfolder(raw_folders)

          print(survey[i,])
          print('RAW:')
          print(raw_folders)
          print('LSSS:')
          print(lsss_folders)
          print('WORK:')
          print(work_folders)


        }


      }



      #' copy data
      #'
      #' This function copies the data from a tape server to a scratch disc in a clean structure
      #'
      #' @param dir Path to the scratch disc
      #' @export
      copydata <- function(dir = '//ces.hi.no/mea/scratch/CRIMAC_survey_data',survey = NULL){


        #if this is a linux

        ces_folder <- '//ces.hi.no/cruise_data/'
        os_ <- T

        if(Sys.info()['sysname']!='Windows'){
          dir<-gsub("//ces.hi.no", "//data", dir)
          os_ <- F
          ces_folder <- '//data/cruise_data/'
        }




        library(reshape2)
        library(Rstox)
        if(is.null(survey))time_series<-melt(getNMDinfo("cs"))



        copyRaw_data <- function(CES_path,Acoustic_dir){
          #Process to copy the raw data

          #Get list of files
          print('scanning for raw files')
          listoffiles <- list.files(CES_path,recursive = TRUE,full.names = T)
          print('Finnished scanning')




          #get unique folders, and filter
          raw_folders <- unique(dirname(listoffiles[grep('.raw', listoffiles)]))
          raw_folders <- filter_rawfolder(raw_folders)




          #Loop through all raw folders
          for(raw in raw_folders){



            #Print msg for the user
            print(paste0('Copying from: ',raw))



            #Special case
            if(Cruise=='2017840'){


              #Make EK60 Folder
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/'))){
                dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))
              }

              #Make EK60 raw folder
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/'))){
                dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))
              }

              #SKip duplicate folder
              if(raw != "\\\\ces.hi.no/cruise_data/2017/S2017840_PVENDLA_3206/201840"){

                #Copy each file
                for(file in list.files(raw,full.names = T)){
                  if(file.exists(file) && !dir.exists(file)){
                    if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))){
                      file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))
                    }
                  }
                }
              }
            }

            else if(Cruise=='2013842'){


              #Make EK60 Folder
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/'))){
                dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))
              }

              #Make EK60 raw folder
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/'))){
                dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))
              }

              #SKip duplicate folder

              #Copy each file
              for(file in list.files(raw,full.names = T)){
                if(file.exists(file) && !dir.exists(file)){
                  if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))){
                    file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))
                  }
                }
              }
            }


            #Special case
            else if(any(Cruise%in%c('2020818','2019809','2020802','2020803'))){


              #Make EK80 Folder
              if(grepl("EK80_ORIGINALRAWDATA",raw,fixed=TRUE)){
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))

                #Make EK80_RAWDATA folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/')))

                #Copy each file
                for(file in list.files(raw,full.names = T)){
                  if(file.exists(file) && !dir.exists(file)){
                    if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))){
                      file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))
                    }
                  }
                }
              }
              else if(grepl("EK60_ORIGINALRAWDATA",raw,fixed=TRUE)){
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))

                #Make EK80_RAWDATA folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))

                #Copy each file
                for(file in list.files(raw,full.names = T)){
                  if(file.exists(file) && !dir.exists(file)){
                    if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK60_RAWDATA',basename(file),sep='/'))){
                      file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))
                    }
                  }
                }
              }
            }



            #Special case
            else if(Cruise=='2015844'){

              if(grepl("EK60_RAWDATA",raw,fixed=TRUE)){

                #Make EK60 Folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))

                #Make EK60_RAWDATA Folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',sep='/')))

                #Loop through each file
                for(file in list.files(raw,full.names = T)){
                  if(file.exists(file) && !dir.exists(file)){
                    if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',basename(file),sep='/'))){
                      file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',basename(file),sep='/'))
                    }
                  }

                }


              }


              else if(grepl("EK60_ORIGINALRAWDATA",raw,fixed=TRUE)){

                #Make EK60 Folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))

                #Make EK60_RAWDATA Folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))

                #Loop through each file
                for(file in list.files(raw,full.names = T)){
                  if(file.exists(file) && !dir.exists(file)){
                    if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))){
                      file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))
                    }
                  }

                }


              }

            }

            #Special case
            else if(Cruise=='2018830'){


              #make EK80 folder
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/'))){
                dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))
              }

              #Make EK80_RAWDATA folder
              if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/'))){
                dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/')))
              }

              #Loop through each file
              for(file in list.files(raw,full.names = T)){
                if(file.exists(file) && !dir.exists(file)){
                  if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))){
                    file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))
                  }
                }
              }
            }


            #General case
            else{


              if(grepl("EK60_RAWDATA",raw,fixed=TRUE)){

                #Make EK60 Folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))

                #Make EK60_RAWDATA Folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',sep='/')))

                #Loop through each file
                for(file in list.files(raw,full.names = T)){
                  if(file.exists(file) && !dir.exists(file)){
                    if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))){
                      file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_RAWDATA',basename(file),sep='/'))
                    }
                  }

                }


              }

              else if(grepl("EK80_RAWDATA",raw,fixed=TRUE)){

                #Make EK80 folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))

                #Make EK80_RAWDATA folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/')))dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',sep='/')))


                #Copy each file
                for(file in list.files(raw,full.names = T)){
                  if(file.exists(file) && !dir.exists(file)){
                    if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))){
                      file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_RAWDATA',basename(file),sep='/'))
                    }
                  }

                }
              }

              else if(grepl("EK60_ORIGINALRAWDATA",raw,fixed=TRUE)){

                #Make EK60 folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/'))){
                  dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60',sep='/')))
                }

                #Make EK60_ORIGINALDATA folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',sep='/'))){
                  dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',sep='/')))
                }


                #Copy each file
                for(file in list.files(raw,full.names = T)){
                  if(file.exists(file) && !dir.exists(file)){
                    if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',basename(file),sep='/'))){
                      file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK60','EK60_ORIGINALRAWDATA',basename(file),sep='/'))
                    }
                  }

                }


              }

              else if(grepl("EK80_ORIGINALRAWDATA",raw,fixed=TRUE)){

                #Make EK80 Folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/'))){
                  dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80',sep='/')))
                }

                #Make EK80_ORIGINALRAWDATA folder
                if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_ORIGINALRAWDATA',sep='/'))){
                  dir.create(file.path(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_ORIGINALRAWDATA',sep='/')))
                }

                #Copy each file
                for(file in list.files(raw,full.names = T)){
                  if(file.exists(file) && !dir.exists(file)){
                    if(!file.exists(paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_ORIGINALRAWDATA',basename(file),sep='/'))){
                      file.copy(from=file, to=paste(dir,year,basename(CES_path),'ACOUSTIC','EK80','EK80_ORIGINALRAWDATA',basename(file),sep='/'))
                    }
                  }

                }


              }

              else{ print('Did not copy')}
            }
          }

        }



        #Make main dir
        print('Make main dir')
        if(!file.exists(dir))dir.create(file.path(dir))


        # Loop through each survey
        for (i in 1:nrow((survey))){

          print(paste0('processing survey number: ',i))


          #year
          year <- survey[i,]$Year
          Cruise <- survey[i,]$Cruise


          #Make year folder
          print('make year folder')
          if(!dir.exists(paste0(dir,'/',year))) dir.create(paste0(dir,'/',year))




          #Get path of the survey
          print('get path of the survey')
          if(any((grepl(Cruise, list.dirs(path=paste0(ces_folder,year),recursive = F))))){
            CES_path <- (grep(Cruise, list.dirs(path=paste0(ces_folder,year),recursive = F), value=TRUE))
          }else if(any((grepl(Cruise, list.dirs(path=paste0(ces_folder,year,'/staging'),recursive = F))))){
            CES_path <- (grep(Cruise, list.dirs(path=paste0(ces_folder,year,'/staging'),recursive = F), value=TRUE))
          }else{CES_path <- NULL}




          #Make survey folder
          print('make survey folder')
          if(!dir.exists(paste0(dir,'/',year,'/',basename(CES_path)))) dir.create(paste0(dir,'/',year,'/',basename(CES_path)))


          #Store directory of survey folder
          survey_dir <- paste0(dir,'/',year,'/',basename(CES_path))

          #Acoustic dir
          Acoustic_dir <- paste(survey_dir,'ACOUSTIC',sep='/')

          #Create acoustic dir
          print('Make acoutic folder')
          if(!dir.exists(Acoustic_dir)) dir.create(Acoustic_dir)


          #Copy raw data
          print('Copy raw data')
          copyRaw_data(CES_path,Acoustic_dir)


          #Link to the lsss and work dir from path
          lsss_dir <- paste(Acoustic_dir,'LSSS',sep='/')
          Work_dir <- paste(Acoustic_dir,'LSSS','WORK',sep='/')


          #Create in the folder structure
          if(!dir.exists(lsss_dir)) dir.create(lsss_dir)
          if(!dir.exists(Work_dir)) dir.create(Work_dir)

          lsss_dir <- paste(lsss_dir,'LSSS_FILES',sep='/')
          if(!dir.exists(lsss_dir)) dir.create(lsss_dir)


          #For bookkeeping
          exist_remote = F
          exist_local = F

          #Set working dir to work_dir (makes the code simpler)
          setwd(Work_dir)


          #Check if git is avaliable on server and on data
          if(os_==T){
            if(system(sprintf("git ls-remote \"%s\"",paste0('https://git.imr.no/lsss_interpretationmask/',basename(CES_path),'.git')))==0)exist_remote <- T}

          if(system('git branch')==0) exist_local <- T


          #List all files on CES
          print('Scanning all files')
          listoffiles <- list.files(CES_path,recursive = TRUE,full.names = T)
          print('Finnished scanning')


          #Grab all folders for the work files and filter out special cases that should not be moved
          work_folders <- unique(dirname(listoffiles[grep('.work', listoffiles)]))
          work_folders <- filter_workfolder(work_folders)



          #Git handling
          if(!all(exist_local,exist_remote)){
            print('Adding new git to both local and remote')
            system('git init ')
          }else{

            if(!any(exist_local,exist_remote)){
              if(exist_local){
                print('Local git exist')
              }
              else if(exist_remote){
                print('Remote git exist. Cloning to local')
                system(paste0('git clone ', paste0('https://git.imr.no/lsss_interpretationmask/',basename(CES_path),'.git'),' ./'))
              }
            }
            else{
              print('Git exist in both versions')
            }
          }


          #Loop through each work dir
          for( work_folder in work_folders){

            #Get log of git
            got_log <- system(paste('git log --pretty=format:%s '), intern = TRUE)

            #If this versjon has not been committed do the process
            if(!any(work_folder%in%got_log)){
              print('Version does not exist in git. Adding new version!')

              #Remove all files from folder first.
              for (file in list.files(Work_dir,full.names = T)){
                Sys.chmod(file, "777", use_umask = FALSE)
                file.remove(file)
              }

              #Copy all files
              file.copy(list.files(work_folder,full.names = T),Work_dir)

              #Comitt changes to local repository
              system('git add .')
              system(paste0('git commit -m ',work_folder))

              if(os_==T){
                if(!exist_remote){
                  print('Creating new remote repository')
                  system(paste0("git remote add ",basename(CES_path) , ' ',paste0('https://git.imr.no/lsss_interpretationmask/',basename(CES_path),'.git') ))
                }

                #Commit changes to remote
                print('Push to remote')
                system(paste0('git push ', basename(CES_path),' master'))
              }
            }

          }





          #For bookkeeping
          exist_remote = F
          exist_local = F


          #LSSS handling
          lsss_folders <- unique(dirname(listoffiles[grep('.lsss', listoffiles)]))
          lsss_folders <- filter_lsssfolder(lsss_folders)

          setwd(lsss_dir)


          #Check if git is avaliable on server and on data
          if(system(sprintf("git ls-remote \"%s\"",paste0('https://git.imr.no/lsss_lsss_config/',basename(CES_path),'.git')))==0)exist_remote <- T
          if(system('git branch')==0) exist_local <- T


          #Git handling
          exist_local
          exist_remote
          if(!all(exist_local,exist_remote)){
            print('Adding new git to both local and remote')
            system('git init ')
          }else{

            if(!any(exist_local,exist_remote)){
              if(exist_local){
                print('Local git exist')
              }
              else if(exist_remote){
                print('Remote git exist. Cloning to local')
                system(paste0('git clone ', paste0('https://git.imr.no/lsss_lsss_config/',basename(CES_path),'.git'),' ./'))
              }
            }
            else{
              print('Git exist in both versions')
            }
          }



          #Loop through each work dir
          for(lsss_folder in lsss_folders){

            #Get log of git
            got_log <- system(paste('git log --pretty=format:%s '), intern = TRUE)


            #   #If this versjon has not been committed do the process
            if(!any(lsss_folder%in%got_log)){
              print('Version does not exist in git. Adding new version!')

              #Remove all files from folder first.
              for (file in list.files(lsss_dir  ,full.names = T)){
                Sys.chmod(file, "777", use_umask = FALSE)
                file.remove(file)
              }


              #     #Copy all files
              file.copy(list.files(lsss_folder,full.names = T, pattern = '.lsss'),lsss_dir)

              #Comitt changes to local repository
              system('git add .')
              system(paste0('git commit -m ',lsss_folder))

              if(!exist_remote){
                print('Creating new remote repository')
                system(paste0("git remote add ",basename(CES_path) , ' ',paste0('https://git.imr.no/lsss_lsss_config/',basename(CES_path),'.git') ))
              }

              #Commit changes to remote
              print('Push to remote')
              system(paste0('git push ', basename(CES_path),' master'))
            }
            #
          }




        }


      }




    }




    }


    }




