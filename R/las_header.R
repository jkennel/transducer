#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
las_header <- function(dat, gravity = 9.80665, density = 0.9989) {

  n      <- nrow(dat)
  type   <- dat[1]$model
  serial <- dat[1]$serial
  dt     <- dat[1]$dt / 1000
  strt   <- dat[1]$start
  stop   <- dat[1]$end
  fn     <- dat[1]$file_name
  v      <- dat[1]$ruskin_version
  datet  <- as.character(Sys.Date())

  basic_header <-
'#------------------------------------------------------------
~VERSION INFORMATION
VERS.               2.0  :CWLS LOG ASCII STANDARD - VERSION 2.0
WRAP.                NO  :ONE LINE PER DEPTH STEP
'


  version_information <-
glue(
'#------------------------------------------------------------
~WELL INFORMATION
STRT.S       ', strt,'  :FIRST INDEX VALUE
STOP.S       ', stop,'  :LAST INDEX VALUE
STEP.S               ', dt,'   :STEP
NULL.           -999.99  :NULL VALUE
COMP.              G360  :COMP
WELL.             ', serial, '  :WELL
FLD.                     :FLD
LOC.                     :LOC
SRVC.                    :SRVC
STAT.                    :STAT
CNTY.                    :CNTY
DATE.       ', datet ,'   :DATE
API.                     :API
RUSK.             ', v, '  :RUSKIN VERSION
MODEL.          ', type    ,'  :MODEL
RSK.  ', fn, ':  RSK FILE

'
)

curve_information <-
'#------------------------------------------------------------
~CURVE INFORMATION
TIME.S                   :TIME
'

parameter_information <-
'#------------------------------------------------------------
~PARAMETER INFORMATION
Datetime.     Time in UTC :Plot Title
'
column_names <-
'#------------------------------------------------------------
~A   TIME[S]'


for (i in 1:n) {

  if(dat[i]$type == 'pressure') {
    curve_information <- paste0(curve_information,
'PRES.M               :PRES
')
    parameter_information <- paste0(parameter_information,
'Pressure.    Pressure   :Pressure
')
    column_names <- paste0(column_names,
'     PRES[M]')
  } else if (dat[i]$type == 'temperature') {
    if(i == 1) {

    curve_information <- paste0(curve_information,
'TEMPHR.degC               :TEMPERATURE
'
    )
    parameter_information <- paste0(parameter_information,
'Temperature.  Temperature High Res   :Temperature
')
    column_names <- paste0(column_names,
'     TEMPHR[degC]')
    } else {
      curve_information <- paste0(curve_information,
'TEMPLR.degC               :TEMPERATURE
'
      )
      parameter_information <- paste0(parameter_information,
'Temperature.  Temperature Low Res   :Temperature
')
      column_names <- paste0(column_names,
'     TEMPLR[degC]')
    }
  }



}

  column_names <- paste0(column_names,
'
')




  other_information <-
glue('#------------------------------------------------------------
~OTHER INFORMATION
DENSITYWATER.      ', density,'   :DENSITY OF WATER
GRAVITY.           ', gravity,'  :GRAVITY
')


  glue(basic_header,
       version_information,
       curve_information,
       other_information,
       column_names)


}


#' Title
#'
#' @param dat
#' @param fn_las
#'
#' @return
#' @export
#'
#' @examples
write_las <- function(dat, fn_las, gravity = 9.80665, density = 0.9989) {

  writeLines(las_header(dat), fn_las)
  dat[type == 'pressure', data := lapply(data, function(x) x[, value := value * 10.0/(gravity * density)])]
  tmp <- dat[, (data[[1]]), by = list(id = paste(id, type, sep = '_'))]
  tmp[, value := round(value, 8)]
  setkey(tmp, id)
  tmp <- dcast(tmp, datetime~id, value.var = 'value')
  tmp[, datetime := sprintf('%.1f', datetime)]

  fwrite(tmp,
         file = fn_las,
         sep = ' ',
         append = TRUE)


}

#' Title
#'
#' @param fn_vbs
#' @param fn_las
#' @param fn_wcl
#' @param fn_ini
#' @param fn_wdt
#'
#' @return
#' @export
#'
#' @examples
write_vbs <- function(fn_vbs, fn_las, fn_wcl, fn_ini, fn_wdt=NA) {

  writeLines(
  glue('Set obWCAD = CreateObject("WellCAD.Application")
  Set obBHole = obWCAD.FileImport ("',
       normalizePath(fn_las, winslash = '\\'),
  '", FALSE, "',
  normalizePath(fn_ini, winslash = '\\'),'")
  obBHole.SaveAs "',
  normalizePath(fn_wcl, winslash = '\\'), '"
  obWCAD.CloseBorehole FALSE, 0
  obWCAD.Quit FALSE'),
  fn_vbs
  )

}

#obWCAD.ApplyTemplate "', normalizePath(fn_wdt, winslash = '\\'), '"

# fn_wdt <- here('wellcad', 'Rbr Dply Indiv Press.wdt')

write_ini <- function(fn_ini) {
writeLines('[LASImport]
MaxDepthRange=NO
TopDepth=0.0
BottomDepth=150.0',
fn_ini
)
}

#' Title
#'
#' @param fn
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
export_wcl <- function(fn,
                       by = NULL,
                       gravity = 9.80665,
                       density = 0.9989) {


  dat <- read_rbr(fn, by = by)
  dat <- rbr_start_end(dat)

  fn_vbs <- here(gsub('.rsk', '.vbs', dat[1]$file_name))
  fn_las <- here(gsub('.rsk', '.las', dat[1]$file_name))
  fn_ini <- here(gsub('.rsk', '.ini', dat[1]$file_name))
  fn_wcl <- here(gsub('.rsk', '.WCL', dat[1]$file_name))
  fn_zip <- here(gsub('.rsk', '.zip', dat[1]$file_name))

  write_ini(fn_ini)
  write_las(dat, fn_las, gravity = gravity, density = density)
  write_vbs(fn_vbs, fn_las, fn_wcl, fn_ini)

  # free some room in RAM
  rm(dat)
  gc()

  shell(
    shQuote(
      normalizePath(fn_vbs)),
    shell = 'cscript',
    flag = '//nologo')

  zip(zipfile = fn_zip, files = c(fn_vbs,
                                  fn_las,
                                  fn_ini))

  unlink(fn_vbs)
  unlink(fn_ini)
  unlink(fn_las)

  invisible()

}



