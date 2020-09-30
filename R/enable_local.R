#' @title enable_local
#' @description This function extracts all of the necessary oracle tables to a local folder so that
#' the functions can be run locally.
#' @param data.dir  The default is your working directory. If you are hoping to
#' load existing data, this folder should identify the folder containing your
#' *.rdata files.
#' @param fn.oracle.username default is \code{'_none_'} This is your username for
#' accessing oracle objects. If you have a value for \code{oracle.username}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param fn.oracle.password default is \code{'_none_'} This is your password for
#' accessing oracle objects. If you have a value for \code{oracle.password}
#' stored in your environment (e.g. from an rprofile file), this can be left out
#' and that value will be used.  If a value for this is provided, it will take
#' priority over your existing value.
#' @param fn.oracle.dsn default is \code{'_none_'} This is your dsn/ODBC
#' identifier for accessing oracle objects. If you have a value for
#' \code{oracle.dsn} stored in your environment (e.g. from an rprofile file),
#' this can be left and that value will be used.  If a value for this is
#' provided, it will take priority over your existing value.
#' @param usepkg default is \code{'rodbc'}. This indicates whether the connection to Oracle should
#' use \code{'rodbc'} or \code{'roracle'} to connect.  rodbc is slightly easier to setup, but
#' roracle will extract data ~ 5x faster.
#' @family setup
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
enable_local <- function(data.dir = NULL,
                        fn.oracle.username = "_none_",
                        fn.oracle.password = "_none_",
                        fn.oracle.dsn = "_none_",
                        usepkg = "rodbc"){
  cat("Extracting MARFIS data...\n")
  Mar.utils::get_data_tables(fn.oracle.username = fn.oracle.username,
                                     fn.oracle.password = fn.oracle.password,
                                     fn.oracle.dsn = fn.oracle.dsn,
                                     schema = "MARFISSCI",
                                     data.dir = data.dir,
                                     tables = c("GEARS","HAIL_IN_CALLS","HAIL_OUTS","LICENCE_SUBTYPES",
                                                "LICENCE_VESSELS","LICENCES","LOG_EFRT_ENTRD_DETS",
                                                "LOG_EFRT_STD_INFO","MON_DOC_DEFNS","MON_DOC_ENTRD_DETS",
                                                "MON_DOCS","NAFO_UNIT_AREAS","PRO_SPC_INFO","VESSELS"),
                                     env = environment(), quietly = TRUE)

  cat("Extracting ISDB data...\n")
  Mar.utils::get_data_tables(fn.oracle.username = fn.oracle.username,
                                     fn.oracle.password = fn.oracle.password,
                                     fn.oracle.dsn = fn.oracle.dsn,
                                     schema = "ISDB",
                                     data.dir = data.dir,
                                     tables = c("ISFISHSETS","ISSETPROFILE_WIDE","ISTRIPS","ISVESSELS"),
                                     env = environment(), quietly = TRUE)
cat(paste0("All tables successfully extracted to ", data.dir),"\n")
}
