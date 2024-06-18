#' @export

remove.died.in.hospital = function(dat){
  dat |> filter(e_discharge_place != 6)
}
