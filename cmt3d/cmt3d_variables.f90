module cmt3d_variables

  use cmt3d_constants

  implicit none

  ! parameters in or derived from the parameter file

  ! inversion parameters
  character(len=150) :: cmt_file, new_cmt_file
  integer :: npar
  real*8 :: ddelta, ddepth, dmoment

  ! data selection
  character(len=150) :: flexwin_out_file
  logical :: weigh_data_files
  real*8 :: comp_z_weight, comp_r_weight, comp_t_weight,&
       az_exp_weight, &
       pnl_dist_weight, rayleigh_dist_weight, love_dist_weight

  ! inversion schemes
  logical :: station_correction
  logical :: zero_trace_inversion, double_couple_inversion
  real*8 :: lambda
  ! misc
  logical :: write_new_syn


  ! other parameters used globally
  ! collect all pars in an array
  real*8,dimension(NPARMAX) :: cmt_par,dcmt_par,new_cmt_par

  ! number of files and windows
  integer :: nfiles,nwins(NRECMAX),nwin_total
  real*8 :: data_weights(NWINMAX)

  ! data and syn arrays
  real*8, dimension(NDATAMAX) :: data, syn, new_syn
  real, dimension(NDATAMAX) :: data_sngl, syn_sngl
  character(len=150) :: data_file, syn_file
    
  
end module cmt3d_variables

! unscaled cmt pars:  cmt_par, new_cmt_par
! scaled cmt pars:  old_par, dcmt_par, new_par


