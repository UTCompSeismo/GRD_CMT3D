module cmt3d_constants

!**********************************************
!* All the constants used in CMT 3D INVERSION *
!**********************************************

  implicit none

! mathematical constantS
  real*8, parameter :: PI = 3.141592653589793d0

! scale of cmt pars (latitude,longitude,depth and moment
!          centroid time and half duration)
  real*8, parameter :: SCALE_DELTA = 0.001 ! degree
  real*8, parameter :: SCALE_DEPTH = 1.0  ! km
  real*8, parameter :: SCALE_MOMENT = 1.0e+22 ! dyns*cm
  real*8, parameter :: SCALE_CTIME = 0.1  ! seconds
  real*8, parameter :: SCALE_HDUR = 1.0 ! seconds

! maximum number of parameters
  integer, parameter  :: NPARMAX = 11

! scales and names of derivative paramete
!note: coordinates for location is set to be depth,longitude and latituders
  real*8, parameter :: SCALE_PAR(NPARMAX) =  &
       (/ SCALE_MOMENT, SCALE_MOMENT, SCALE_MOMENT, SCALE_MOMENT, &
       SCALE_MOMENT, SCALE_MOMENT, SCALE_DEPTH, SCALE_DELTA, SCALE_DELTA, &
        SCALE_CTIME, SCALE_HDUR /)   

  character(len=9), parameter :: PAR_NAME(NPARMAX) =  &
       (/'Mrr      ','Mtt      ','Mpp      ','Mrt      ','Mrp      ', &
         'Mtp      ','depth    ','longitude','latitude ', &
         'ctime    ','hdur     '/)  
! maximum npts for records
  integer, parameter :: NDATAMAX = 30000

! maximum number of records (NRECMAX < NWINMAX)
  integer, parameter :: NRECMAX = 500
  
! maximum number of windows
  integer, parameter :: NWINMAX = 1000

! number of pars for moment only
  integer, parameter :: NM = 6

! small numbers
  real*8, parameter :: EPS2 = 1.0d-2
  real*8, parameter :: EPS5 = 1.0d-5

! io unit for parameter files
  integer, parameter :: IOPAR = 40

! io unit for flexwin output files
  integer, parameter :: IOWIN = 35

! io unit for input file
  integer, parameter :: IOINV = 30

! debug boolean
  logical, parameter :: DEBUG = .true.

! number of regions for azimuthal weighting
  integer, parameter :: NREGIONS = 10

! reference distance for Pnl, Rayleigh and Love wave weighting
  real*8, parameter :: REF_DIST = 100.0


end module cmt3d_constants

