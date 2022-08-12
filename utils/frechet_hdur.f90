  program frechet_half_duration

! JT July 2001
! LQY modified for syn only

  implicit none

  integer, parameter :: NDATAMAX = 20000
  real*8, parameter :: MOMENT = 1.0e+26
  real*8, parameter :: PI = 3.141592653589793d0

  double precision, parameter :: DECAY_RATE = 2.628d0

  double precision, parameter :: SCALE_GAUSSIAN = 5.0d0/3.0d0

  integer i,j,N_j,lfile_name
  integer number_remove
  integer nt
  integer yr,mo,jda,ho,mi
  logical triangle
  double precision hdur
  double precision hdur_cmt,sec,t_cmt,elat,elon,depth,moment_tensor(6)
  double precision dt,time(NDATAMAX),dummy(NDATAMAX)
  double precision synt(NDATAMAX),synt_convolved(NDATAMAX),dsynt_convolved(NDATAMAX)
  double precision alpha,tau_j,source,dsource_dhdur
  character(len=150) cmt_file,file_name,synthetic_file

! read file with number of lines in input
  open(unit=2,file='input_frechet_hdur.txt',status='old')
  rewind(2)
  read(2,*) hdur
  read(2,"(a)") file_name
  read(2,*) triangle
  close(2)

! open the CMTSOLUTION file with the source information
!  cmt_file = 'CMTSOLUTION'
!  call get_cmt(cmt_file,yr,mo,jda,ho,mi,sec,t_cmt,hdur_cmt,elat,elon,depth,moment_tensor)
!  moment_tensor(:) = moment_tensor(:)/MOMENT

! for Gaussian use 1.66667*hdur to get roughly a triangle with half-duration hdur
  if(.not.triangle) hdur = hdur*SCALE_GAUSSIAN

! read in syn file

  call read_file(file_name,nt,dt,time,dummy)
  synt(:) = dummy(:)

! 
  alpha = DECAY_RATE/hdur
  N_j = int(hdur/dt)

  do i=1,nt

    synt_convolved(i) = 0.0
    dsynt_convolved(i) = 0.0

    do j=-N_j,N_j
      tau_j=dble(j)*dt

! convolve with a triangle
    if(triangle) then
       if(abs(tau_j) > hdur) then
         source = 0.0
         dsource_dhdur = 0.
       else if (tau_j < 0) then
         source = (tau_j+hdur)/hdur**2
         dsource_dhdur = -(2.0*tau_j+hdur)/hdur**3
       else
         source = -(tau_j-hdur)/hdur**2
         dsource_dhdur = (2.0*tau_j-hdur)/hdur**3
       endif

      else

! convolve with a Gaussian
        source = alpha*dexp(-alpha*alpha*tau_j*tau_j)/dsqrt(PI)
        dsource_dhdur = -alpha*dexp(-alpha*alpha*tau_j*tau_j)*(1.0-2.0*alpha*alpha*tau_j*tau_j)/(dsqrt(PI)*hdur)
        dsource_dhdur = dsource_dhdur*SCALE_GAUSSIAN

      endif

      if(i > j .and. (i-j) <= nt) then
        synt_convolved(i) = synt_convolved(i)+synt(i-j)*source*dt
        dsynt_convolved(i) = dsynt_convolved(i)+synt(i-j)*dsource_dhdur*dt
      endif

    enddo

  enddo

! compute number of samples to remove from end of seismograms
  number_remove = int(hdur / dt) + 1

  synthetic_file = trim(file_name)//'.hdur'
  open(unit=2,file=synthetic_file,status='unknown')
  rewind(2)
  do i=1,nt - number_remove
    write(2,*) sngl(time(i)),sngl(dsynt_convolved(i))
  enddo
  close(2)

  end program frechet_half_duration

!
! -------------------------------------------------------
!
 
  subroutine read_file(file_name,nt,dt,time,synt)

    implicit none

    integer, parameter :: NDATAMAX = 20000
    integer nt
    real t,value
    double precision dt,time(NDATAMAX),synt(NDATAMAX)
    character(len=150) file_name

    integer i,ios

    open(unit=10,file=file_name,status='old',iostat=ios)
    if (ios /= 0 ) stop 'Cannot read synthetics file'
    rewind(10)

    i=0
    ios = 0
    do while(ios == 0)
       read(10,*,iostat=ios) t,value
       if(ios == 0) then
          i = i+1
          time(i) = dble(t)
          synt(i) = dble(value)
       endif
    enddo

    nt = i
    dt = time(2)-time(1)

    close(10)

  end subroutine read_file
