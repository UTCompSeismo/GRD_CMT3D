program add_derivatives

! xadd_frechet_moment_only [s|a] syn_summary_file cmt_file moment_scale
!   [s|a]: sac or ascii moment derivative files
!   syn_summary_file: a list of synthetic file names based on which .M[rtp][rtp]
!                     files are read and added up.
!   cmt_file: CMTSOLUTION (only moment tensor is used)
!   moment_scale: the moment scale in dyne.cm used to compute the moment derivatives
  implicit none

  character(len=150) :: syn_summary_file,cmt_file,synfile,new_synfile,sm_char,filetype
  real :: scale_moment
  integer :: yr,mo,jda,ho,mi
  real :: sec,t_cmt,hdur,elat,elon,depth,moment_tensor(6)
  integer :: iofile,iosyn,ios,ios1,ipar,i,npts,npts0,nerr
  character(len=3) :: par(6)
  integer , parameter :: NDATAMAX = 30000
  real :: eps,t,t0,dt,dt0,disp(NDATAMAX),syn(NDATAMAX)
  logical :: lex

  call getarg(1,filetype)
  call getarg(2,syn_summary_file)
  call getarg(3,cmt_file)
  call getarg(4,sm_char)

  if (trim(syn_summary_file) .eq. '' .or. &
       trim(cmt_file) .eq. '' .or. trim(sm_char) .eq. '') &
       stop 'Usage: xadd_derivatives [s|a] syn_summary_file cmt_file moment_scale'

  if (trim(filetype) /= 's' .and. trim(filetype) /= 'a') &
       stop 'the filetype can only be s (sac) or a (ascii)'
  read(sm_char,*) scale_moment

  call get_cmt(cmt_file,yr,mo,jda,ho,mi,sec, &
       t_cmt,hdur,elat,elon,depth,moment_tensor)


  moment_tensor = moment_tensor/scale_moment

  iofile = 11
  iosyn = 12
  open(iofile,file=syn_summary_file,status='old',iostat=ios)
  if (ios /= 0) stop 'Error opening synthetics summary files'

  par = (/ 'Mrr', 'Mtt', 'Mpp', 'Mrt', 'Mrp', 'Mtp' /)
  ipar = 6
  eps = 1e-4

  print *, 'Looping over synthetics ...'
  do while (ios == 0) 

     read(iofile,'(a)',iostat=ios) synfile
     if (ios /= 0) exit
     do i = 1, ipar
        new_synfile = trim(synfile)//'.'//par(i)
        inquire(file=new_synfile,exist=lex)
        if (.not. lex) exit
        if (i == 1) then
           if (trim(filetype) == 's') then
              call read_sacdata_f(new_synfile, t0, dt0, npts0, disp)
           else
              call rasc(new_synfile, disp, npts0, t0, dt0, NDATAMAX, nerr)
           endif
           syn(1:npts0) = disp(1:npts0) * moment_tensor(i)
        else
           if (trim(filetype) == 's') then
              call read_sacdata_f(new_synfile, t, dt, npts, disp)
           else
              call rasc(new_synfile, disp, npts, t, dt, NDATAMAX, nerr)
           endif
           if (  t-t0 > eps .or. dt-dt0 > eps .or. npts /= npts0 ) then
              print *, t0, t, dt0, dt, npts,npts0
              stop 'Error: Not same t ,dt or npts for different derivatives'
           endif
           syn(1:npts) = syn(1:npts) + disp(1:npts) * moment_tensor(i)
        endif
     enddo
     if (i <=  ipar) then
        print *, 'Missing file ', trim(synfile)//'.'//trim(par(i))
     else
!        print *, 'Writing  file   ', trim(synfile), '   npts = ', npts
        if (trim(filetype) == 's') then
           call write_sacfile_f(new_synfile,synfile,t,npts, syn)
        else
           call wasc(synfile,syn,npts,t,dt,nerr)
        endif
     endif
  enddo

  print *,'Done with all synthetics'

end program add_derivatives
     
