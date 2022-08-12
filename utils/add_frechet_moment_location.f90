program add_frechet_full

! xadd_frechet_moment_location [s|a] syn_file_list old_cmt_file new_cmt-file dmoment ddepth dlocation
!   [s|a]: sac or ascii frechet derivative files
!   syn_file_list: a list of synthetic file names based on which .M[rtp][rtp], .dep, .lat, .lon files are read and added up.
!   old_cmt_file: CMTSOLUTION for which synthetics already calculated
!   new_cmt_file: CMTSOLUTION_NEW for which synthetics are to be computed by adding up the derivatives
!   dmoment: the moment scale in dyne.cm used to compute the moment derivatives
!   ddepth: delta depth in km
!   dlocation: delta in degrees for latitude and longitude 
  implicit none

  integer, parameter :: NPAR = 9 
  integer, parameter :: NM = 6
  integer , parameter :: NDATAMAX = 30000
  real, parameter :: EPS = 1.d-4

  character(len=150) :: syn_summary_file,old_cmt_file,new_cmt_file, &
       syn_file,new_synfile,dsyn_file,filetype,dm_char,dd_char,dl_char
  real :: dmoment,ddepth,dlocation
  integer :: yr,mo,jda,ho,mi
  real :: sec,t_cmt,hdur
  real :: elat,elon,depth,moment_tensor(NM)
  real :: elat_n,elon_n,depth_n,moment_tensor_new(NM)
  integer :: iofile,iosyn,ios,ios1,ipar,i,npts,npts0,nerr
  character(len=3) :: par(NPAR)
  real :: t,t0,dt,dt0,disp(NDATAMAX),syn(NDATAMAX),new_syn(NDATAMAX)


  call getarg(1,filetype)
  call getarg(2,syn_summary_file)
  call getarg(3,old_cmt_file)
  call getarg(4,new_cmt_file)
  call getarg(5,dm_char)
  call getarg(6,dd_char)
  call getarg(7,dl_char)

  if (trim(dl_char) .eq. '') &
       stop 'Usage: xadd_frechet_full [s|a] syn_file_list old_cmt_file new_cmt-file dmoment ddepth dx[dy]'

  if (trim(filetype) /= 's' .and. trim(filetype) /= 'a') &
       stop 'the filetype can only be s (sac) or a (ascii)'
  read(dm_char,*) dmoment
  read(dd_char,*) ddepth
  read(dl_char,*) dlocation

  call get_cmt(old_cmt_file,yr,mo,jda,ho,mi,sec, &
       t_cmt,hdur,elat,elon,depth,moment_tensor)

  call get_cmt(new_cmt_file,yr,mo,jda,ho,mi,sec, &
       t_cmt,hdur,elat_n,elon_n,depth_n,moment_tensor_new)

  par = (/'Mrr','Mtt','Mpp','Mrt','Mrp', &
         'Mtp','dep','lon','lat'/)
  
  iofile = 11
  iosyn = 12

  open(iofile,file=syn_summary_file,status='old',iostat=ios)
  if (ios /= 0) stop 'Error opening synthetics summary files'

  do while (ios == 0) 

     read(iofile,'(a)',iostat=ios) syn_file
     if (ios /= 0) exit

     ! read old synthetics
     if (trim(filetype) == 's') then
        call read_sacdata_f(syn_file, t, dt, npts, syn)
     else
        call rasc(syn_file, syn, npts, t, dt, NDATAMAX, nerr)
     endif
     new_syn(1:npts) = syn(1:npts)

     ! loop over derivatives
     do i = 1, NPAR
        dsyn_file = trim(syn_file)//'.'//par(i)
        
        ! read dsyn
        if (trim(filetype) == 's') then
           call read_sacdata_f(dsyn_file, t0, dt0, npts0, disp)
        else
           call rasc(dsyn_file, disp, npts, t0, dt0, npts0, NDATAMAX,nerr)
        endif
        
        ! check the header consistency
        if (  t-t0 > EPS .or. dt-dt0 > EPS .or. npts /= npts0 ) then
           print *, t0, t, dt0, dt, npts,npts0
           stop 'Error: Not same t ,dt or npts for different derivatives'
        endif
        
        ! add new_syn
        if (i <= NM) then ! moment
           new_syn(1:npts) = new_syn(1:npts) + (moment_tensor_new(i)-moment_tensor(i)) / dmoment * disp(1:npts)
        else if (i == NM + 1) then ! depth
           new_syn(1:npts) = new_syn(1:npts) + (depth_n-depth) / ddepth * (disp(1:npts) - syn(1:npts))
        else if (i == NM + 2) then ! longitude
           new_syn(1:npts) = new_syn(1:npts) + (elon_n-elon) / dlocation * (disp(1:npts) - syn(1:npts))
        else ! latitude
           new_syn(1:npts) = new_syn(1:npts) + (elat_n-elat) / dlocation * (disp(1:npts) - syn(1:npts))
        endif
     enddo ! loop over ipar

     new_synfile = trim(syn_file) // '.new'
     print *, 'Writing  file   ', trim(new_synfile), '   npts = ', npts
     if (trim(filetype) == 's') then
        call write_sacfile_f(dsyn_file,new_synfile,t,npts, new_syn)
     else
        call wasc(new_synfile, syn, npts, t, dt, nerr)
     endif

  enddo ! loop over syn files

  print *,'Done with all synthetics'

end program add_frechet_full
     
