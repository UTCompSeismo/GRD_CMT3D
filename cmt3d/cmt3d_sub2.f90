
module cmt3d_sub2

  use cmt3d_constants
  use cmt3d_variables
  use cmt3d_sub3

  implicit none

contains

  subroutine read_sac_info(file_s, file_o, kstnm, kcmpnm, knetwk, &
       azimuth, dist_deg, dist_km)

    character (len=*),intent(in) :: file_s, file_o
    character (len=*),intent(out) :: kstnm, kcmpnm, knetwk
    real,intent(out) :: azimuth, dist_deg, dist_km

    real :: b1, dt1, b2, dt2, b, dt
    real :: evla, evlo, stla, stlo, evdp, backazimuth
    integer :: npts1, npts2, npts, nerr
    character(len=1) :: cmp

    ! read synthetic
    call rsac1(file_s,syn_sngl,npts1,b1,dt1,NDATAMAX,nerr)
    if (nerr.ne.0) stop ' Error reading synthetic file'

    ! read observed
    call rsac1(file_o,data_sngl, npts2,b2,dt2,NDATAMAX,nerr)
    if (nerr.ne.0) stop ' Error reading data file '

    ! check sample rates are equal
    if (abs(dt1-dt2).gt.EPS5) stop 'Sampling rates differ, program stop !!!'
    dt = dble(dt1) 
    !write(*,*)'sampling rate dt=',dt

    ! check start times are equal (LQY: a difference of dt is probably too big)
    if(abs(b1-b2).gt.dt) stop ' start times differ, program stop !!!'
    b=dble(b1)

    ! set global npts to the npts of the shortest seismogram
    npts = min(npts1, npts2)

    ! DEBUG
    if (DEBUG) write(*,*) 'DEBUG : b, dt, npts ', b, dt, npts

    ! read event and station header parameters from observation file
    call getfhv('evla', evla, nerr)
    call getfhv('evlo', evlo, nerr)
    call getfhv('stla', stla, nerr)
    call getfhv('stlo', stlo, nerr)
    call getfhv('evdp', evdp, nerr)
    call getkhv('kstnm', kstnm, nerr)
    call getkhv('kcmpnm', kcmpnm, nerr)
    call getkhv('knetwk', knetwk, nerr)

    cmp=kcmpnm(3:3)

    if ((cmp .ne. 'Z') .and. (cmp .ne. 'T') .and. (cmp .ne. 'R')) &
         stop 'We only deal with Z, R, and T components at the moment'

    ! CHT: why does this output as:  FMP     BHR      CI(013, 064)?
    if (DEBUG) write(*,'(a, a,a,a)') '       sta, cmp, net    ', &
         trim(kstnm), trim(kcmpnm), trim(knetwk)

    ! calculate distances and azimuths (needs testing)
    call distaz(evla,evlo,stla,stlo,azimuth,backazimuth,dist_deg,dist_km)
    if (azimuth < 0 .or. azimuth > 360) stop 'check azimuth in (0,360)'


  end subroutine read_sac_info

  !-------------------------------------------------------------------

  subroutine compute_data_weights(kcmpnm,azimuth,dist_km,data_weights)

    character*8, dimension(:),intent(in) :: kcmpnm
    real, dimension(:), intent(in) :: azimuth, dist_km
    real*8,intent(out) :: data_weights(NWINMAX)

    real :: daz
    integer :: naz(NREGIONS), nwint, i, j, k
    character(len=8) :: comp_name
    real*8 :: cmp_weight(NWINMAX), dist_exp_weight(NWINMAX),max_data_weight


    daz = 360./NREGIONS
    naz = 1 ! start with a water level of 1 

    nwint = 0
    do i = 1, nfiles
       do j = 1, nwins(i)
          nwint = nwint + 1

          ! component weights
          comp_name=kcmpnm(i)
          if (comp_name(3:3) .eq. 'Z') then
             cmp_weight(nwint)=comp_z_weight
          else if (comp_name(3:3) .eq. 'R') then
             cmp_weight(nwint)=comp_r_weight
          else if (comp_name(3:3) .eq. 'T') then
             cmp_weight(nwint)=comp_t_weight
          endif

          ! dist weights
          if (comp_name(3:3) == 'T') then
             dist_exp_weight(nwint) = love_dist_weight
          else
             if (nwins(i) > 1 .and. j == 1) then
                dist_exp_weight(nwint) = pnl_dist_weight
             else
                dist_exp_weight(nwint) = rayleigh_dist_weight
             endif
          endif
       enddo

       ! azimuth counts
       k = floor(azimuth(i)/daz) + 1
       if (k < 0 .or. k > NREGIONS) stop 'Error binning azimuth'
       naz(k) = naz(k) + 1
    enddo

    if (nwint /= nwin_total) stop 'Error counting number of windows'
    if (DEBUG) write(*,*) 'Number of files in az bins: ', naz-1

    ! assemble data weights
    nwint = 0
    do i = 1, nfiles
       do j = 1, nwins(i)
          nwint = nwint + 1
          k = floor(azimuth(i)/daz) + 1
          data_weights(nwint) =  cmp_weight(nwint) &
               * ( (dist_km(i)/REF_DIST) ** dist_exp_weight(nwint) ) &
               / ( naz(k) ** az_exp_weight)
       enddo
    enddo

    ! normalization of data weights
    max_data_weight = maxval(data_weights(1:nwin_total))

    data_weights(1:nwin_total) = data_weights(1:nwin_total)/max_data_weight

  end subroutine compute_data_weights

  !---------------------------------------------------------------------------

  subroutine compute_A_b(syn_file,data_file,data_weight,tstart,tend,A1,b1,npar)

    character(len=*),intent(in) :: syn_file, data_file
    real*8,intent(in) :: data_weight, tstart, tend
    real*8, intent(out),dimension(:,:) :: A1
    real*8, intent(out),dimension(:) :: b1
    integer, intent(in) :: npar

    real :: t0, dt, t0_1, dt1
    integer :: npts, npts1, npts2, nerr,istart,iend, nshift
    integer :: i, j, istart_d, iend_d
    real, dimension(NDATAMAX,NPARMAX) :: dsyn_sngl
    character(len=150) :: dsyn_file
    real*8 :: dtt, tshift, dlna, cc


    ! read in data, syn
    call rsac1(data_file,data_sngl,npts1,t0,dt,NDATAMAX,nerr)
    call rsac1(syn_file,syn_sngl,npts2,t0,dt,NDATAMAX,nerr)
    npts=min(npts1,npts2)

    ! array indices of the start and end of the selected window
    istart = max(floor((tstart-t0)/dt),1)
    iend = min(ceiling((tend-t0)/dt) + 1,npts)
    if (istart >= iend) stop 'Check tstart and tend'

    if (station_correction) then
       data(1:npts) = dble(data_sngl(1:npts))
       syn(1:npts) = dble(syn_sngl(1:npts))
       dtt = dble(dt)
       ! matching syn(is:ie) with data(is+it:ie+it)
       call calc_criteria(data,syn,npts,istart,iend,dtt,tshift,cc,dlna)
       nshift = nint(tshift/dt)
       istart_d = istart + nshift
       iend_d = iend + nshift
    else
       istart_d = istart
       iend_d = iend
    endif

    ! read in dsyns
    do i = 1, npar
       dsyn_file = trim(syn_file) // '.' // trim(PAR_NAME(i))
       ! wierd enough, rsac1 can not detect the non-existence of dsyn_file
       call rsac1(dsyn_file,dsyn_sngl(:,i),npts1,t0_1,dt1,NDATAMAX,nerr)
       if (nerr /= 0) stop 'Error reading dsynthetics' 
       if (npts1 /= npts2 .or. abs(t0_1-t0) > EPS2 .or. abs(dt1-dt) > EPS5) then
          print *,  trim(dsyn_file),npts1,t0_1,dt1,'; nerr = ', nerr
          stop 'Check npts, b, dt of the derivative synthetics'
       endif
       if (i <= NM) then
          dsyn_sngl(1:npts,i) = dsyn_sngl(1:npts,i) / dcmt_par(i)
       else
          dsyn_sngl(1:npts,i) = (dsyn_sngl(1:npts,i) - syn_sngl(1:npts)) / dcmt_par(i)
       endif
    enddo
    
    ! compute A and b taking into account data_weights
    do j = 1, npar ! col
       do i = 1, j ! row
          A1(i,j) = data_weight * sum(dsyn_sngl(istart:iend,i)*dsyn_sngl(istart:iend,j))*dble(dt)
       enddo
       b1(j) = data_weight * sum((data_sngl(istart_d:iend_d)-syn_sngl(istart:iend))*dsyn_sngl(istart:iend,j))*dble(dt)
     
    enddo

    do j = 1, npar
       do i = j+1, npar
          A1(i,j) = A1(j,i)
       enddo
    enddo

  end subroutine compute_A_b

  !******************************************************************

  subroutine get_f_df(npar,A,b,m,lam,mstart,fij,f0)

    integer,intent(in) :: npar
    real*8,intent(in) :: A(:,:),b(:)
    real*8,intent(in) :: m(:),mstart(:)
    real*8,intent(in) :: lam(:)

    real*8,intent(out) :: fij(:,:),f0(:)

    real*8 :: dc1_dm(NM),dc2_dm(NM),dc2_dmi_dmj(NM,NM)
    integer :: i,j

    ! U_j's
    dc1_dm = (/ 1, 1, 1, 0, 0, 0 /)

    ! V_j's 
    dc2_dm(1) = m(2) * m(3) - m(6) ** 2
    dc2_dm(2) = m(1) * m(3) - m(5) ** 2
    dc2_dm(3) = m(1) * m(2) - m(4) ** 2
    dc2_dm(4) = 2 * m(5) * m(6) - 2 * m(3) * m(4)
    dc2_dm(5) = 2 * m(4) * m(6) - 2 * m(2) * m(5)
    dc2_dm(6) = 2 * m(4) * m(5) - 2 * m(1) * m(6)

    ! f(x^i) = H_jk (m_k^i -m_k^0) - b_j + lam_1 * U_j + lam_2 * V_j (A11)
    f0(1:npar) = matmul(A(1:npar,1:npar),m(1:npar)-mstart(1:npar)) - b(1:npar)
    f0(1:NM) = f0(1:NM) + lam(1) * dc1_dm(1:NM) + lam(2) * dc2_dm(1:NM)

    ! f_n+1 and f_n+2
    f0(npar+1) = m(1) + m(2) + m(3) 
    f0(npar+2) = m(1) * ( m(2) * m(3) - m(6) ** 2 ) &
         - m(4) * ( m(4) * m(3) - m(6) * m(5) ) &
         + m(5) * ( m(4) * m(6) - m(5) * m(2) )

    ! Y_jk
    dc2_dmi_dmj(1,:) = (/  0.d0,    m(3),    m(2),    0.d0,    0.d0,      -2*m(6) /)
    dc2_dmi_dmj(2,:) = (/ m(3),    0.0d0,    m(1),    0.d0,    -2*m(5),   0.0d0   /)
    dc2_dmi_dmj(3,:) = (/ m(2),    m(1),     0.0d0,   -2*m(4), 0.0d0,     0.0d0   /)
    dc2_dmi_dmj(4,:) = (/ 0.d0,    0.d0,     -2*m(4), -2*m(3),  2*m(6),   2*m(5) /)
    dc2_dmi_dmj(5,:) = (/ 0.d0,    -2*m(5),  0.0d0,   2*m(6),  -2*m(2),   2*m(4) /)
    dc2_dmi_dmj(6,:) = (/-2*m(6),  0.0d0,    0.0d0,   2*m(5),   2*m(4),  -2*m(1) /)

    ! f_jk = H_jk + lam_2 * Y_jk
    fij = 0.
    fij(1:npar,1:npar) = A(1:npar,1:npar) 
    fij(1:NM,1:NM) = fij(1:NM,1:NM) + lam(2) * dc2_dmi_dmj(1:NM,1:NM)
    fij(1:NM,npar+1) = dc1_dm
    fij(1:NM,npar+2) = dc2_dm
    fij(npar+1,1:NM) = dc1_dm
    fij(npar+2,1:NM) = dc2_dm

    return

  end subroutine get_f_df

  ! ------------------------------------------------------------------


  subroutine compute_new_syn(data_file,syn_file,npts,b,dt,dm)

    character(len=*),intent(in):: data_file, syn_file
    integer,intent(out) :: npts
    real*8 ,intent(out) :: b, dt
    real*8,intent(in) :: dm(:)

    real, dimension(NDATAMAX,NPARMAX) :: dsyn_sngl
    real, dimension(NDATAMAX) :: time
    integer :: nerr, i, npts1,npts2
    real :: b1, dt1
    character(len=150) :: dsyn_file
    character*8 :: kstnm,knetwk,kcmpnm

    ! read in data, syn
    call rsac1(data_file,data_sngl,npts1,b1,dt1,NDATAMAX,nerr)
    call rsac1(syn_file,syn_sngl,npts2,b1,dt1,NDATAMAX,nerr)
    npts=min(npts1,npts2)

    call getkhv('kstnm', kstnm, nerr)
    call getkhv('kcmpnm', kcmpnm, nerr)
    call getkhv('knetwk', knetwk, nerr)

    ! prepare b, dt, data(:), syn(:) for calc_criteria
    b=dble(b1)
    dt=dble(dt1)
    data(1:npts) = dble(data_sngl(1:npts))
    syn(1:npts) = dble(syn_sngl(1:npts))

    ! read in dsyns
    do i = 1, npar
       dsyn_file = trim(syn_file) // '.' // trim(PAR_NAME(i))
       call rsac1(dsyn_file,dsyn_sngl(:,i),npts1,b1,dt1,NDATAMAX,nerr)
       if (nerr /= 0) stop 'Error reading frechet derivative synthetics'
       if (npts1 /= npts2 .or. abs(b1-b) > EPS2 .or. abs(dt1-dt) > EPS5) &
            stop 'Check npts, b, dt of the derivative synthetics'
       if (i <= NM) then
          dsyn_sngl(1:npts,i) = dsyn_sngl(1:npts,i) / dcmt_par(i)
       else
          dsyn_sngl(1:npts,i) = (dsyn_sngl(1:npts,i) - syn_sngl(1:npts)) / dcmt_par(i)
       endif
    enddo

    ! update synthetics with linearized relation
    new_syn(1:npts) = syn(1:npts) + matmul(dsyn_sngl(1:npts,1:npar),dm(1:npar))

    ! output new synthetics as sac file
    if (write_new_syn) then
       syn_sngl(1:npts) = sngl(new_syn(1:npts))
       do i = 1, npts
          time(i) = b1 + (i-1)*dt1
       enddo
       call newhdr()
       call setkhv('kstnm',trim(kstnm),nerr)
       call setkhv('knetwk',trim(knetwk),nerr)
       call setkhv('kcmpnm',trim(kcmpnm),nerr)
       call setfhv('b',sngl(b),nerr)
       call setfhv('delta',sngl(dt),nerr)
       call setnhv('npts',npts,nerr)
       call setihv('iftype','ITIME',nerr)
       call setihv('iztype','IB',nerr)
       call setlhv('leven',1,nerr)
       call wsac0(trim(syn_file)//'.new',time(1:npts),syn_sngl(1:npts),nerr)
       if (nerr .ne. 0) stop 'Error writing new synthetics files'
    endif

  end subroutine compute_new_syn

  ! ===========================================================

  subroutine calc_criteria(d,s,npts,i1,i2,dt,tshift,cc_max,dlnA)

    double precision, dimension(*), intent(in) :: d, s
    integer, intent(in) :: npts,i1,i2
    double precision, intent(in) :: dt
    double precision, intent(out) ::  tshift,cc_max,dlnA

    double precision, dimension(NDATAMAX) :: d_win, s_win
    integer :: ishift

    ! do cross-correlation
    ! CHT: zero the data and synthetics outside the window (see comments in xcorr_calc)
    d_win(:) = 0. ; d_win(i1:i2) = d(i1:i2)
    s_win(:) = 0. ; s_win(i1:i2) = s(i1:i2)
    call xcorr_calc(d_win,s_win,npts,i1,i2,ishift,cc_max)

    ! cross-correlation time-shift
    tshift = ishift*dt

    ! calculate dlnA : definition of Dahlen and Baig (2002), Eq. 3,17,18 : dlnA = Aobs/Asyn - 1
    ! NOTE 1: these records are unshifted
    ! NOTE 2: this measurement will reflect any noise in the data, too
    dlnA = sqrt( ( sum( d_win(i1:i2)*d_win(i1:i2) )) / (sum( s_win(i1:i2)*s_win(i1:i2) )) ) - 1.0

  end subroutine calc_criteria

  ! ==========================================================

end module cmt3d_sub2