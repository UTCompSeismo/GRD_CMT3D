module cmt3d_sub4

  use cmt3d_constants
  implicit none

contains

  !==================================================================

  subroutine rotate_cmt(cmt_par,utm_zone,utm_center_x,utm_center_y,DIRECTION)

    ! DIRECTION = 1: entering cmt_par(1:6) - Mij, cmt_par(7:9) - dep, lon, lat
    ! cmt_par will always enter the code carrying [M(rtp)(rtp), dep, lat, lon] when DIRECTION = 1
    ! or leaving the code when DIRECTION = -1.
    ! However, cmt_par carries parameters of different meanings 
    ! when leaving at DIRECTION=1 and entering when DRECTION=-1,
    ! and one has to make sure how derivative synthetics are calculated 
    ! M(rtp)(rtp)'s, depth, lat,lon or X,Y,Z) correspondingly

    real*8, intent(inout) :: cmt_par(NPARMAX)
    real*8, intent(inout) :: utm_center_x, utm_center_y
    integer,intent(in) :: utm_zone
    integer,intent(in)  :: DIRECTION

    logical :: global_code, global_coord
    real*8, dimension(3,3) :: rmat
    real*8 :: th, phi, loc(3), gl(3), moment(3,3), elon,elat,edep
    real*8 :: ee, en, delon, delat


    ! check input arguments
    if (utm_zone > 60 .or. utm_zone < -1) then
       stop 'Error utm_zone ([-1,0] for global or [1,60])'
    else if (utm_zone == -1) then
       global_code = .true.; global_coord=.true.
    else if (utm_zone == 0) then 
       global_code = .true.; global_coord=.false.
    else
       global_code = .false.; global_coord=.false. !(unused)
    endif
    if (DIRECTION /= 1 .and. DIRECTION /= -1) stop 'Error DIRECTION (1 or -1)'

    if (global_code) then 
       if (DIRECTION == 1) then ! local to global coordinates (dep,lon/E,lat/N) -> (r, th, phi)
          edep=cmt_par(7); elon=cmt_par(8); elat=cmt_par(9)
          loc=(/R_EARTH-edep,0.d1,0.d1/)  ! local [R,T,P]
          utm_center_x=elon; utm_center_y=elat ! this info needs to be recorded 
          if (global_coord) then
             ! rmat converts local {r,t,p} (or {Z,S,E}) coord. to global {X,Y,Z} coord.
             ! X_global= rmat * X_local, then M_global= R * M_local * R^T
             call calc_rot_matrix(elon,elat,rmat)
             moment(1,1)=cmt_par(1); moment(2,2)=cmt_par(2); moment(3,3)=cmt_par(3)
             moment(1,2)=cmt_par(4); moment(1,3)=cmt_par(5); moment(2,3)=cmt_par(6)
             moment(2,1)=moment(1,2); moment(3,1)=moment(1,3); moment(3,2)=moment(2,3)
             moment=matmul(rmat, matmul(moment,transpose(rmat)))
             loc=matmul(rmat,loc)  ! global [X,Y,Z]
             cmt_par(1)=moment(1,1); cmt_par(2)=moment(2,2); cmt_par(3)=moment(3,3)
             cmt_par(4)=moment(1,2); cmt_par(5)=moment(1,3); cmt_par(6)=moment(2,3)
          endif
          cmt_par(7:9)=loc(1:3)  ![R,T,P] in local or [X,Y,Z] in global
          ! derivatives should be calculated for above output coordinate system
       else ! global to local coordinates
          if (global_coord) then
             gl=cmt_par(7:9)/sqrt(sum(cmt_par(7:9)**2)) ! global [X,Y,Z]
             th=acos(gl(3))
             if (abs(th)<EPS5 .or. abs(th-pi)<EPS5) then
                phi=0
             else
                phi=atan2(gl(2)/sin(th),gl(1)/sin(th))
             endif
             elon=phi*180/PI; elat=90-th*180/PI
             if (elon > 180) elon=elon-360
             call calc_rot_matrix(elon,elat,rmat)
             moment(1,1)=cmt_par(1); moment(2,2)=cmt_par(2); moment(3,3)=cmt_par(3)
             moment(1,2)=cmt_par(4); moment(1,3)=cmt_par(5); moment(2,3)=cmt_par(6)
             moment(2,1)=moment(1,2); moment(3,1)=moment(1,3); moment(3,2)=moment(2,3)
             moment=matmul(transpose(rmat), matmul(moment,rmat))
             loc=matmul(transpose(rmat),cmt_par(7:9))  ! moment, loc in [R,T,P] local coord
             cmt_par(1)=moment(1,1); cmt_par(2)=moment(2,2); cmt_par(3)=moment(3,3)
             cmt_par(4)=moment(1,2); cmt_par(5)=moment(1,3); cmt_par(6)=moment(2,3)
             cmt_par(7)=R_EARTH-loc(1); cmt_par(8)=elon; cmt_par(9)=elat  ! [depth,lon,lat]
             if (abs(loc(2)) > EPS5 .or. abs(loc(3)) > EPS5) stop 'Error loc(2,3)'
          else ! global cmt, but only need to update new location
             call calc_rot_matrix(utm_center_x,utm_center_y,rmat) !(elon,elat,rmat)
             gl=matmul(rmat,cmt_par(7:9)); gl=gl/sqrt(sum(gl**2)) ! local [R,T,P] to global [X,Y,Z]
             th=acos(gl(3))
             if (abs(th)<EPS5 .or. abs(th-pi)<EPS5) then
                phi=0
             else
                phi=atan2(gl(2)/sin(th),gl(1)/sin(th))
             endif 
             ! convert back to [dep,lon,lat]
             elon=phi*180/PI; elat=90-th*180/PI; edep=R_EARTH-cmt_par(7)
             if (elon > 180) elon=elon-360
             cmt_par(7)=edep; cmt_par(8)=elon; cmt_par(9)=elat
          endif
       endif

    else  ! regional UTM coordinates (mostly because of implementation in SEM)
       if (DIRECTION == 1) then 
          delon=cmt_par(8); delat=cmt_par(9)
          call utm_geo(delon,delat,ee,en,utm_zone,ILONGLAT2UTM,SUPPRESS_UTM_PROJECTION)
          ee=ee-utm_center_x; en=en-utm_center_y
          cmt_par(8)=ee; cmt_par(9)=en
       else
          ee=cmt_par(8); en=cmt_par(9)
          ee=ee+utm_center_x; en=en+utm_center_y
          call utm_geo(delon,delat,ee,en,utm_zone,IUTM2LONGLAT,SUPPRESS_UTM_PROJECTION)
          cmt_par(8)=delon; cmt_par(9)=delat
       endif
    endif

  end subroutine rotate_cmt

  !==================================================================

  subroutine calc_rot_matrix(elon,elat,rmat)

    real*8,intent(in) :: elon, elat
    real*8,intent(out) :: rmat(3,3)
    real*8 :: th,phi

    th=(90-elat)*PI/180
    phi=elon*PI/180

    rmat(1,1)=dsin(th)*dcos(phi)
    rmat(1,2)=dcos(th)*dcos(phi)
    rmat(1,3)=-dsin(phi)
    rmat(2,1)=dsin(th)*dsin(phi)
    rmat(2,2)=dcos(th)*dsin(phi)
    rmat(2,3)=dcos(phi)
    rmat(3,1)=dcos(th)
    rmat(3,2)=-dsin(th)
    rmat(3,3)=0

  end subroutine calc_rot_matrix

end module cmt3d_sub4
