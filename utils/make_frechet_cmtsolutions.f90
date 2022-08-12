program make_frechet_cmtsolutions

 ! Usage: xmake_frechet_cmtsolutions cmt_file ddelta ddepth dmoment
 
  implicit none

  integer yr,jda,ho,mi
  double precision sec,t_cmt,hdur,elat,elon,depth
  double precision moment_tensor(6)
  character(len=150) cmt_file,dc_delta,dc_depth,dc_moment
  double precision DDELTA,DDEPTH,MOMENT
  character(len=3) par(9)

  integer iu,i,ios,lstr,mo,da,julian_day
  double precision mb,ms
  double precision elatp,elonp
  character(len=200) header
  character(len=5) datasource
  character(len=150) string

  call getarg(1,cmt_file)
  call getarg(2,dc_delta)
  call getarg(3,dc_depth)
  call getarg(4,dc_moment)

  if (trim(cmt_file) == '' .or. trim(dc_delta) == '' &
       .or. trim(dc_depth) == '' .or. trim(dc_moment) == '') then
     stop 'Usage: xmake_frechet_cmtsolutions cmt_file ddelta(in-degrees) ddepth(in km) dmoment(in dyne.cm)'
  endif
  read(dc_delta,*) ddelta
  read(dc_depth,*) ddepth
  read(dc_moment,*) moment

  par = (/ 'lat', 'lon','dep',&
       'Mrr', 'Mtt', &
       'Mpp', 'Mrt', &
       'Mrp', 'Mtp'/)

  open(1,file=cmt_file,iostat=ios,status='old')
  if(ios /= 0) stop 'error opening CMT file '

  do iu = 2, 10
     open(iu,file=trim(cmt_file)//'_'//trim(par(iu-1)),iostat=ios,status='unknown')
  enddo

  read(1,'(a)') header

  do iu=2,10
     write(iu,'(a)') trim(header)
  enddo

  ios=0
  do while(ios == 0)

     read(1,"(a)",iostat=ios) string

     if(ios == 0) then

        lstr=len_trim(string)

        if(string(1:10) == 'event name') then
           do iu=2,10
              write(iu,"(a)") string(1:lstr)
           enddo
        else if(string(1:10) == 'time shift') then
           read(string(12:lstr),*) t_cmt
           do iu=2,10
              write(iu,"(a)") string(1:lstr)
           enddo
        else if(string(1:13) == 'half duration') then
           read(string(15:lstr),*) hdur
           do iu=2,10
              write(iu,"(a)") string(1:lstr)
           enddo
        else if(string(1:8) == 'latitude') then
           read(string(10:lstr),*) elat
           elatp = elat + DDELTA
           if(elatp > 90.0) elatp = 180.0 - elatp
           write(2,"(a9,5x,f9.4)") string(1:9),elatp
           do iu=3,10
              write(iu,"(a)") string(1:lstr)
           enddo
        else if(string(1:9) == 'longitude') then
           read(string(11:lstr),*) elon
           write(2,"(a)") string(1:lstr)
           elonp = elon + DDELTA
           if(elonp > 180.0) elonp = elonp - 360.0
           write(3,"(a10,4x,f9.4)") string(1:10),elonp
           do iu=4,10
              write(iu,"(a)") string(1:lstr)
           enddo
        else if(string(1:5) == 'depth') then
           read(string(7:lstr),*) depth
           write(2,"(a)") string(1:lstr)
           write(3,"(a)") string(1:lstr)
           write(4,"(a6,8x,f9.4)") string(1:6),depth+DDEPTH
           do iu=5,10
              write(iu,"(a)") string(1:lstr)
           enddo
        else if(string(1:3) == 'Mrr') then
           read(string(5:lstr),*) moment_tensor(1)
           do iu=2,4
              write(iu,"(a)") string(1:lstr)
           enddo
           write(5,"(a4,4x,e15.6)") string(1:4),MOMENT
           write(6,"(a4,4x,e15.6)") string(1:4),0.0
           write(7,"(a4,4x,e15.6)") string(1:4),0.0
           write(8,"(a4,4x,e15.6)") string(1:4),0.0
           write(9,"(a4,4x,e15.6)") string(1:4),0.0
           write(10,"(a4,4x,e15.6)") string(1:4),0.0
        else if(string(1:3) == 'Mtt') then
           read(string(5:lstr),*) moment_tensor(2)
           do iu=2,4
              write(iu,"(a)") string(1:lstr)
           enddo
           write(5,"(a4,4x,e15.6)") string(1:4),0.0
           write(6,"(a4,4x,e15.6)") string(1:4),MOMENT
           write(7,"(a4,4x,e15.6)") string(1:4),0.0
           write(8,"(a4,4x,e15.6)") string(1:4),0.0
           write(9,"(a4,4x,e15.6)") string(1:4),0.0
           write(10,"(a4,4x,e15.6)") string(1:4),0.0
        else if(string(1:3) == 'Mpp') then
           read(string(5:lstr),*) moment_tensor(3)
           do iu=2,4
              write(iu,"(a)") string(1:lstr)
           enddo
           write(5,"(a4,4x,e15.6)") string(1:4),0.0
           write(6,"(a4,4x,e15.6)") string(1:4),0.0
           write(7,"(a4,4x,e15.6)") string(1:4),MOMENT
           write(8,"(a4,4x,e15.6)") string(1:4),0.0
           write(9,"(a4,4x,e15.6)") string(1:4),0.0
           write(10,"(a4,4x,e15.6)") string(1:4),0.0
        else if(string(1:3) == 'Mrt') then
           read(string(5:lstr),*) moment_tensor(4)
           do iu=2,4
              write(iu,"(a)") string(1:lstr)
           enddo
           write(5,"(a4,4x,e15.6)") string(1:4),0.0
           write(6,"(a4,4x,e15.6)") string(1:4),0.0
           write(7,"(a4,4x,e15.6)") string(1:4),0.0
           write(8,"(a4,4x,e15.6)") string(1:4),MOMENT
           write(9,"(a4,4x,e15.6)") string(1:4),0.0
           write(10,"(a4,4x,e15.6)") string(1:4),0.0
        else if(string(1:3) == 'Mrp') then
           read(string(5:lstr),*) moment_tensor(5)
           do iu=2,4
              write(iu,"(a)") string(1:lstr)
           enddo
           write(5,"(a4,4x,e15.6)") string(1:4),0.0
           write(6,"(a4,4x,e15.6)") string(1:4),0.0
           write(7,"(a4,4x,e15.6)") string(1:4),0.0
           write(8,"(a4,4x,e15.6)") string(1:4),0.0
           write(9,"(a4,4x,e15.6)") string(1:4),MOMENT
           write(10,"(a4,4x,e15.6)") string(1:4),0.0
        else if(string(1:3) == 'Mtp') then
           read(string(5:lstr),*) moment_tensor(6)
           do iu=2,4
              write(iu,"(a)") string(1:lstr)
           enddo
           write(5,"(a4,4x,e15.6)") string(1:4),0.0
           write(6,"(a4,4x,e15.6)") string(1:4),0.0
           write(7,"(a4,4x,e15.6)") string(1:4),0.0
           write(8,"(a4,4x,e15.6)") string(1:4),0.0
           write(9,"(a4,4x,e15.6)") string(1:4),0.0
           write(10,"(a4,4x,e15.6)") string(1:4),MOMENT
        endif

     endif

  enddo

  close(1)

  close(2)
  close(3)
  close(4)
  close(5)
  close(6)
  close(7)
  close(8)
  close(9)
  close(10)

end program make_frechet_cmtsolutions

