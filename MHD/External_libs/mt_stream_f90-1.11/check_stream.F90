program testdc
#ifdef _DEBUG_
  use mt_stream_debug
#else
  use mt_stream
#endif
  implicit none
  integer, parameter :: NSTREAM=123
  type(mt_state) :: mts(0:NSTREAM-1)
  integer :: iseed = 123456789
  integer :: iseeda(4) = (/ Z'123', Z'234', Z'345', Z'456' /)
  integer :: i
  integer :: k
  integer(8) :: jj,sumi
  real(8) :: rr,sum
  integer :: KK,iw,ir,im,in,j,is,it
  integer :: jp = 16

  call set_mt19937
  call new(mts(0))
!  call init(mts,iseed)  ! init by scalar
  call init(mts(0),iseeda)  ! init by array
  call print(mts(0))

  write(*,'("@ int32")')
  sumi = 0
  do i=1,1000
    k = genrand_int32(mts(0))
!    write(*,'("@ ",Z8)')k
    jj = k
    if (jj < 0) jj = jj + 2_8**32  ! convert to unsigned int
    sumi = sumi + jj
    write(*,'("@ ",Z8,I11)')k,jj
  enddo
  write(*,'("@ total sum of int32 ",I20)')sumi

  sum = 0.0d0
  write(*,'("@ double1")')
  do i=1,1000
    rr = genrand_double1(mts(0))
    sum = sum + rr
    write(*,'("@ ",Z16,F19.16)')rr,rr
  enddo
  write(*,'("@ total sum of double1 ",ES22.16)')sum

  sum = 0.0d0
  write(*,'("@ double2")')
  do i=1,1000
    rr = genrand_double2(mts(0))
    sum = sum + rr
    write(*,'("@ ",Z16,F19.16)')rr,rr
  enddo
  write(*,'("@ total sum of double2 ",ES22.16)')sum

  sum = 0.0d0
  write(*,'("@ double3")')
  do i=1,1000
    rr = genrand_double3(mts(0))
    sum = sum + rr
    write(*,'("@ ",Z16,F19.16)')rr,rr
  enddo
  write(*,'("@ total sum of double3 ",ES22.16)')sum

  sum = 0.0d0
  write(*,'("@ double4")')
  do i=1,1000
    rr = genrand_double4(mts(0))
    sum = sum + rr
    write(*,'("@ ",Z16,F19.16)')rr,rr
  enddo
  write(*,'("@ total sum of double4 ",ES22.16)')sum

#ifndef _CHECK_SAVEREAD_
  do is=1,NSTREAM-1
    call create_stream(mts(0),mts(is),is)
  enddo
  do is=0,NSTREAM-1
    call print(mts(is))
  enddo

#if defined(_CHECK_STREAM_) && defined(_DEBUG_)
!
! compare stream_id=0 and others
!
  is=0
  write(*,'("@ first 100 random integers from stream(",I4,")")')is
  write(*,'("@ int32")')
  do i=1,100
     k = genrand_int32(mts(is))
    jj = k
    if (jj < 0) jj = jj + 2_8**32
    write(*,'("@ ",Z8,I11)')k,jj
  enddo
  do i=101,2**jp ! drop remainder of stream_id=0
     k = genrand_int32(mts(is))
  enddo
  do is=1,NSTREAM-1
    write(*,'("@ first 100 random integers from stream(",I4,")")')is
    write(*,'("@ int32")')
    do i=1,100
       j = genrand_int32(mts(0))
       k = genrand_int32(mts(is))
      jj = k
      if (jj < 0) jj = jj + 2_8**32
      write(*,'("@ ",Z8,I11,3I12)')k,jj,k,j,k-j  ! compare to stream_id=0
    enddo
    do i=101,2**jp ! drop remainder of stream_id=0
       k = genrand_int32(mts(0))
    enddo
  enddo
#else
  do is=0,NSTREAM-1
    write(*,'("@ first 100 random integers from stream(",I4,")")')is
    write(*,'("@ int32")')
    do i=1,100
       k = genrand_int32(mts(is))
      jj = k
      if (jj < 0) jj = jj + 2_8**32
      write(*,'("@ ",Z8,I11)')k,jj
    enddo
  enddo
#endif

  do is=0,NSTREAM-1
    call delete(mts(is))
  enddo
#endif

#if defined(_CHECK_SAVEREAD_) && defined(_DEBUG_)
  call new(mts(0))
  call init(mts(0),iseeda)
  call print(mts(0))
  do i=1,50
    k = genrand_int32(mts(0))
    jj = k
    if (jj < 0) jj = jj + 2_8**32
    write(*,'("S ",Z8,I11)')k,jj
  enddo
  open(11,file="mt_state_save.dat",form="unformatted")
  call save(mts(0),11)
  close(11)
  call delete(mts(0))
  call new(mts(0))
  open(11,file="mt_state_save.dat",form="unformatted")
  call read(mts(0),11)
  close(11)
  call print(mts(0))
  do i=1,50
    k = genrand_int32(mts(0))
    jj = k
    if (jj < 0) jj = jj + 2_8**32
    write(*,'("S ",Z8,I11)')k,jj
  enddo
  call delete(mts(0))

  call new(mts(0))
  call init(mts(0),iseeda)
  call print(mts(0))
  do i=1,100
    k = genrand_int32(mts(0))
    jj = k
    if (jj < 0) jj = jj + 2_8**32
    write(*,'("L ",Z8,I11)')k,jj
  enddo
  call delete(mts(0))
#endif

  stop
end program
