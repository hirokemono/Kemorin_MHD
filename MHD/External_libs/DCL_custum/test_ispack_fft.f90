!
!
      program test_ispack_fft
!
      use m_precision
      use m_constants
!
      use ispack_FFT_wrapper
!
      implicit none
!
      integer(kind = kint), parameter :: ndat = 120
      real(kind = kreal) :: xsm(4,ndat), xcm(4,ndat)
      real(kind = kreal) :: xst(4,ndat), xct(4,0:ndat)
      real(kind = kreal) :: x(8,ndat)
      real(kind = kreal) :: pi
!
      real(kind = kreal) :: work(ndat,8)
      real(kind = kreal), allocatable :: t(:)
      integer(kind = kint) :: it(5*ndat)
!
      integer(kind = kint), parameter :: ncomp_s = 4, ncomp_c = 4
      integer(kind = kint), parameter :: ncomp = 8
      integer(kind = kint), parameter :: Nsmp = 2
      integer(kind = kint), parameter :: Nstack_c(0:2) = (/0,2,4/)
      integer(kind = kint), parameter :: Nstacksmp(0:2) = (/0,4,8/)
!
      integer(kind = kint) :: i, j
!
!
      pi = four*atan(one)
!
      do i = 1, ndat
        do j = 1, ncomp_c
          xsm(j,i) = sin( dble(j)*pi*dble(2*i-1) / dble(2*ndat) )
          xcm(j,i) = cos( dble(j)*pi*dble(2*i-1) / dble(2*ndat) )
          x(2*j-1,i) = sin( dble(2*j)*pi*dble(i-1) / dble(ndat) )
          x(2*j,  i) = cos( dble(2*j)*pi*dble(i-1) / dble(ndat) )
        end do
      end do
      do i = 1, ndat-1
        do j = 1, ncomp_c
          xst(j,i) = sin( dble(j)*pi*dble(i) / dble(ndat) )
        end do
      end do
      do i = 0, ndat
        do j = 1, ncomp_c
          xct(j,i) = cos( dble(j)*pi*dble(i) / dble(ndat) )
        end do
      end do
!
        write(25,*) 'original'
      do i = 1, ndat
        write(25,'(1p8E25.15e3)') x(1:8,i)
      end do
!
      call verify_work_4_ispack(Nsmp, Nstacksmp, ndat)
      call FTTRUF_kemo(Nsmp, Nstacksmp, ncomp, ndat, x)
!
        write(25,*) 'spectr'
      do i = 1, ndat
        write(25,'(1p8E25.15e3)') x(1:8,i)
      end do
!
!
      call FTTRUB_kemo(Nsmp, Nstacksmp, ncomp, ndat, x)

        write(25,*) 'reversed'
      do i = 1, ndat
        write(25,'(1p8E25.15e3)') x(1:8,i)
      end do
!
      close(25)
!
      end program test_ispack_fft
