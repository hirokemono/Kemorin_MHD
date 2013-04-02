!
      program test_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use FFT_selector
!
      implicit none
!
      integer(kind = kint), parameter :: nstack(0:2) = (/0,3,6/)

      integer(kind = kint), parameter :: nfld = 6, ngrd = 512
      real(kind = kreal), allocatable :: x(:,:)
      real(kind = kreal), allocatable :: y(:,:)
      real(kind = kreal), allocatable :: z(:,:)
      real(kind = kreal) :: pi
!
      integer(kind = kint) :: i, j, k
!
!
      iflag_debug = 1
      np_smp = 2
!
      write(*,*) 'select FFT library'
      write(*,*) '0: ISPACK'
      write(*,*) '1: FFTPACK5'
      write(*,*) '2: FFTW3 (if avaiable)'
      read(*,*) iflag_FFT
!
      pi = four*atan(one)
!
      allocate( x(nfld,ngrd) )
      allocate( y(nfld,ngrd) )
      allocate( z(nfld,ngrd) )
!
      do i = 1, ngrd
        x(1,i) = 10.0d0
        x(2,i) = 2.0d0 * sin(two*pi * dble(i-1) / dble(ngrd) )
        x(3,i) = 3.0d0 * cos(two*pi * dble(i-1) / dble(ngrd) )
        x(4,i) =-4.0d0 * sin(3.0d0*two*pi * dble(i-1) / dble(ngrd) )
        x(5,i) =-5.0d0 * cos(4.0d0*two*pi * dble(i-1) / dble(ngrd) )
        x(6,i) = 6.0d0 * sin(10.0d0*two*pi * dble(i-1) / dble(ngrd) )   &
     &         + 1.5d0 * cos( 8.0d0*two*pi * dble(i-1) / dble(ngrd) )
      end do
!
      z(1:nfld,1:ngrd) = x(1:nfld,1:ngrd)
!
      call initialize_FFT_select(np_smp, nstack, ngrd)
!
      call forward_FFT_select(np_smp, nstack, nfld, ngrd, x)
!
      y(1:nfld,1:ngrd) = x(1:nfld,1:ngrd)
!
      call backward_FFT_select(np_smp, nstack, nfld, ngrd, x)
!
      open(15,file='fft_test.dat')
      do j = 1, nfld
          write(15,'(a,i5)')                                            &
     &         'i, k, result_back, result_fw, original', j
        do i = 1, ngrd
          k = ((i+1)/2-1) * (-1)**mod((i-1),2)
          write(15,'(2i5,1p3E25.15e3)') i, k, z(j,i), y(j,i), x(j,i)
        end do
      end do
      close(15)
!
      stop
      end program test_FFT

