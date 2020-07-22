!
      module t_fft_test_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      type fft_test_data
        integer(kind = kint) :: nfld = 6, ngrd = 512
        integer(kind = kint), allocatable :: nstack(:)
        real(kind = kreal), allocatable :: x(:,:)
        real(kind = kreal), allocatable :: y(:,:)
        real(kind = kreal), allocatable :: z(:,:)
      end type fft_test_data
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_fft_test_data(ngrid, ftst)
!
      integer(kind = kint), intent(in) ::  ngrid
      type(fft_test_data), intent(inout) :: ftst
!
      real(kind = kreal) :: pi
      integer(kind = kint) :: i
!
      pi = four*atan(one)
!
      np_smp = 2
      allocate(ftst%nstack(0:np_smp))
      ftst%nstack(0:np_smp) = (/0,3,6/)
!
      ftst%nfld = 6
      ftst%ngrd = ngrid
!
      allocate(ftst%x(ftst%nfld,ftst%ngrd))
      allocate(ftst%y(ftst%nfld,ftst%ngrd))
      allocate(ftst%z(ftst%nfld,ftst%ngrd))
!
!$omp parallel do
      do i = 1, ftst%ngrd
        ftst%x(1,i) = 10.0d0
        ftst%x(2,i) = 2.0d0 * sin(two*pi * dble(i-1) / dble(ftst%ngrd))
        ftst%x(3,i) = 3.0d0 * cos(two*pi * dble(i-1) / dble(ftst%ngrd))
        ftst%x(4,i)                                                     &
     &        = -4.0d0 * sin(3.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))
        ftst%x(5,i)                                                     &
     &        = -5.0d0 * cos(4.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))
        ftst%x(6,i)                                                     &
     &        =  6.0d0 * sin(10.0d0*two*pi * dble(i-1)/dble(ftst%ngrd)) &
     &         + 1.5d0 * cos( 8.0d0*two*pi * dble(i-1)/dble(ftst%ngrd))
      end do
!$omp end parallel do
!
      end subroutine init_fft_test_data
!
! ------------------------------------------------------------------
!
      subroutine dealloc_fft_test_data(ftst)
!
      type(fft_test_data), intent(inout) :: ftst
!
      deallocate(ftst%x, ftst%y, ftst%z, ftst%nstack)
!
      end subroutine dealloc_fft_test_data
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine write_fft_test_data(file_name, ftst)
!
      character(len = *), intent(in) :: file_name
      type(fft_test_data), intent(in) :: ftst
!
      integer(kind = kint) :: i, j, k
!
!
      open(15,file=file_name)
      do j = 1, ftst%nfld
          write(15,'(a,i5)')                                            &
     &         'i, k, result_back, result_fw, original', j
        do i = 1, ftst%ngrd
          k = ((i+1)/2-1) * (-1)**mod((i-ione),itwo)
          write(15,'(2i5,1p3E25.15e3)')                                 &
     &          i, k, ftst%z(j,i), ftst%y(j,i), ftst%x(j,i)
        end do
      end do
      close(15)
!
      end subroutine write_fft_test_data
!
! ------------------------------------------------------------------
!
      end module t_fft_test_data
