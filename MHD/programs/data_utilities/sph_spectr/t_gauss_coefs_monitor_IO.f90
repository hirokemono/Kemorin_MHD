!>@file   t_gauss_coefs_monitor_IO.f90
!!@brief  module t_gauss_coefs_monitor_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays for gauss coefficients output
!!
!!@verbatim
!!        subroutine alloc_gauss_coef_monitor(gauss_IO)
!!        subroutine alloc_gauss_coefs_series(n_step, gauss_IO)
!!        subroutine dealloc_gauss_coef_monitor(gauss_IO)
!!        subroutine dealloc_gauss_coefs_series(gauss_IO)
!!        integer(kind = kint), intent(in) :: n_step
!!        type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!!
!!      subroutine copy_to_gauss_coefs_series                           &
!!     &         (icou, i_step, time, gauss_IO)
!!        integer(kind = kint), intent(in) :: icou
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!!@endverbatim
!
      module t_gauss_coefs_monitor_IO
!
      use m_precision
      use m_constants
      use t_pickup_sph_spectr_data
!
      implicit  none
!
      type picked_gauss_coefs_IO
!>        Radius for the gauss coefficients
        real(kind = kreal) :: radius_gauss
!
!>        Number of modes of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_mode =  0
!>        monitoring spectrum
        real(kind = kreal), allocatable :: gauss_coef(:)
!>        Number of modes of Gauss coefficients to be evaluated
!>        Name of Gauss coefficients  (g_{l}^{m} or h_{l}^{m})
        character(len=kchara), allocatable :: gauss_coef_name(:)
!
!>        Number of time series
        integer(kind = kint) :: n_step = 0
!>        Number of data for each step
        integer(kind = kint), allocatable :: i_step(:)
!>        time
        real(kind = kreal), allocatable :: d_time(:)
!>        Gauss coefficient time series
        real(kind = kreal), allocatable :: d_gauss(:,:)
      end type picked_gauss_coefs_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_gauss_coef_monitor(gauss_IO)
!
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
!
      allocate(gauss_IO%gauss_coef_name(gauss_IO%num_mode) )
      allocate(gauss_IO%gauss_coef(gauss_IO%num_mode) )
!
      if(gauss_IO%num_mode .le. 0) return
!$omp parallel workshare
      gauss_IO%gauss_coef(1:gauss_IO%num_mode) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_gauss_coef_monitor
!
! -----------------------------------------------------------------------
!
      subroutine alloc_gauss_coefs_series(n_step, gauss_IO)
!
      integer(kind = kint), intent(in) :: n_step
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
!
      gauss_IO%n_step = n_step
!
      allocate(gauss_IO%i_step(gauss_IO%n_step))
      allocate(gauss_IO%d_time(gauss_IO%n_step))
      allocate(gauss_IO%d_gauss(gauss_IO%num_mode,gauss_IO%n_step))
!
      if(gauss_IO%n_step .gt. 0) then
        gauss_IO%i_step = -1
        gauss_IO%d_time = 0.0d0
        gauss_IO%d_gauss = 0.0d0
      end if
!
      end subroutine alloc_gauss_coefs_series
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_coef_monitor(gauss_IO)
!
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
      deallocate(gauss_IO%gauss_coef_name, gauss_IO%gauss_coef)
!
      end subroutine dealloc_gauss_coef_monitor
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_coefs_series(gauss_IO)
!
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
      deallocate(gauss_IO%i_step, gauss_IO%d_time, gauss_IO%d_gauss)
!
      end subroutine dealloc_gauss_coefs_series
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_to_gauss_coefs_series                             &
     &         (icou, i_step, time, gauss_IO)
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(picked_gauss_coefs_IO), intent(inout) :: gauss_IO
!
!
      gauss_IO%i_step(icou) = i_step
      gauss_IO%d_time(icou) = time
!$omp parallel workshare
      gauss_IO%d_gauss(1:gauss_IO%num_mode,icou)                        &
     &                 = gauss_IO%gauss_coef(1:gauss_IO%num_mode)
!$omp end parallel workshare
!
      end subroutine copy_to_gauss_coefs_series
!
! -----------------------------------------------------------------------
!
      end module t_gauss_coefs_monitor_IO
