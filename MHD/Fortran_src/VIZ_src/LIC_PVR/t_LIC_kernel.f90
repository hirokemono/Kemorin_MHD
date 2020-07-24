!>@file   t_LIC_kernel.f90
!!@brief  module t_LIC_kernel
!!
!!@author Y. Liao and H. Matsui
!!@date Programmed by Y. Liao in Apr. 2018
!!      Modified by H. Matsui in Apr. 2020
!
!> @brief Construct kernel data for LIC
!!
!!@verbatim
!!      subroutine set_control_LIC_kernel(kernel_ctl, knl)
!!        type(lic_kernel_ctl), intent(in) :: kernel_ctl
!!        type(LIC_kernel), intent(inout) :: knl
!!      subroutine sel_const_LIC_kernel(knl)
!!      subroutine bcast_LIC_kernel(knl)
!!      subroutine dealloc_LIC_kernel(knl)
!!        type(LIC_kernel), intent(inout) :: knl
!!@endverbatim
!
      module t_LIC_kernel
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
!
      character(len = kchara), parameter, private                       &
     &                        :: cflag_gaussian = 'gaussian'
      character(len = kchara), parameter, private                       &
     &                        :: cflag_triangle = 'triangle'
!
      integer(kind = kint), parameter :: iflag_gaussian =    0
      integer(kind = kint), parameter :: iflag_triangle =    1
!
      character(len = kchara), parameter                                &
     &                        :: cflag_by_lengh =   'length'
      character(len = kchara), parameter                                &
     &                        :: cflag_by_e_count = 'element_count'
!
      integer(kind = kint), parameter :: iflag_by_lengh =   0
      integer(kind = kint), parameter :: iflag_by_e_count = 1
!
      type LIC_kernel
        character(len=kchara) :: kernel_type_name
        integer(kind = kint) :: iflag_kernel_type
!
        real(kind = kreal) :: x_peak
        real(kind = kreal) :: sigma
!
        real(kind = kreal) :: half_lengh = 0.5d0
        real(kind = kreal) :: alength
!
        integer(kind = kint) :: n_knl = 257
        real(kind = kreal), allocatable :: x_ary(:)
        real(kind = kreal), allocatable :: k_ary(:)
!
        integer(kind = kint) :: iflag_trace_type
        integer(kind = kint) :: max_trace_count
      end type LIC_kernel
!
      private :: alloc_LIC_kernel, check_LIC_kernel
      private :: cal_gaussian_kernel, cal_triangle_kernel
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_LIC_kernel(kernel_ctl, knl)
!
      use t_control_data_LIC_kernel
      use skip_comment_f
!
      type(lic_kernel_ctl), intent(in) :: kernel_ctl
      type(LIC_kernel), intent(inout) :: knl
!
      character(len = kchara) :: tmpchara
!
!
      knl%kernel_type_name =  cflag_gaussian
      knl%iflag_kernel_type = iflag_gaussian
      if(kernel_ctl%kernel_type_ctl%iflag .gt. 0) then
        tmpchara = kernel_ctl%kernel_type_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_triangle)) then
          knl%kernel_type_name =  cflag_triangle
          knl%iflag_kernel_type = iflag_triangle
        end if
      end if
!
      knl%sigma = 0.22
      if(kernel_ctl%kernel_sigma_ctl%iflag .gt. 0) then
        knl%sigma = kernel_ctl%kernel_sigma_ctl%realvalue
      end if
!
      knl%x_peak = zero
      if(kernel_ctl%kernel_peak_ctl%iflag .gt. 0) then
        knl%x_peak = kernel_ctl%kernel_peak_ctl%realvalue
      end if
!
      knl%n_knl = 256
      if(kernel_ctl%kernel_resolution_ctl%iflag .gt. 0) then
        knl%n_knl = kernel_ctl%kernel_resolution_ctl%intvalue
      end if
!
      knl%half_lengh = half
      if(kernel_ctl%half_length_ctl%iflag .gt. 0) then
        knl%half_lengh = kernel_ctl%half_length_ctl%realvalue
      end if
      knl%alength = one / (two * knl%half_lengh)
!
      knl%iflag_trace_type = iflag_by_lengh
      if(kernel_ctl%trace_length_mode_ctl%iflag .gt. 0) then
        tmpchara = kernel_ctl%trace_length_mode_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_by_e_count)) then
          knl%iflag_trace_type = iflag_by_e_count
        end if
      end if
!
      knl%max_trace_count =  20
      if(kernel_ctl%max_trace_count_ctl%iflag .gt. 0) then
        knl%max_trace_count = kernel_ctl%max_trace_count_ctl%intvalue
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'knl%iflag_kernel_type', knl%iflag_kernel_type
        write(*,*) 'knl%x_peak', knl%x_peak
        write(*,*) 'knl%sigma', knl%sigma
        write(*,*) 'knl%n_knl', knl%n_knl
        write(*,*) 'knl%half_lengh', knl%half_lengh
!
        write(*,*) 'knl%iflag_trace_type', knl%iflag_trace_type
        write(*,*) 'knl%max_trace_count', knl%max_trace_count
      end if
!
      end subroutine set_control_LIC_kernel
!
!  ---------------------------------------------------------------------
!
      subroutine sel_const_LIC_kernel(knl)
!
      type(LIC_kernel), intent(inout) :: knl
!
!
      call alloc_LIC_kernel(knl)
!
      if(knl%iflag_kernel_type .eq. iflag_triangle) then
        call cal_triangle_kernel(knl%half_lengh, knl%x_peak,            &
     &      knl%n_knl, knl%x_ary, knl%k_ary)
      else
        call cal_gaussian_kernel(knl%half_lengh, knl%x_peak, knl%sigma, &
     &      knl%n_knl, knl%x_ary, knl%k_ary)
      end if
!
      if(iflag_debug .gt. 0) call check_LIC_kernel(knl)
!
      end subroutine sel_const_LIC_kernel
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_LIC_kernel(knl)
!
      type(LIC_kernel), intent(inout) :: knl
!
      deallocate(knl%x_ary, knl%k_ary)
!
      end subroutine dealloc_LIC_kernel
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_LIC_kernel(knl)
!
      use transfer_to_long_integers
      use calypso_mpi_real
!
      type(LIC_kernel), intent(inout) :: knl
!
!
      call MPI_BCAST(knl%iflag_kernel_type, 1, CALYPSO_INTEGER,         &
     &               0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(knl%kernel_type_name, kchara, CALYPSO_CHARACTER,   &
     &               0, CALYPSO_COMM, ierr_MPI)
!
      call MPI_BCAST(knl%x_peak, 1, CALYPSO_REAL,                       &
     &               0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(knl%sigma, 1, CALYPSO_REAL,                        &
     &               0, CALYPSO_COMM, ierr_MPI)
!
      call MPI_BCAST(knl%n_knl, 1, CALYPSO_INTEGER,                     &
     &               0, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .gt. 0) call alloc_LIC_kernel(knl)
      call calypso_mpi_bcast_real(knl%k_ary, cast_long(knl%n_knl), 0)
!
      end subroutine bcast_LIC_kernel
!
!  ---------------------------------------------------------------------
!
      subroutine check_LIC_kernel(knl)
!
      type(LIC_kernel), intent(in) :: knl
!
      integer(kind = kint) :: i
!
      do i = 1, knl%n_knl
        write(*,*) i, knl%x_ary(i), knl%k_ary(i)
      end do
!
      end subroutine check_LIC_kernel
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_LIC_kernel(knl)
!
      type(LIC_kernel), intent(inout) :: knl
!
!
      allocate(knl%x_ary(knl%n_knl))
      allocate(knl%k_ary(knl%n_knl))
!
!$omp parallel workshare
      knl%x_ary(1:knl%n_knl) = 0.0d0
      knl%k_ary(1:knl%n_knl) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_LIC_kernel
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_gaussian_kernel                                    &
     &         (half_len, x_peak, sigma, n_knl, x_ary, k_ary)
!
      real(kind = kreal), intent(in) :: half_len, x_peak, sigma
      integer(kind = kint), intent(in) :: n_knl
      real(kind = kreal), intent(inout) :: x_ary(n_knl)
      real(kind = kreal), intent(inout) :: k_ary(n_knl)

      real(kind = kreal) :: pi, dnorm, e_func
      integer(kind = kint) :: i
!
!
      pi = four * atan(one)
      dnorm = one / (sigma * sqrt(two * pi))
!
!$omp parallel do private(i)
      do i = 1, n_knl
        x_ary(i) = half_len * (two * dble(i-1) / dble(n_knl-1) - one)
        k_ary(i) = dnorm * exp(-half * ((x_ary(i)-x_peak) / sigma)**2)
      end do
!$omp end parallel do
!
      dnorm = one / dble(3 * n_knl)
      e_func = 0.0d0
      do i = 1, n_knl-2, 2
        e_func = e_func                                                 &
     &          + dnorm * (k_ary(i) + four * k_ary(i+1) + k_ary(i+2))
      end do
      dnorm = one / e_func
!
!$omp parallel do private(i)
      do i = 1, n_knl
        k_ary(i) = dnorm * k_ary(i)
      end do
!$omp end parallel do
!
      end subroutine cal_gaussian_kernel
!
!  ---------------------------------------------------------------------
!
      subroutine cal_triangle_kernel                                    &
     &         (half_len, x_peak, n_knl, x_ary, k_ary)
!
      real(kind = kreal), intent(in) :: half_len, x_peak
      integer(kind = kint), intent(in) :: n_knl
      real(kind = kreal), intent(inout) :: x_ary(n_knl)
      real(kind = kreal), intent(inout) :: k_ary(n_knl)

      real(kind = kreal) :: dnorm
      integer(kind = kint) :: i, i_peak
!
!
      i_peak = int(half * (x_peak / half_len + one) * dble(n_knl-1))
      i_peak = min(i_peak+1, n_knl)
!
      dnorm = one / (one + x_peak)
!$omp parallel do private(i)
      do i = 1, i_peak
        x_ary(i) = two * dble(i-1) / dble(n_knl-1) - one
        k_ary(i) = dnorm * (one + x_ary(i))
      end do
!$omp end parallel do
!
      dnorm = one / (one - x_peak)
!$omp parallel do private(i)
      do i = i_peak+1, n_knl
        x_ary(i) = two * dble(i-1) / dble(n_knl-1) - one
        k_ary(i) = dnorm * (one - x_ary(i))
      end do
!$omp end parallel do
!
!$omp parallel do private(i)
      do i = 1, n_knl
        x_ary(i) = x_ary(i) * half_len
        k_ary(i) = k_ary(i) / half_len
      end do
!$omp end parallel do
!
      end subroutine cal_triangle_kernel
!
!  ---------------------------------------------------------------------
!
      end module t_LIC_kernel
