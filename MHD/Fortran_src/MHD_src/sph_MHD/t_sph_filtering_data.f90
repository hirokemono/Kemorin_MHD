!>@file   t_sph_filtering_data.f90
!!@brief  module t_sph_filtering_data
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine const_sph_gaussian_filter(l_truncation, sph_filter)
!!
!!      subroutine alloc_sph_filter_weights(ltr, sph_filter)
!!      subroutine alloc_sph_filter_moments(sph_filter)
!!      subroutine dealloc_sph_filter_weights(sph_filter)
!!      subroutine dealloc_sph_filter_moments(sph_filter)
!!@endverbatim
!!
!
      module t_sph_filtering_data
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type sph_gaussian_filter
!>        Truncation degree
        integer(kind = kint) :: l_truncation
!>        filter width
        integer(kind = kint) :: k_width
!>        Coefficients for each degree
        real(kind = kreal), allocatable :: weight(:)
!
!>        Truncation degree
        integer(kind = kint) :: num_momentum
!>        Momentums of filter
        real(kind = kreal), allocatable :: filter_mom(:)
      end type sph_gaussian_filter
!
      type sph_gaussian_filters
        type(sph_gaussian_filter) :: sph_filter
        type(sph_gaussian_filter) :: sph_wide_filter
        type(sph_gaussian_filter) :: sph_wider_filter
      end type sph_gaussian_filters
!
      private :: alloc_sph_filter_weights, alloc_sph_filter_moments
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_sph_gaussian_filter(l_truncation, sph_filter)
!
      integer(kind = kint), intent(in) :: l_truncation
!
      type(sph_gaussian_filter), intent(inout) :: sph_filter
!
!
      call alloc_sph_filter_weights(l_truncation, sph_filter)
      call alloc_sph_filter_moments(sph_filter)
      write(*,*) 'set_sph_gaussian_filter'
      call set_sph_gaussian_filter(sph_filter%l_truncation,             &
     &    sph_filter%k_width, sph_filter%weight,                        &
     &    sph_filter%num_momentum, sph_filter%filter_mom)
      write(*,*) 'set_sph_gaussian_filter end'
!
      end subroutine const_sph_gaussian_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_sph_filter_weights(ltr, sph_filter)
!
      integer(kind = kint), intent(in) :: ltr
      type(sph_gaussian_filter), intent(inout) :: sph_filter
!
!
      sph_filter%l_truncation = ltr
      allocate(sph_filter%weight(0:sph_filter%l_truncation))
!
      end subroutine alloc_sph_filter_weights
!
! ----------------------------------------------------------------------
!
      subroutine alloc_sph_filter_moments(sph_filter)
!
      type(sph_gaussian_filter), intent(inout) :: sph_filter
!
!
      allocate(sph_filter%filter_mom(0:sph_filter%num_momentum))
      sph_filter%filter_mom = 0.0d0
!
      end subroutine alloc_sph_filter_moments
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_filter_weights(sph_filter)
!
      type(sph_gaussian_filter), intent(inout) :: sph_filter
!
!
      deallocate(sph_filter%weight)
!
      end subroutine dealloc_sph_filter_weights
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_filter_moments(sph_filter)
!
      type(sph_gaussian_filter), intent(inout) :: sph_filter
!
!
      deallocate(sph_filter%filter_mom)
!
      end subroutine dealloc_sph_filter_moments
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_gaussian_filter(l_truncation, k_width, weight, &
     &          num_momentum, filter_mom)
!
      integer(kind = kint), intent(in) :: l_truncation, k_width
      integer(kind = kint), intent(in) :: num_momentum
      real(kind = kreal), intent(inout) :: weight(0:l_truncation)
      real(kind = kreal), intent(inout) :: filter_mom(0:num_momentum)
!
      integer(kind = kint) :: i, l, l_rest
      real(kind = kreal) :: b
!
!
      b = log(two) / (one - cos(one / real(k_width)))
!
      weight(0) = one
      weight(1) = (one + exp(-two*b)) / (one - exp(-two*b)) - one / b
!
      l_rest = 2
      do l = 2, l_truncation
        weight(l) = - weight(l-1) * dble(2*l - 1) / b + weight(l-2)
        if(weight(l) .lt. zero) then
          l_rest = l
          exit
        end if
      end do
!
      do l = l_rest, l_truncation
        weight(l) = zero
      end do
!
      filter_mom(0) = one
      filter_mom(1) = zero
      do i = 2, num_momentum, 2
        filter_mom(i) =   real(2*i-1) * filter_mom(2*i-2) * b**i
        filter_mom(i+1) = zero
      end do
      filter_mom(0:num_momentum) = filter_mom**2
!
      end subroutine set_sph_gaussian_filter
!
! -----------------------------------------------------------------------
!
      end module t_sph_filtering_data
