!>@file   t_sph_filtering_data.f90
!!@brief  module t_sph_filtering_data
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!      subroutine alloc_sph_filter_moms(mom)
!!      subroutine dealloc_sph_filter_moms(mom)
!!        type(sph_filter_moment), intent(inout) :: mom
!!
!!      subroutine check_radial_filter(sph_rj, r_filter)
!!      subroutine check_radial_filter_func(sph_rj, r_filter)
!!@endverbatim
!!
!
      module t_sph_filtering_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_spheric_mesh
      use t_filter_coefficients
      use t_spheric_parameter
!
      implicit none
!
!
      type sph_filter_moment
!>        Truncation degree
        integer(kind = kint) :: num_momentum
        integer(kind = kint) :: nfilter_sides = 3
!
!>        Momentums of filter
        real(kind = kreal), allocatable :: filter_mom(:)
      end type sph_filter_moment
!
      type sph_gaussian_filter
!>        Truncation degree
        integer(kind = kint) :: l_truncation
!>        filter width
        integer(kind = kint) :: k_width
!>        Coefficients for each degree
        real(kind = kreal), allocatable :: weight(:)
      end type sph_gaussian_filter
!
!
      type radial_filters_type
!> filter width
        real(kind = kreal), allocatable :: filter_mom(:)
      end type radial_filters_type
!
!
!>      Structure for filtering data for spherical shell
      type sph_filters_type
        real(kind = kreal) :: width = 1.0d0
!
!> data structure for radial filter coefficients table
        type(filter_coefficients_type) :: r_filter
!> data structure for horizontral filter coefficients table
        type(sph_gaussian_filter) :: sph_filter
!
!> data structure for radial filter moments table
        type(sph_filter_moment) :: r_moments
!> data structure for horizontral filter moments table
        type(sph_filter_moment) :: sph_moments
      end type sph_filters_type
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_sph_filter_moms(mom)
!
      type(sph_filter_moment), intent(inout) :: mom
!
!
      mom%nfilter_sides = (mom%num_momentum + 1) / 2
      allocate(mom%filter_mom(0:mom%num_momentum-1))
      mom%filter_mom = 0.0d0
!
      end subroutine alloc_sph_filter_moms
!
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
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_filter_moms(mom)
!
      type(sph_filter_moment), intent(inout) :: mom
!
!
      deallocate(mom%filter_mom)
!
      end subroutine dealloc_sph_filter_moms
!
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
! ----------------------------------------------------------------------
!
      subroutine cal_r_gaussian_moments(mom)
!
      type(sph_filter_moment), intent(inout) :: mom
!
      integer(kind = kint) :: imom
!
!
      mom%filter_mom(0) = one
      do imom = 1, mom%nfilter_sides-1
        mom%filter_mom(2*imom-1) = zero
        mom%filter_mom(2*imom  )                                        &
     &              = real(2*imom-1) * mom%filter_mom(2*imom-2)
      end do
!
      end subroutine cal_r_gaussian_moments
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_gaussian_filter(l_truncation, k_width, weight, &
     &          num_momentum, filter_mom)
!
      integer(kind = kint), intent(in) :: l_truncation, k_width
      integer(kind = kint), intent(in) :: num_momentum
      real(kind = kreal), intent(inout) :: weight(0:l_truncation)
      real(kind = kreal), intent(inout) :: filter_mom(0:num_momentum-1)
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
      do i = 2, num_momentum, 2
        filter_mom(i-1) = zero
        filter_mom(i) =   real(2*i-1) * filter_mom(i-2) * b**i
      end do
      filter_mom(0:num_momentum-1) = filter_mom**2
!
      end subroutine set_sph_gaussian_filter
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_radial_filter(sph_rj, r_filter)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: i, ist, ied
!
!
      if(my_rank .ne. 0) return
        write(*,*)  'r_filter%inod_filter(i)',  r_filter%istack_node
        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
          ist = r_filter%istack_near_nod(i-1) + 1
          ied = r_filter%istack_near_nod(i)
          write(*,*) i, r_filter%inod_filter(i),                        &
     &                  r_filter%inod_near(ist:ied)
        end do
        write(*,*)  'r_filter%weight(i)'
        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
          ist = r_filter%istack_near_nod(i-1) + 1
          ied = r_filter%istack_near_nod(i)
          write(*,*) sph_rj%radius_1d_rj_r(r_filter%inod_filter(i)),    &
     &               i, r_filter%inod_filter(i),                        &
     &                  r_filter%weight(ist:ied)
        end do
!
      end subroutine check_radial_filter
!
! ----------------------------------------------------------------------
!
      subroutine check_radial_filter_func(sph_rj, r_filter)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: i, ist, ied
!
!
      if(my_rank .ne. 0) return
        write(*,*)  'r_filter%func(i)'
        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
          ist = r_filter%istack_near_nod(i-1) + 1
          ied = r_filter%istack_near_nod(i)
          write(*,*) sph_rj%radius_1d_rj_r(r_filter%inod_filter(i)),    &
     &               i, r_filter%inod_filter(i),                        &
     &                  r_filter%func(ist:ied)
        end do
!
      end subroutine check_radial_filter_func
!
! ----------------------------------------------------------------------
!
      end module t_sph_filtering_data
