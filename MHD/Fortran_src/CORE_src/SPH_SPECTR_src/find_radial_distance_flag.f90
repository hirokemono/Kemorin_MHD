!>@file   find_radial_distance_flag.f90
!!@brief  module find_radial_distance_flag
!!
!!@author H. Okuda and H. Matsui
!!@date Programmed in Sep., 2009
!
!> @brief Find grid distance mode
!!
!!@verbatim
!!      integer(kind = kint) function s_find_radial_distance_flag       &
!!     &                            (num_layer, nlayer_ICB, nlayer_CMB, &
!!     &                             r_ICB, r_CMB, r_grid)
!!        integer(kind = kint), intent(in) :: num_layer
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: r_grid(num_layer)
!!@endverbatim
!
      module find_radial_distance_flag
!
      use m_precision
      use m_constants
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      integer(kind = kint) function s_find_radial_distance_flag         &
     &                            (num_layer, nlayer_ICB, nlayer_CMB,   &
     &                             r_ICB, r_CMB, r_grid)
!
      use chebyshev_radial_grid
      use half_chebyshev_radial_grid
      use m_spheric_constants
!
      integer(kind = kint), intent(in) :: num_layer
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: r_grid(num_layer)
!
      integer(kind = kint) :: k
      real(kind = kreal) :: diff
      real(kind = kreal) :: diff_ch_max, diff_eq_max, diff_hch_max
      integer(kind = kint) :: iflag_rgrid
!
      real(kind = kreal), allocatable :: r_eq(:), r_ch(:), r_hch(:)
!
      if(num_layer .le. 0) then
        s_find_radial_distance_flag = igrid_error
        return
      end if
!
      allocate(r_eq(num_layer))
      allocate(r_ch(num_layer))
      allocate(r_hch(num_layer))
!
      r_eq(1:num_layer) =  0.0d0
      r_ch(1:num_layer) =  0.0d0
      r_hch(1:num_layer) = 0.0d0
!
      if(r_ICB .eq. 0.0d0) then
        call set_equi_distance_sphere                                   &
     &     (num_layer, nlayer_CMB, r_CMB, r_eq)
        call set_chebyshev_distance_sphere                              &
     &     (num_layer, nlayer_CMB, r_CMB, r_ch)
        call half_chebyshev_distance_sphere                             &
     &     (num_layer, nlayer_CMB, r_CMB, r_hch)
      else
        call set_equi_distance_shell                                    &
     &     (num_layer, nlayer_ICB, nlayer_CMB, r_ICB, r_CMB, r_eq)
        call set_chebyshev_distance_shell                               &
     &     (num_layer, nlayer_ICB, nlayer_CMB, r_ICB, r_CMB, r_ch)
        call half_chebyshev_distance_shell                              &
     &     (num_layer, nlayer_ICB, nlayer_CMB, r_ICB, r_CMB, r_hch)
      end if
!
!
      diff_eq_max =  0.0d0
      diff_ch_max =  0.0d0
      diff_hch_max = 0.0d0
!
      do k = 1, num_layer
        diff = abs( r_grid(k) - r_eq(k)) / r_eq(k)
        diff_eq_max = max(diff_eq_max,diff)
!
        diff = abs( r_grid(k) - r_ch(k)) / r_ch(k)
        diff_ch_max = max(diff_ch_max,diff)
!
        diff = abs( r_grid(k) - r_hch(k)) / r_hch(k)
        diff_hch_max = max(diff_hch_max,diff)
      end do
!
      if      (diff_ch_max .lt. 1.0d-10) then
        iflag_rgrid = igrid_Chebyshev
      else if (diff_eq_max .lt. 1.0d-10) then
        iflag_rgrid = igrid_equidistance
      else if (diff_hch_max .lt. 1.0d-10) then
        iflag_rgrid = igrid_half_Chebyshev
      else
        iflag_rgrid = igrid_non_equidist
      end if
      s_find_radial_distance_flag = iflag_rgrid
!
      deallocate(r_eq, r_ch, r_hch)
!
      end function s_find_radial_distance_flag
!
!  -------------------------------------------------------------------
!
      end module find_radial_distance_flag
