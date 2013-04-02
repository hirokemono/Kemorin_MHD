!set_vert_diff_z_filter.f90
!      module set_vert_diff_z_filter
!
      module set_vert_diff_z_filter
!
!      Written by H. Matsui
!
      use m_precision
!
      implicit none
!
!     subroutine set_spatial_difference(n_int)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_spatial_difference(n_int)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians_4_edge
      use m_commute_filter_z
      use m_int_edge_data
!
      integer (kind = kint), intent(in) :: n_int
      integer (kind = kint) :: iele, inod, j, k, ix
      real(kind = kreal), parameter :: one = 1.0d0, half = 0.5d0
!
!
       do k = 1, n_int
         ix = k + int_start1(n_int)
         do iele = 1, numele
           dz(iele) = dz(iele) + xeg_edge(iele,ix,3) * owe(ix)
         end do
       end do
       do iele = 1, numele
         dz(iele) = half * dz(iele)
       end do
!
!
      end subroutine set_spatial_difference
!
!   --------------------------------------------------------------------
!
      end module set_vert_diff_z_filter
