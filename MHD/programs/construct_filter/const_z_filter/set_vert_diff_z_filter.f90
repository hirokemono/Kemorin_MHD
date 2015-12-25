!set_vert_diff_z_filter.f90
!      module set_vert_diff_z_filter
!
!      Written by H. Matsui
!
!     subroutine set_spatial_difference(n_int, jac_1d)
!
      module set_vert_diff_z_filter
!
      use m_precision
      use m_constants
!
      use t_jacobian_1d
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_spatial_difference(n_int, jac_1d)
!
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_commute_filter_z
      use m_int_edge_data
!
      integer (kind = kint), intent(in) :: n_int
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind = kint) :: iele, k, ix
!
!
       do k = 1, n_int
         ix = k + int_start1(n_int)
         do iele = 1, ele1%numele
           dz(iele) = dz(iele) + jac_1d%xeg_edge(iele,ix,3) * owe(ix)
         end do
       end do
       do iele = 1, ele1%numele
         dz(iele) = half * dz(iele)
       end do
!
      end subroutine set_spatial_difference
!
!   --------------------------------------------------------------------
!
      end module set_vert_diff_z_filter
