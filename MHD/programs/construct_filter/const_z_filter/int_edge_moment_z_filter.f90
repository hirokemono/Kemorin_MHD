!int_edge_moment_z_filter.f90
!      module int_edge_moment_z_filter
!
!      Written by H. Matsui
!
!!      subroutine int_edge_moment                                      &
!!     &         (numnod, numele, edge, n_int, spf_1d, jac_1d)
!
      module int_edge_moment_z_filter
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine int_edge_moment                                        &
     &         (numnod, numele, edge, n_int, spf_1d, jac_1d)
!
      use t_edge_data
      use t_shape_functions
      use t_jacobian_1d
!
      use m_fem_gauss_int_coefs
      use m_int_edge_data
      use m_int_commtative_filter
      use m_commute_filter_z
!
      type(edge_data), intent(in) :: edge
      type(edge_shape_function), intent(in) :: spf_1d
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind= kint), intent(in) :: n_int
      integer (kind= kint), intent(in) :: numnod, numele
!
      integer (kind= kint) :: inod, inod1, inod2, iele, j1, j2
      integer (kind= kint) :: i, kf, ix
!
!
      do iele = 1, numele
       do j1 = 1, 2
        do j2 = 1, 2
         inod1 = edge%ie_edge(iele,j1)
         inod2 = edge%ie_edge(iele,j2)
!
         do kf = 0, 2
          do i = 1, n_int
           ix = i + int_start1(n_int)
           xmom_dt(inod2,kf) = xmom_dt(inod2,kf)                        &
     &                        + xmom_int_t(inod1,kf) * owe(ix)          &
     &                         * spf_1d%dnxi_ed(j1,ix)                  &
     &                         * jac_1d%an_edge(j2,ix)
           xmom_dot(inod2,kf) = xmom_dot(inod2,kf)                      &
     &                        + xmom_int_to(inod1,kf) * owe(ix)         &
     &                         * spf_1d%dnxi_ed(j1,ix)                  &
     &                         * jac_1d%an_edge(j2,ix)
          end do
         end do
        end do
       end do
      end do
!
      do inod = 1, numnod
       do kf = 0, 2
           xmom_dt(inod,kf)  = xmom_dt(inod,kf)  * mk(inod)
           xmom_dot(inod,kf) = xmom_dot(inod,kf) * mk(inod)
       end do
      end do
!
      end subroutine int_edge_moment
!
!   --------------------------------------------------------------------
!
      end module int_edge_moment_z_filter
