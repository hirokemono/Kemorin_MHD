!int_edge_moment_z_filter.f90
!      module int_edge_moment_z_filter
!
!      Written by H. Matsui
!
!>@file   int_edge_moment_z_filter.f90
!!        module int_edge_moment_z_filter
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief FEM integration for vertical moments for filteres
!!
!!@verbatim
!!      subroutine int_edge_z_moment(node, ele, edge, n_int, spf_1d,    &
!!     &          g_FEM, jac_1d, mk_z, xmom_int_t, xmom_int_to,         &
!!     &          xmom_dt, xmom_dot)
!!        integer(kind = kint), intent(in) :: n_int
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(edge_shape_function), intent(in) :: spf_1d
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_1d), intent(in) :: jac_1d
!!        real(kind = kreal), intent(in) :: mk_z(node%numnod)
!!        real(kind = kreal), intent(in) :: xmom_int_t(node%numnod,0:2)
!!        real(kind = kreal), intent(in) :: xmom_int_to(node%numnod,0:2)
!!        real(kind = kreal), intent(inout) :: xmom_dt(node%numnod,0:2)
!!        real(kind = kreal), intent(inout) :: xmom_dot(node%numnod,0:2)
!!@endverbatim
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
      subroutine int_edge_z_moment(node, ele, edge, n_int, spf_1d,      &
     &          g_FEM, jac_1d, mk_z, xmom_int_t, xmom_int_to,           &
     &          xmom_dt, xmom_dot)
!
      use t_geometry_data
      use t_edge_data
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use t_jacobian_1d
!
      integer(kind = kint), intent(in) :: n_int
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(edge_shape_function), intent(in) :: spf_1d
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
      real(kind = kreal), intent(in) :: mk_z(node%numnod)
      real(kind = kreal), intent(in) :: xmom_int_t(node%numnod,0:2)
      real(kind = kreal), intent(in) :: xmom_int_to(node%numnod,0:2)
!
      real(kind = kreal), intent(inout) :: xmom_dt(node%numnod,0:2)
      real(kind = kreal), intent(inout) :: xmom_dot(node%numnod,0:2)
!
!
      integer(kind = kint) :: inod, inod1, inod2, iele, j1, j2
      integer(kind = kint) :: i, kf, ix
!
!
      do iele = 1, ele%numele
       do j1 = 1, 2
        do j2 = 1, 2
         inod1 = edge%ie_edge(iele,j1)
         inod2 = edge%ie_edge(iele,j2)
!
         do kf = 0, 2
          do i = 1, n_int
           ix = i + g_FEM%int_start1(n_int)
           xmom_dt(inod2,kf) = xmom_dt(inod2,kf)                        &
     &                        + xmom_int_t(inod1,kf) * g_FEM%owe(ix)    &
     &                         * spf_1d%dnxi_ed(j1,ix)                  &
     &                         * jac_1d%an_edge(j2,ix)
           xmom_dot(inod2,kf) = xmom_dot(inod2,kf)                      &
     &                        + xmom_int_to(inod1,kf) * g_FEM%owe(ix)   &
     &                         * spf_1d%dnxi_ed(j1,ix)                  &
     &                         * jac_1d%an_edge(j2,ix)
          end do
         end do
        end do
       end do
      end do
!
      do inod = 1, node%numnod
       do kf = 0, 2
           xmom_dt(inod,kf)  = xmom_dt(inod,kf)  * mk_z(inod)
           xmom_dot(inod,kf) = xmom_dot(inod,kf) * mk_z(inod)
       end do
      end do
!
      end subroutine int_edge_z_moment
!
!   --------------------------------------------------------------------
!
      end module int_edge_moment_z_filter
