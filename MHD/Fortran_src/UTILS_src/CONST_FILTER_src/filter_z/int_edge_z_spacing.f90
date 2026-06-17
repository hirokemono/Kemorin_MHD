!>@file   int_edge_z_spacing.f90
!!        module int_edge_z_spacing
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief FEM shape functgions for vertical filter
!!
!!@verbatim
!!      subroutine int_edge_vart_width                                  &
!!     &         (numele, edge, n_int, g_FEM, jac_1d)
!!      subroutine cal_vart_width_by_ele(numnod, numele, edge,          &
!!     &                                 dz_ele, delta_z)
!!        integer(kind = kint), intent(in) :: numnod, numele
!!        type(edge_data), intent(in) :: edge
!!        real(kind = kreal), intent(in) :: dz_ele(numele)
!!        real(kind = kreal), intent(inout) :: delta_z(numnod)
!!     subroutine int_edge_vart_width(numnod, numele, edge, n_int,      &
!!    &                               g_FEM, jac_1d, rhs_dz)
!!        integer(kind = kint), intent(in) :: numnod, numele
!!        type(edge_data), intent(in) :: edge
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_1d), intent(in) :: jac_1d
!!        integer (kind = kint), intent(in) :: n_int
!!        real(kind = kreal), intent(inout) :: rhs_dz(numnod)
!!      subroutine int_edge_diff_vart_w(node, ele, edge, n_int, spf_1d, &
!!     &                                g_FEM, jac_1d, delta_z, rhs_dz)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(edge_shape_function), intent(in) :: spf_1d
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_1d), intent(in) :: jac_1d
!!        integer (kind = kint), intent(in) :: n_int
!!       real(kind = kreal), intent(in) :: delta_z(node%numnod)
!!        real(kind = kreal), intent(inout) :: rhs_dz(node%numnod)
!!      subroutine int_edge_d2_vart_w(node, ele, edge, n_int, spf_1d,   &
!!     &          g_FEM, jac_1d, delta_z, delta_dz, rhs_dz)
!!      subroutine int_edge_d2_vart_w2(node, ele, edge, n_int, spf_1d,  &
!!     &                               g_FEM, jac_1d, delta_dz, rhs_dz)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(edge_shape_function), intent(in) :: spf_1d
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_1d), intent(in) :: jac_1d
!!        integer (kind = kint), intent(in) :: n_int
!!        real(kind = kreal), intent(in) :: delta_z(node%numnod)
!!        real(kind = kreal), intent(in) :: delta_dz(node%numnod)
!!        real(kind = kreal), intent(inout) :: rhs_dz(node%numnod)
!!@endverbatim
!!
      module int_edge_z_spacing
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_vart_width_by_ele(numnod, numele, edge,            &
     &                                 dz_ele, delta_z)
!
      use t_edge_data
      use m_commute_filter_z
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(edge_data), intent(in) :: edge
      real(kind = kreal), intent(in) :: dz_ele(numele)
!
      real(kind = kreal), intent(inout) :: delta_z(numnod)
!
      integer (kind = kint) :: inod2, iele, k2
!
!
      delta_z = 0.0d0
!
      do iele = 1, numele
        do k2 = 1, 2
          inod2 = edge%ie_edge(iele,k2)
          if (inod2 .eq. 1) then
            delta_z(inod2) = delta_z(inod2) + dz_ele(iele)
          else if (inod2 .eq. numnod) then
            delta_z(inod2) = delta_z(inod2) + dz_ele(iele)
          else
            delta_z(inod2) = delta_z(inod2)                             &
     &                      + dz_ele(iele) * dz_ele(iele+(-1)**k2)      &
     &                       / (dz_ele(iele) + dz_ele(iele+(-1)**k2) )
          end if
        end do
      end do
!
      end subroutine cal_vart_width_by_ele
!
! ----------------------------------------------------------------------
!
     subroutine int_edge_vart_width(numnod, numele, edge, n_int,        &
    &                               g_FEM, jac_1d, rhs_dz)
!
      use t_edge_data
      use t_fem_gauss_int_coefs
      use t_jacobian_1d
!
      use m_commute_filter_z
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(edge_data), intent(in) :: edge
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind = kint), intent(in) :: n_int
!
      real(kind = kreal), intent(inout) :: rhs_dz(numnod)
!
      integer (kind = kint) ::  inod2, iele, k2, i, ix
!
!
      rhs_dz = 0.0d0
!
      do iele = 1, numele
        do i = 1, n_int
          ix = i + g_FEM%int_start1(n_int)
          do k2 = 1, 2
           inod2 = edge%ie_edge(iele,k2)
           rhs_dz(inod2) = rhs_dz(inod2)                                &
     &                    + abs(jac_1d%xeg_edge(iele,ix,3))             &
     &                     * jac_1d%an_edge(k2,ix)                      &
     &                     * jac_1d%xeg_edge(iele,ix,3) * g_FEM%owe(ix)
         end do
       end do
      end do
!
      end subroutine int_edge_vart_width
!
! ----------------------------------------------------------------------
!
      subroutine int_edge_diff_vart_w(node, ele, edge, n_int, spf_1d,   &
     &                                g_FEM, jac_1d, delta_z, rhs_dz)
!
      use t_geometry_data
      use t_edge_data
      use t_jacobian_1d
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use m_commute_filter_z
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(edge_shape_function), intent(in) :: spf_1d
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: delta_z(node%numnod)
!
      real(kind = kreal), intent(inout) :: rhs_dz(node%numnod)
!
      integer (kind = kint) :: inod1, inod2, iele, k1, k2, i, ix
!
!
      rhs_dz = 0.0d0
!
      do iele = 1, ele%numele
        do i = 1, n_int
          ix = i + g_FEM%int_start1(n_int)
          do k1 = 1, 2
            do k2 = 1, 2
              inod1 = edge%ie_edge(iele,k1)
              inod2 = edge%ie_edge(iele,k2)
              rhs_dz(inod2) = rhs_dz(inod2)                             &
     &                       + delta_z(inod1) * spf_1d%dnxi_ed(k1,ix)   &
     &                        * jac_1d%an_edge(k2,ix) * g_FEM%owe(ix)
            end do
          end do
        end do
      end do
!
!
      end subroutine int_edge_diff_vart_w
!
! ----------------------------------------------------------------------
!
      subroutine int_edge_d2_vart_w(node, ele, edge, n_int, spf_1d,     &
     &          g_FEM, jac_1d, delta_z, delta_dz, rhs_dz)
!
      use calypso_mpi
      use t_geometry_data
      use t_edge_data
      use t_jacobian_1d
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use m_commute_filter_z
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(edge_shape_function), intent(in) :: spf_1d
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: delta_z(node%numnod)
      real(kind = kreal), intent(in) :: delta_dz(node%numnod)
!
      real(kind = kreal), intent(inout) :: rhs_dz(node%numnod)
!
      integer (kind = kint) :: inod1, inod2, iele, k1, k2, i, ix
!
!
      rhs_dz = 0.0d0
!
      do iele = 1, ele%numele
        do i = 1, n_int
          ix = i + g_FEM%int_start1(n_int)
          do k1 = 1, 2
            do k2 = 1, 2
              inod1 = edge%ie_edge(iele,k1)
              inod2 = edge%ie_edge(iele,k2)
              rhs_dz(inod2) = rhs_dz(inod2) - delta_z(inod1)            &
     &                  * spf_1d%dnxi_ed(k1,ix) * spf_1d%dnxi_ed(k2,ix) &
     &                  * g_FEM%owe(ix)  / jac_1d%xeg_edge(iele,ix,3)
            end do
          end do
        end do
      end do
!
      rhs_dz(1) = rhs_dz(1) - delta_dz(1)
      rhs_dz(node%internal_node) = rhs_dz(node%internal_node)           &
     &                             + delta_dz(node%internal_node)
!
      end subroutine int_edge_d2_vart_w
!
! ----------------------------------------------------------------------
!
      subroutine int_edge_d2_vart_w2(node, ele, edge, n_int, spf_1d,    &
     &                               g_FEM, jac_1d, delta_dz, rhs_dz)
!
      use t_geometry_data
      use t_edge_data
      use t_jacobian_1d
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use m_commute_filter_z
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(edge_shape_function), intent(in) :: spf_1d
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind = kint), intent(in) :: n_int
      real(kind = kreal), intent(in) :: delta_dz(node%numnod)
!
      real(kind = kreal), intent(inout) :: rhs_dz(node%numnod)
!
      integer (kind = kint) :: inod1, inod2, iele, k1, k2, i, ix
!
!
      rhs_dz = 0.0d0
!
      do iele = 1, ele%numele
        do i = 1, n_int
          ix = i + g_FEM%int_start1(n_int)
          do k1 = 1, 2
            do k2 = 1, 2
              inod1 = edge%ie_edge(iele,k1)
              inod2 = edge%ie_edge(iele,k2)
              rhs_dz(inod2) = rhs_dz(inod2) + delta_dz(inod1)           &
     &                  * spf_1d%dnxi_ed(k1,ix) * jac_1d%an_edge(k2,ix) &
     &                  * g_FEM%owe(ix)
            end do
          end do
        end do
      end do
!
      end subroutine int_edge_d2_vart_w2
!
! ----------------------------------------------------------------------
!
      end module int_edge_z_spacing
