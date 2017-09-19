!
!     module m_int_edge_vart_width
!
!!     subroutine int_edge_vart_width                                   &
!!    &         (numele, edge, n_int, g_FEM, jac_1d)
!!      subroutine int_edge_diff_vart_w                                 &
!!     &         (ele, edge, n_int, spf_1d, g_FEM, jac_1d)
!!      subroutine int_edge_d2_vart_w                                   &
!!     &         (node, ele, edge, n_int, spf_1d, g_FEM, jac_1d)
!!      subroutine int_edge_d2_vart_w2                                  &
!!     &         (ele, edge, n_int, spf_1d, g_FEM, jac_1d)
!
      module m_int_edge_vart_width
!
      use m_precision
!
      implicit none
!
      integer (kind = kint) :: iflag_mass = 1
!
      real(kind = kreal), dimension(:), allocatable :: delta_z
      real(kind = kreal), dimension(:), allocatable :: delta_dz
      real(kind = kreal), dimension(:), allocatable :: d2_dz
!
      real(kind = kreal), dimension(:), allocatable :: delta_z_e
      real(kind = kreal), dimension(:), allocatable :: delta_dz_e
      real(kind = kreal), dimension(:), allocatable :: d2_dz_e
!
      real(kind = kreal), dimension(:), allocatable :: rhs_dz
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_delta_z(numnod, numele)
!
      integer(kind = kint), intent(in) :: numnod, numele
!
      allocate ( delta_z(numnod) )
      allocate ( delta_dz(numnod) )
      allocate ( d2_dz(numnod) )
!
      allocate ( delta_z_e(numele) )
      allocate ( delta_dz_e(numele) )
      allocate ( d2_dz_e(numele) )
!
      allocate( rhs_dz(numnod) )
!
      rhs_dz = 0.0d0

      delta_z  = 0.0d0
      delta_dz = 0.0d0
      d2_dz = 0.0d0
!
      delta_z_e  = 0.0d0
      delta_dz_e = 0.0d0
      d2_dz_e = 0.0d0
!
      end subroutine allocate_delta_z
!
! ----------------------------------------------------------------------
!
     subroutine int_edge_vart_width                                     &
    &         (numele, edge, n_int, g_FEM, jac_1d)
!
      use t_edge_data
      use t_fem_gauss_int_coefs
      use t_jacobian_1d
!
      use m_commute_filter_z
      use m_int_edge_data
!
      integer(kind = kint), intent(in) :: numele
      type(edge_data), intent(in) :: edge
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind = kint), intent(in) :: n_int
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
     subroutine set_rhs_vart_width(numnod)
!
      use m_consist_mass_crs
!
      integer(kind = kint), intent(in) :: numnod
!
      integer (kind = kint) :: inod
!
!
      do inod = 1, numnod
        rhs_mk_crs(inod) = rhs_dz(inod)
      end do
!
      end subroutine set_rhs_vart_width
!
! ----------------------------------------------------------------------
!
     subroutine cal_sol_vart_width(numnod)
!
      use m_commute_filter_z
      use m_int_edge_data
!
      integer(kind = kint), intent(in) :: numnod
!
      integer (kind = kint) :: inod
!
!
      do inod = 1, numnod
        delta_z(inod) = rhs_dz(inod) * mk(inod)
      end do
!
      end subroutine cal_sol_vart_width
!
! ----------------------------------------------------------------------
!
     subroutine cal_sol_diff_vart_width(numnod)
!
      use m_commute_filter_z
      use m_int_edge_data
!
      integer(kind = kint), intent(in) :: numnod
!
      integer (kind = kint) :: inod
!
!
      do inod = 1, numnod
        delta_dz(inod) = rhs_dz(inod) * mk(inod)
      end do
!
      end subroutine cal_sol_diff_vart_width
!
! ----------------------------------------------------------------------
!
     subroutine cal_sol_d2_vart_width(numnod)
!
      use m_commute_filter_z
      use m_int_edge_data
!
      integer(kind = kint), intent(in) :: numnod
      integer (kind = kint) :: inod
!
!
      do inod = 1, numnod
        d2_dz(inod) = rhs_dz(inod) * mk(inod)
      end do
!
      end subroutine cal_sol_d2_vart_width
!
! ----------------------------------------------------------------------
!
      subroutine cal_vart_width_by_ele(numnod, numele, edge)
!
      use t_edge_data
      use m_commute_filter_z
      use m_int_edge_data
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(edge_data), intent(in) :: edge
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
            delta_z(inod2) = delta_z(inod2) + dz(iele)
          else if (inod2 .eq. numnod) then
            delta_z(inod2) = delta_z(inod2) + dz(iele)
          else
            delta_z(inod2) = delta_z(inod2) + dz(iele)                  &
     &            * dz(iele+(-1)**k2) / (dz(iele) + dz(iele+(-1)**k2) )
          end if
        end do
      end do
!
      end subroutine cal_vart_width_by_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_edge_diff_vart_w                                   &
     &         (ele, edge, n_int, spf_1d, g_FEM, jac_1d)
!
      use t_geometry_data
      use t_edge_data
      use t_jacobian_1d
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use m_commute_filter_z
      use m_int_edge_data
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(edge_shape_function), intent(in) :: spf_1d
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind = kint), intent(in) :: n_int
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
      subroutine int_edge_d2_vart_w                                     &
     &         (node, ele, edge, n_int, spf_1d, g_FEM, jac_1d)
!
      use calypso_mpi
      use t_geometry_data
      use t_edge_data
      use t_jacobian_1d
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use m_commute_filter_z
      use m_int_edge_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(edge_shape_function), intent(in) :: spf_1d
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind = kint), intent(in) :: n_int
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
      subroutine int_edge_d2_vart_w2                                    &
     &         (ele, edge, n_int, spf_1d, g_FEM, jac_1d)
!
      use calypso_mpi
      use t_geometry_data
      use t_edge_data
      use t_jacobian_1d
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use m_commute_filter_z
      use m_int_edge_data
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(edge_shape_function), intent(in) :: spf_1d
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
      integer (kind = kint), intent(in) :: n_int
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
!      do inod1 = 1, node%numnod
!        write(*,*) inod1, rhs_dz(inod1)
!      end do
!
      end subroutine int_edge_d2_vart_w2
!
! ----------------------------------------------------------------------
!
      end module m_int_edge_vart_width
