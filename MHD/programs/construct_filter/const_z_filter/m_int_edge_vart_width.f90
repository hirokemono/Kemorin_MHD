!
!     module m_int_edge_vart_width
!
!      subroutine int_edge_vart_width(n_int)
!      subroutine int_edge_diff_vart_w(n_int)
!      subroutine int_edge_d2_vart_w(n_int)
!      subroutine int_edge_d2_vart_w2(n_int)
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
      subroutine allocate_delta_z
!
      use m_geometry_data
!
      allocate ( delta_z(node1%numnod) )
      allocate ( delta_dz(node1%numnod) )
      allocate ( d2_dz(node1%numnod) )
!
      allocate ( delta_z_e(ele1%numele) )
      allocate ( delta_dz_e(ele1%numele) )
      allocate ( d2_dz_e(ele1%numele) )
!
      allocate( rhs_dz(node1%numnod) )
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
     subroutine int_edge_vart_width(n_int)
!
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians_4_edge
      use m_commute_filter_z
      use m_int_edge_data
!
      integer (kind = kint), intent(in) :: n_int
      integer (kind = kint) ::  inod2, iele, k2, i, ix
!
!
      rhs_dz = 0.0d0
!
      do iele = 1, ele1%numele
        do i = 1, n_int
          ix = i + int_start1(n_int)
          do k2 = 1, 2
           inod2 = ie_edge(iele,k2)
           rhs_dz(inod2) = rhs_dz(inod2)                                &
     &                    + abs(jac1_1d_l%xeg_edge(iele,ix,3))          &
     &                     * jac1_1d_l%an_edge(k2,ix)                   &
     &                     * jac1_1d_l%xeg_edge(iele,ix,3) * owe(ix)
         end do
       end do
      end do
!
      end subroutine int_edge_vart_width
!
! ----------------------------------------------------------------------
!
!     subroutine set_rhs_vart_width
!
     subroutine set_rhs_vart_width
!
      use m_geometry_data
      use m_consist_mass_crs
!
      integer (kind = kint) :: inod
!
!
      do inod = 1, node1%numnod
        rhs_mk_crs(inod) = rhs_dz(inod)
      end do
!
      end subroutine set_rhs_vart_width
!
! ----------------------------------------------------------------------
!
     subroutine cal_sol_vart_width
!
      use m_geometry_data
      use m_commute_filter_z
      use m_int_edge_data
!
      integer (kind = kint) :: inod
!
!
      do inod = 1, node1%numnod
        delta_z(inod) = rhs_dz(inod) * mk(inod)
      end do
!
      end subroutine cal_sol_vart_width
!
! ----------------------------------------------------------------------
!
     subroutine cal_sol_diff_vart_width
!
      use m_geometry_data
      use m_commute_filter_z
      use m_int_edge_data
!
      integer (kind = kint) :: inod
!
!
      do inod = 1, node1%numnod
        delta_dz(inod) = rhs_dz(inod) * mk(inod)
      end do
!
      end subroutine cal_sol_diff_vart_width
!
! ----------------------------------------------------------------------
!
     subroutine cal_sol_d2_vart_width
!
      use m_geometry_data
      use m_commute_filter_z
      use m_int_edge_data
!
      integer (kind = kint) :: inod
!
!
      do inod = 1, node1%numnod
        d2_dz(inod) = rhs_dz(inod) * mk(inod)
      end do
!
      end subroutine cal_sol_d2_vart_width
!
! ----------------------------------------------------------------------
!
      subroutine cal_vart_width_by_ele
!
      use m_geometry_data
      use m_commute_filter_z
      use m_int_edge_data
!
      integer (kind = kint) :: inod2, iele, k2
!
!
      delta_z = 0.0d0
!
      do iele = 1, ele1%numele
        do k2 = 1, 2
          inod2 = ie_edge(iele,k2)
          if (inod2 .eq. 1) then
            delta_z(inod2) = delta_z(inod2) + dz(iele)
          else if (inod2 .eq. node1%numnod) then
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
      subroutine int_edge_diff_vart_w(n_int)
!
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_shape_functions
      use m_jacobians_4_edge
      use m_commute_filter_z
      use m_int_edge_data
!
      integer (kind = kint), intent(in) :: n_int
      integer (kind = kint) :: inod1, inod2, iele, k1, k2, i, ix
!
!
      rhs_dz = 0.0d0
!
      do iele = 1, ele1%numele
        do i = 1, n_int
          ix = i + int_start1(n_int)
          do k1 = 1, 2
            do k2 = 1, 2
              inod1 = ie_edge(iele,k1)
              inod2 = ie_edge(iele,k2)
              rhs_dz(inod2) = rhs_dz(inod2) + delta_z(inod1)            &
     &                                     * dnxi_ed1(k1,ix)            &
     &                       * jac1_1d_l%an_edge(k2,ix) * owe(ix)
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
      subroutine int_edge_d2_vart_w(n_int)
!
      use calypso_mpi
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_shape_functions
      use m_jacobians_4_edge
      use m_commute_filter_z
      use m_int_edge_data
!
      integer (kind = kint), intent(in) :: n_int
      integer (kind = kint) :: inod1, inod2, iele, k1, k2, i, ix
!
!
      rhs_dz = 0.0d0
!
      do iele = 1, ele1%numele
        do i = 1, n_int
          ix = i + int_start1(n_int)
          do k1 = 1, 2
            do k2 = 1, 2
              inod1 = ie_edge(iele,k1)
              inod2 = ie_edge(iele,k2)
              rhs_dz(inod2) = rhs_dz(inod2) - delta_z(inod1)            &
     &                     * dnxi_ed1(k1,ix)* dnxi_ed1(k2,ix) * owe(ix) &
     &                      / jac1_1d_l%xeg_edge(iele,ix,3)
            end do
          end do
        end do
      end do
!
      rhs_dz(1) = rhs_dz(1) - delta_dz(1)
      rhs_dz(internal_node) = rhs_dz(internal_node)                     &
     &    + delta_dz(internal_node)
!
      end subroutine int_edge_d2_vart_w
!
! ----------------------------------------------------------------------
!
      subroutine int_edge_d2_vart_w2(n_int)
!
      use calypso_mpi
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_shape_functions
      use m_jacobians_4_edge
      use m_commute_filter_z
      use m_int_edge_data
!
      integer (kind = kint), intent(in) :: n_int
      integer (kind = kint) :: inod1, inod2, iele, k1, k2, i, ix
!
!
      rhs_dz = 0.0d0
!
      do iele = 1, ele1%numele
        do i = 1, n_int
          ix = i + int_start1(n_int)
          do k1 = 1, 2
            do k2 = 1, 2
              inod1 = ie_edge(iele,k1)
              inod2 = ie_edge(iele,k2)
              rhs_dz(inod2) = rhs_dz(inod2) + delta_dz(inod1)           &
     &                    * dnxi_ed1(k1,ix) * jac1_1d_l%an_edge(k2,ix)  &
     &                    * owe(ix)
            end do
          end do
        end do
      end do
!
!      do inod1 = 1, node1%numnod
!        write(*,*) inod1, rhs_dz(inod1)
!      end do
!
      end subroutine int_edge_d2_vart_w2
!
! ----------------------------------------------------------------------
!
      end module m_int_edge_vart_width
