!
!      module set_aiccg_boundaries
!
!     Written by H. Matsui on June, 2005
!
!      subroutine set_bc_4_velo_mat(nst, ned, iele, k1, k2)
!      subroutine set_bc_4_press_mat(iele, k1, k2)
!      subroutine set_bc_4_temp_mat(iele, k1, k2)
!      subroutine set_bc_4_magne_mat(nst, ned, iele, k1, k2)
!      subroutine set_bc_4_composite_mat(iele, k1, k2)
!
      module set_aiccg_boundaries
!
      use m_precision
!
      use m_geometry_parameter
      use m_geometry_data
!
      use correct_matrix_4_boundary
      use set_off_diagonals
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_4_velo_mat(nst, ned, iele, k1, k2)
!
      use m_solver_djds_MHD
      use m_velo_matrix
      use set_aiccg_bc_node_type
!
      integer (kind = kint), intent(in) :: nst, ned, iele, k1, k2
!
!
      call set_bc_4_vector_mat_type(nst, ned, ie(iele,k1), ie(iele,k2), &
     &    DJDS_fluid, Vmat_DJDS)
!
      end subroutine set_bc_4_velo_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_4_temp_mat(iele, k1, k2)
!
      use m_solver_djds_MHD
      use m_temp_matrix
      use set_aiccg_bc_node_type
!
      integer (kind = kint), intent(in) :: iele, k1, k2
!
!
      call set_bc_4_scalar_mat_type(ie(iele,k1), ie(iele,k2),           &
     &    DJDS_fluid, Tmat_DJDS)
!
      end subroutine set_bc_4_temp_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_4_press_mat(iele, k1, k2)
!
      use m_solver_djds_linear_fl
      use m_velo_matrix
!
      integer (kind = kint), intent(in) :: iele, k1, k2
      integer (kind = kint) :: nod1, nod2, mat_num
!
!
      nod1 = ie(iele,k1)
      nod2 = ie(iele,k2)
!
      call set_off_diag_linear_fl( nod1, nod2, mat_num )
      call correct_matrix11_4_boundary                                  &
     &   (numnod, itotal1_fl_u, itotal1_fl_l, mat_num, Pmat_DJDS%aiccg)
!
      end subroutine set_bc_4_press_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_4_magne_mat(nst, ned, iele, k1, k2)
!
      use m_solver_djds_MHD
      use m_magne_matrix
      use set_aiccg_bc_node_type
!
      integer (kind = kint), intent(in) :: nst, ned, iele, k1, k2
!
!
      call set_bc_4_vector_mat_type(nst, ned, ie(iele,k1), ie(iele,k2), &
     &    DJDS_entire, Bmat_DJDS)
!
      end subroutine set_bc_4_magne_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_4_composite_mat(iele, k1, k2)
!
      use m_solver_djds_MHD
      use m_light_element_matrix
      use set_aiccg_bc_node_type
!
      integer (kind = kint), intent(in) :: iele, k1, k2
!
!
      call set_bc_4_scalar_mat_type(ie(iele,k1), ie(iele,k2),           &
     &    DJDS_fluid, Cmat_DJDS)
!
      end subroutine set_bc_4_composite_mat
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_boundaries
