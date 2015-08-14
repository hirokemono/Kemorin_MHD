!>@file   set_aiccg_bc_fixed.f90
!!@brief  module set_aiccg_bc_fixed
!!
!!@author H. Matsui
!!@date Programmed in June, 2005
!!@date  programmed by H.Matsui in July 2000 (ver 1.1)
!!@n      modified by Kemorin in Jan. 2004
!!@n     Merged by Kemorin in June. 2005
!!@n     Merged by Kemorin in Oct. 2005
!!@n     modified by Kemorin in Nov. 2012
!
!>     Preconditiong of DJDS solver for MHD dynamo
!!
!!@verbatim
!!      subroutine set_aiccg_bc_velo_nod
!!      subroutine set_aiccg_bc_velo_rot
!!      subroutine set_aiccg_bc_vecp_nod
!!      subroutine set_aiccg_bc_magne_nod
!!
!!      subroutine set_aiccg_bc_press_nod
!!      subroutine set_aiccg_bc_temp_nod
!!      subroutine set_aiccg_bc_composition_nod
!!      subroutine set_aiccg_bc_mag_p_nod
!!@endverbatim
!
      module set_aiccg_bc_fixed
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use m_geometry_data
      use m_phys_constants
!
      implicit none
!
      private :: set_aiccg_bc_scalar_nod, set_aiccg_bc_vector_nod
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_velo_nod
!
      use m_bc_data_velo
      use m_solver_djds_MHD
      use m_velo_matrix
!
      integer(kind = kint) :: nd, k0
!
!
      do nd = 1, n_vector
        if(num_idx_ibc2_v(nd) .ne. 0) then
          do k0 = 1, num_idx_ibc2_v(nd)
            call set_aiccg_bc_vector_nod(ele1%nnod_4_ele,               &
     &          ele_bc2_v_id(k0,nd), nod_bc2_v_id(k0,nd), nd, nd,       &
     &          DJDS_fluid, Vmat_DJDS)
          end do
        end if
      end do
!
      end subroutine set_aiccg_bc_velo_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_velo_rot
!
      use m_bc_data_rotate
      use m_solver_djds_MHD
      use m_velo_matrix
!
      integer (kind = kint) :: k0
!
!
      do k0 = 1, num_index_ibc_vrot
        call set_aiccg_bc_vector_nod(ele1%nnod_4_ele,                   &
     &      ele_bc_vrot_id(k0), nod_bc_vrot_id(k0), ione, n_vector,     &
     &      DJDS_fluid, Vmat_DJDS)
      end do
!
      end subroutine set_aiccg_bc_velo_rot
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_vecp_nod
!
      use m_bc_data_vect_p
      use m_solver_djds_MHD
      use m_magne_matrix
!
      integer(kind = kint) :: nd, k0
!
!
      do nd = 1, n_vector
        if ( num_idx_ibc2_vp(nd) .ne. 0 ) then
          do k0 = 1, num_idx_ibc2_vp(nd)
            call set_aiccg_bc_vector_nod(ele1%nnod_4_ele,               &
     &          ele_bc2_vp_id(k0,nd), nod_bc2_vp_id(k0,nd), nd, nd,     &
     &          DJDS_entire, Bmat_DJDS)
          end do
        end if
      end do
!
      end subroutine set_aiccg_bc_vecp_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_magne_nod
!
      use m_bc_data_magne
      use m_solver_djds_MHD
      use m_magne_matrix
!
      integer(kind = kint) :: nd, k0
!
!
      do nd = 1, n_vector
        if ( num_idx_ibc2_b(nd) .ne. 0 ) then
          do k0 = 1, num_idx_ibc2_b(nd)
            call set_aiccg_bc_vector_nod(ele1%nnod_4_ele,               &
     &          ele_bc2_b_id(k0,nd), nod_bc2_b_id(k0,nd), nd, nd,       &
     &          DJDS_entire, Bmat_DJDS)
          end do
        end if
      end do
!
      end subroutine set_aiccg_bc_magne_nod
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_press_nod
!
      use m_bc_data_press
      use m_solver_djds_MHD
      use m_velo_matrix
!
      integer (kind = kint) :: k0
!
!
      if(num_index_ibc2_press .le. 0) return
      do k0 = 1, num_index_ibc2_press
        call set_aiccg_bc_scalar_nod(num_t_linear,                     &
     &      ele_bc2_p_id(k0), nod_bc2_p_id(k0), DJDS_fl_l, Pmat_DJDS)
      end do
!
      end subroutine set_aiccg_bc_press_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_temp_nod
!
      use m_bc_data_ene
      use m_solver_djds_MHD
      use m_temp_matrix
!
      integer (kind = kint) :: k0
!
!
      if(num_index_ibc2_temp .le. 0) return
      do k0 = 1, num_index_ibc2_temp
        call set_aiccg_bc_scalar_nod(ele1%nnod_4_ele,                   &
     &      ele_bc2_temp_id(k0), nod_bc2_temp_id(k0),                   &
     &      DJDS_fluid, Tmat_DJDS)
      end do
!
      end subroutine set_aiccg_bc_temp_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_composition_nod
!
      use m_bc_data_composition
      use m_solver_djds_MHD
      use m_light_element_matrix
!
      integer (kind = kint) :: k0
!
!
      if(num_index_ibc2_compsition .le. 0)  return
      do k0 = 1, num_index_ibc2_compsition
        call set_aiccg_bc_scalar_nod(ele1%nnod_4_ele,                   &
     &      ele_bc2_composit_id(k0), nod_bc2_composit_id(k0),           &
     &      DJDS_fluid, Cmat_DJDS)
      end do
!
      end subroutine set_aiccg_bc_composition_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_mag_p_nod
!
      use m_bc_data_magne_p
      use m_solver_djds_MHD
      use m_magne_matrix
!
      integer (kind = kint) :: k0
!
!
      if(num_index_ibc2_mag_p .le. 0) return
      do k0 = 1, num_index_ibc2_mag_p
        call set_aiccg_bc_scalar_nod(num_t_linear,                      &
     &      ele_bc2_mag_p_id(k0), nod_bc2_mag_p_id(k0),                 &
     &      DJDS_linear, Fmat_DJDS)
      end do
!
      end subroutine set_aiccg_bc_mag_p_nod
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_scalar_nod(nnod_1ele,                     &
     &          iele, nod_bc2_id, djds_tbl, mat11)
!
      use t_solver_djds
      use m_geometry_data
      use set_aiccg_bc_node_type
!
      integer(kind = kint), intent(in) :: nnod_1ele
      integer(kind = kint), intent(in) :: iele, nod_bc2_id
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      type(DJDS_MATRIX), intent(inout) :: mat11
!
      integer (kind = kint) :: k1, k2
!
!
      k1 = nod_bc2_id
      do k2 = 1, nnod_1ele
        call set_bc_4_scalar_mat_type                                   &
     &     (ele1%ie(iele,k1), ele1%ie(iele,k2), djds_tbl, mat11)
      end do
!
      k2 = nod_bc2_id
      do k1 = 1, nnod_1ele
        call set_bc_4_scalar_mat_type                                   &
     &     (ele1%ie(iele,k1), ele1%ie(iele,k2), djds_tbl, mat11)
      end do
!
      end subroutine set_aiccg_bc_scalar_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_vector_nod(nnod_1ele, iele, nod_bc2_id,   &
     &          nst, ned, djds_tbl, mat33)
!
      use t_solver_djds
      use m_geometry_data
      use set_aiccg_bc_node_type
!
      integer(kind = kint), intent(in) :: nnod_1ele, nst, ned
      integer(kind = kint), intent(in) :: iele, nod_bc2_id
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      type(DJDS_MATRIX), intent(inout) :: mat33
!
      integer(kind = kint) :: k1, k2
!
!
      k1 = nod_bc2_id
      do k2 = 1, nnod_1ele
        call set_bc_4_vector_mat_type(nst, ned,                         &
     &      ele1%ie(iele,k1), ele1%ie(iele,k2), djds_tbl, mat33)
      end do
!
      k2 = nod_bc2_id
      do k1 = 1, nnod_1ele
        call set_bc_4_vector_mat_type(nst, ned,                         &
     &      ele1%ie(iele,k1), ele1%ie(iele,k2), djds_tbl, mat33)
      end do
!
      end subroutine set_aiccg_bc_vector_nod
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_bc_fixed
