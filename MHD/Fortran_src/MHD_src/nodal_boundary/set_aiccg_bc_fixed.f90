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
!!      subroutine set_aiccg_bc_scalar_nod                              &
!!     &         (nnod_1ele, ele, scalar_bc, djds_tbl, mat11)
!!      subroutine set_aiccg_bc_vector_nod                              &
!!     &         (ele, vector_bc, djds_tbl, mat33)
!!      subroutine set_aiccg_bc_velo_rot                                &
!!     &         (ele, nod_bc_rot, djds_tbl, mat33)
!!@endverbatim
!
      module set_aiccg_bc_fixed
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_nodal_bc_data
      use t_solver_djds
!
      implicit none
!
      private :: aiccg_bc_vector_nod
      private :: set_bc_4_vector_mat_type, set_bc_4_scalar_mat_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_scalar_nod                                &
     &         (nnod_1ele, ele, scalar_bc, djds_tbl, mat11)
!
      type(element_data), intent(in) :: ele
      type(scaler_fixed_nod_bc_type), intent(in) :: scalar_bc
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      integer(kind = kint), intent(in) :: nnod_1ele
!
      type(DJDS_MATRIX), intent(inout) :: mat11
!
      integer (kind = kint) :: k0, iele, k1, k2
!
!
      if(scalar_bc%num_idx_ibc2 .le. 0) return
      do k0 = 1, scalar_bc%num_idx_ibc2
        iele = scalar_bc%ele_bc2_id(k0)
        k1 = scalar_bc%nod_bc2_id(k0)
        do k2 = 1, nnod_1ele
          call set_bc_4_scalar_mat_type                                 &
     &     (ele%ie(iele,k1), ele%ie(iele,k2), djds_tbl, mat11)
        end do
!
        k2 = scalar_bc%nod_bc2_id(k0)
        do k1 = 1, nnod_1ele
          call set_bc_4_scalar_mat_type                                 &
     &     (ele%ie(iele,k1), ele%ie(iele,k2), djds_tbl, mat11)
        end do
      end do
!
      end subroutine set_aiccg_bc_scalar_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_vector_nod                                &
     &         (ele, vector_bc, djds_tbl, mat33)
!
      type(element_data), intent(in) :: ele
      type(vect_fixed_nod_bc_type), intent(in) :: vector_bc
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      type(DJDS_MATRIX), intent(inout) :: mat33
!
      integer(kind = kint) :: nd, k0
!
!
      do nd = 1, n_vector
        do k0 = 1, vector_bc%num_idx_ibc2(nd)
          call aiccg_bc_vector_nod(ele, ele%nnod_4_ele,                 &
     &        vector_bc%ele_bc2_id(k0,nd), vector_bc%nod_bc2_id(k0,nd), &
     &        nd, nd, djds_tbl, mat33)
        end do
      end do
!
      end subroutine set_aiccg_bc_vector_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_velo_rot                                  &
     &         (ele, nod_bc_rot, djds_tbl, mat33)
!
      type(element_data), intent(in) :: ele
      type(scaler_rotaion_nod_bc_type), intent(in) :: nod_bc_rot
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      type(DJDS_MATRIX), intent(inout) :: mat33
!
      integer (kind = kint) :: k0
!
!
      do k0 = 1, nod_bc_rot%num_idx_ibc
        call aiccg_bc_vector_nod(ele, ele%nnod_4_ele,                   &
     &      nod_bc_rot%ele_bc_id(k0), nod_bc_rot%nod_bc_id(k0),         &
     &      ione, n_vector, djds_tbl, mat33)
      end do
!
      end subroutine set_aiccg_bc_velo_rot
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine aiccg_bc_vector_nod(ele, nnod_1ele,                    &
     &          iele, nod_bc2_id, nst, ned, djds_tbl, mat33)
!
      type(element_data), intent(in) :: ele
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      integer(kind = kint), intent(in) :: nnod_1ele, nst, ned
      integer(kind = kint), intent(in) :: iele, nod_bc2_id
!
      type(DJDS_MATRIX), intent(inout) :: mat33
!
      integer(kind = kint) :: k1, k2
!
!
      k1 = nod_bc2_id
      do k2 = 1, nnod_1ele
        call set_bc_4_vector_mat_type(nst, ned,                         &
     &      ele%ie(iele,k1), ele%ie(iele,k2), djds_tbl, mat33)
      end do
!
      k2 = nod_bc2_id
      do k1 = 1, nnod_1ele
        call set_bc_4_vector_mat_type(nst, ned,                         &
     &      ele%ie(iele,k1), ele%ie(iele,k2), djds_tbl, mat33)
      end do
!
      end subroutine aiccg_bc_vector_nod
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_bc_4_vector_mat_type(nst, ned, nod1, nod2,         &
     &          djds_tbl, mat33)
!
      use set_idx_4_mat_type
      use correct_matrix_4_boundary
!
      integer (kind = kint), intent(in) :: nst, ned
      integer (kind = kint), intent(in) :: nod1, nod2
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(inout) :: mat33
!
      integer (kind = kint) :: nd2, mat_num
!
!
      call set_DJDS_off_diag_type(mat33%num_diag, mat33%internal_diag,  &
     &    djds_tbl, nod1, nod2, mat_num)
!
      do nd2 = nst, ned
        call correct_matrix33_4_boundary(mat33%num_diag,                &
     &      djds_tbl%itotal_u, djds_tbl%itotal_l, nd2, mat_num,         &
     &      mat33%aiccg)
      end do
!
      end subroutine set_bc_4_vector_mat_type
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_4_scalar_mat_type(nod1, nod2, djds_tbl, mat11)
!
      use set_idx_4_mat_type
      use correct_matrix_4_boundary
!
      integer (kind = kint), intent(in) :: nod1, nod2
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(inout) :: mat11
!
      integer (kind = kint) :: mat_num
!
!
      call set_DJDS_off_diag_type(mat11%num_diag, mat11%internal_diag,  &
     &    djds_tbl, nod1, nod2, mat_num)
      call correct_matrix11_4_boundary                                  &
     &   (mat11%num_diag, djds_tbl%itotal_u, djds_tbl%itotal_l,         &
     &    mat_num, mat11%aiccg)
!
      end subroutine set_bc_4_scalar_mat_type
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_bc_fixed
