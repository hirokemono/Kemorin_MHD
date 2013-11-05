!>@file   set_aiccg_bc_node_type.f90
!!@brief  module set_aiccg_bc_node_type
!!
!!@author H. Matsui
!!@date Programmed in June, 2005
!!@date Modified in Nov., 2013
!
!>     DJDS matrix for light element
!!
!!@verbatim
!!      subroutine set_aiccg_bc_scalar_nod_type(nnod, mesh, scalar_bc,  &
!!     &          djds_tbl, mat11)
!!        integer (kind = kint), intent(in) :: nnod
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(vect_fixed_nod_bc_type), intent(in) :: vector_bc
!!        type(DJDS_ordering_table), intent(in) :: djds_tbl
!!        type(DJDS_MATRIX), intent(inout) :: mat11
!!      subroutine set_aiccg_bc_vector_nod_type(mesh, vector_bc,        &
!!     &          djds_tbl, mat33)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(vect_fixed_nod_bc_type), intent(in) :: vector_bc
!!        type(DJDS_ordering_table), intent(in) :: djds_tbl
!!        type(DJDS_MATRIX), intent(inout) :: mat33
!!
!!      subroutine set_aiccg_bc_vector_rot_type(mesh, rot_bc,           &
!!     &          djds_tbl, mat33)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(vect_fixed_nod_bc_type), intent(in) :: vector_bc
!!        type(DJDS_ordering_table), intent(in) :: djds_tbl
!!        type(DJDS_MATRIX), intent(inout) :: mat33
!!
!!      subroutine set_bc_4_vector_mat_type(nst, ned, nod1, nod2,       &
!!     &          djds_tbl, mat33)
!!        integer (kind = kint), intent(in) :: nst, ned
!!        integer (kind = kint), intent(in) :: nod1, nod2
!!        type(DJDS_ordering_table), intent(in) :: djds_tbl
!!        type(DJDS_MATRIX), intent(inout) :: mat33
!!
!!      subroutine set_bc_4_scalar_mat_type(nod1, nod2, djds_tbl, mat11)
!!      integer (kind = kint), intent(in) :: nod1, nod2
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(DJDS_ordering_table), intent(in) :: djds_tbl
!!        type(DJDS_MATRIX), intent(inout) :: mat11
!!@endverbatim
!
      module set_aiccg_bc_node_type
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use t_mesh_data
      use t_solver_djds
      use t_nodal_bc_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_vector_nod_type(mesh, vector_bc,          &
     &          djds_tbl, mat33)
!
      type(mesh_geometry), intent(in) :: mesh
      type(vect_fixed_nod_bc_type), intent(in) :: vector_bc
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(inout) :: mat33
!
      integer(kind = kint) :: nd, iele, k0, k1, k2, nod1, nod2
!
!
      do nd = 1, n_vector
        do k0 = 1, vector_bc%num_idx_ibc2(nd)
          iele = vector_bc%ele_bc2_id(k0,nd)
!
          k1 = vector_bc%nod_bc2_id(k0,nd)
          do k2 = 1, mesh%ele%nnod_4_ele
            nod1 = mesh%ele%ie(iele,k1)
            nod2 = mesh%ele%ie(iele,k2)
            call set_bc_4_vector_mat_type(nd, nd, nod1, nod2,           &
     &          djds_tbl, mat33)
          end do
!
          k2 = vector_bc%nod_bc2_id(k0,nd)
          do k1 = 1, mesh%ele%nnod_4_ele
            nod1 = mesh%ele%ie(iele,k1)
            nod2 = mesh%ele%ie(iele,k2)
            call set_bc_4_vector_mat_type(nd, nd, nod1, nod2,          &
     &          djds_tbl, mat33)
          end do
!
        end do
      end do
!
      end subroutine set_aiccg_bc_vector_nod_type
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_vector_rot_type(mesh, rot_bc,             &
     &          djds_tbl, mat33)
!
      type(mesh_geometry), intent(in) :: mesh
      type(scaler_rotaion_nod_bc_type), intent(in) :: rot_bc
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(inout) :: mat33
!
      integer(kind = kint) :: nd, iele, k0, k1, k2, nod1, nod2
!
!
      do nd = 1, n_vector
        do k0 = 1, rot_bc%num_idx_ibc
          iele = rot_bc%ele_bc_id(k0)
!
          k1 = rot_bc%nod_bc_id(k0)
          do k2 = 1, mesh%ele%nnod_4_ele
            nod1 = mesh%ele%ie(iele,k1)
            nod2 = mesh%ele%ie(iele,k2)
            call set_bc_4_vector_mat_type(ione, n_vector, nod1, nod2,   &
     &          djds_tbl, mat33)
          end do
!
          k2 = rot_bc%nod_bc_id(k0)
          do k1 = 1, mesh%ele%nnod_4_ele
            nod1 = mesh%ele%ie(iele,k1)
            nod2 = mesh%ele%ie(iele,k2)
            call set_bc_4_vector_mat_type(ione, n_vector, nod1, nod2,   &
     &          djds_tbl, mat33)
          end do
!
        end do
      end do
!
      end subroutine set_aiccg_bc_vector_rot_type
!
! -----------------------------------------------------------------------
!
      subroutine set_aiccg_bc_scalar_nod_type(nnod, mesh, scalar_bc,    &
     &          djds_tbl, mat11)
!
      integer (kind = kint), intent(in) :: nnod
      type(mesh_geometry), intent(in) :: mesh
      type(scaler_fixed_nod_bc_type), intent(in) :: scalar_bc
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(inout) :: mat11
!
      integer (kind = kint) :: iele, k0, k1, k2, nod1, nod2
!
!
        do k0 = 1, scalar_bc%num_idx_ibc2
          iele = scalar_bc%ele_bc2_id(k0)
!
          k1 = scalar_bc%nod_bc2_id(k0)
          do k2 = 1, nnod
            nod1 = mesh%ele%ie(iele,k1)
            nod2 = mesh%ele%ie(iele,k2)
            call set_bc_4_scalar_mat_type(nod1, nod2, djds_tbl, mat11)
          end do
!
          k2 = scalar_bc%nod_bc2_id(k0)
          do k1 = 1, nnod
            nod1 = mesh%ele%ie(iele,k1)
            nod2 = mesh%ele%ie(iele,k2)
            call set_bc_4_scalar_mat_type(nod1, nod2, djds_tbl, mat11)
          end do
!
        end do
!
      end subroutine set_aiccg_bc_scalar_nod_type
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
      call set_off_diag_type(mat33%num_diag, mat33%internal_diag,       &
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
      call set_off_diag_type(mat11%num_diag, mat11%internal_diag,       &
     &    djds_tbl, nod1, nod2, mat_num)
      call correct_matrix11_4_boundary                                  &
     &   (mat11%num_diag, djds_tbl%itotal_u, djds_tbl%itotal_l,         &
     &    mat_num, mat11%aiccg)
!
      end subroutine set_bc_4_scalar_mat_type
!
! -----------------------------------------------------------------------
!
      end module set_aiccg_bc_node_type
