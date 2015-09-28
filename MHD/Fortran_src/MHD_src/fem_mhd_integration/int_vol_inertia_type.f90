!int_vol_inertia_type.f90
!     module int_vol_inertia_type
!
!      Written by H. Matsui on june, 2005
!
!      subroutine int_vol_scalar_inertia_type(mesh, jac_3d,             &
!     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,              &
!     &          i_scalar, vxe, coef, fem_wk, f_nl)
!      subroutine int_vol_vector_inertia_type(mesh, jac_3d,             &
!     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,              &
!     &          i_vector, vxe, coef, fem_wk, f_nl)
!
!      subroutine int_vol_rot_inertia_type(mesh, jac_3d,                &
!     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,              &
!     &          i_vector, wxe, coef, fem_wk, f_nl)
!
!      subroutine int_vol_scalar_inertia_upw_type(mesh, jac_3d,         &
!     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,              &
!     &          i_scalar, vxe, vxe_up, coef, fem_wk, f_nl)
!      subroutine int_vol_vector_inertia_upw_type(mesh, jac_3d,         &
!     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,              &
!     &          i_vector, vxe, vxe_up, coef, fem_wk, f_nl)
!
!      subroutine int_vol_rot_inertia_upw_type(mesh, jac_3d,            &
!     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,              &
!     &          i_vector, wxe, vxe_up, coef, fem_wk, f_nl)
!
      module int_vol_inertia_type
!
      use m_precision
      use m_phys_constants
!
      use t_mesh_data
      use t_phys_data
      use t_jacobians
      use t_finite_element_mat
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_scalar_inertia_type(mesh, jac_3d,              &
     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,               &
     &          i_scalar, vxe, coef, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp_type
      use nodal_fld_cst_to_ele_type
      use fem_skv_nonlinear_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: vxe(mesh%ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_scalar,                                     &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, fem_wk)
!
! -------- loop for shape function for the physical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call scalar_cst_phys_each_ele_type(mesh, nod_fld,               &
     &      k2, i_scalar, coef, fem_wk%scalar_1)
        call fem_skv_scalar_inertia_type(iele_fsmp_stack, n_int, k2,    &
     &      fem_wk%scalar_1, vxe, mesh%ele, jac_3d, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_scalar_inertia_type
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_vector_inertia_type(mesh, jac_3d,              &
     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,               &
     &          i_vector, vxe, coef, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp_type
      use nodal_fld_cst_to_ele_type
      use fem_skv_nonlinear_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: vxe(mesh%ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_vector,                                     &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, fem_wk)
!
! -------- loop for shape function for the physical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_cst_phys_each_ele_type(mesh, nod_fld,               &
     &      k2, i_vector, coef, fem_wk%vector_1)
        call fem_skv_vector_inertia_type(iele_fsmp_stack, n_int, k2,    &
     &      fem_wk%vector_1, vxe, mesh%ele, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_vector_inertia_type
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_rot_inertia_type(mesh, jac_3d,                 &
     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,               &
     &          i_vector, wxe, coef, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp_type
      use nodal_fld_cst_to_ele_type
      use fem_skv_nonlinear_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: wxe(mesh%ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_vector,                                     &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, fem_wk)
!
! -------- loop for shape function for the physical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_cst_phys_each_ele_type(mesh, nod_fld,               &
     &      k2, i_vector, coef, fem_wk%vector_1)
        call fem_skv_rot_inertia_type(iele_fsmp_stack, n_int, k2,       &
     &      fem_wk%vector_1, wxe, mesh%ele, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_rot_inertia_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_scalar_inertia_upw_type(mesh, jac_3d,          &
     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,               &
     &          i_scalar, vxe, vxe_up, coef, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp_type
      use nodal_fld_cst_to_ele_type
      use fem_skv_nonlinear_upw_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: vxe(mesh%ele%numele,3)
      real(kind=kreal), intent(in) :: vxe_up(mesh%ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_scalar,                                     &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, fem_wk)
!
! -------- loop for shape function for the physical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call scalar_cst_phys_each_ele_type(mesh, nod_fld,               &
     &      k2, i_scalar, coef, fem_wk%scalar_1)
        call fem_skv_scalar_inertia_upw_type(iele_fsmp_stack,           &
     &      n_int, k2, vxe, vxe_up, mesh%ele, jac_3d, fem_wk)
      end do
!
      call add1_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_scalar_inertia_upw_type
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_vector_inertia_upw_type(mesh, jac_3d,          &
     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,               &
     &          i_vector, vxe, vxe_up, coef, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp_type
      use nodal_fld_cst_to_ele_type
      use fem_skv_nonlinear_upw_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: vxe(mesh%ele%numele,3)
      real(kind=kreal), intent(in) :: vxe_up(mesh%ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_vector,                                     &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, fem_wk)
!
! -------- loop for shape function for the physical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_cst_phys_each_ele_type(mesh, nod_fld,               &
     &      k2, i_vector, coef, fem_wk%vector_1)
        call fem_skv_vector_inertia_upw_type(iele_fsmp_stack,           &
     &      n_int, k2, vxe, vxe_up, mesh%ele, jac_3d, fem_wk)
      end do
!
      call add3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_vector_inertia_upw_type
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_rot_inertia_upw_type(mesh, jac_3d,             &
     &          nod_fld, rhs_tbl, iele_fsmp_stack, n_int,               &
     &          i_vector, wxe, vxe_up, coef, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp_type
      use nodal_fld_cst_to_ele_type
      use fem_skv_nonlinear_upw_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: coef
      real(kind=kreal), intent(in) :: wxe(mesh%ele%numele,3)
      real(kind=kreal), intent(in) :: vxe_up(mesh%ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_vector,                                     &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, fem_wk)
!
! -------- loop for shape function for the physical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_cst_phys_each_ele_type(mesh, nod_fld,               &
     &      k2, i_vector, coef, fem_wk%vector_1)
        call fem_skv_rot_inertia_upw_type(iele_fsmp_stack, n_int, k2,   &
     &      wxe, vxe_up, mesh%ele, jac_3d, fem_wk)
      end do
!
      call add3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_rot_inertia_upw_type
!
! ----------------------------------------------------------------------
!
      end module int_vol_inertia_type
