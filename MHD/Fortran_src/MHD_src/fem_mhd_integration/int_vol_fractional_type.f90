!
!     module int_vol_fractional_type
!
!      Written by H. Matsui on june, 2005
!
!      subroutine int_vol_div_vect_linear_type(mesh, jac_3d, jac_3d_l,  &
!     &          rhs_tbl, nod_fld, iele_fsmp_stack, n_int, i_vector,    &
!     &          fem_wk, f_l)
!      subroutine int_vol_solenoidal_co_type(mesh, jac_3d, jac_3d_l,    &
!     &          rhs_tbl, nod_fld, iele_fsmp_stack, n_int, i_scalar,    &
!     &          fem_wk, f_nl)
!
!      subroutine int_vol_scalar_diffuse_type(mesh, jac_3d, rhs_tbl,    &
!     &          nod_fld, iele_fsmp_stack, n_int, coef_crank, ak_d,     &
!     &          i_scalar, fem_wk, f_l)
!      subroutine int_vol_vector_diffuse_type(mesh, jac_3d, rhs_tbl,    &
!     &          nod_fld, iele_fsmp_stack, n_int, coef_crank, ak_d,     &
!     &          i_vector, fem_wk, f_l)
!
      module int_vol_fractional_type
!
      use m_precision
!
      use m_phys_constants
      use t_mesh_data
      use t_phys_data
      use t_table_FEM_const
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
      subroutine int_vol_div_vect_linear_type(mesh, jac_3d, jac_3d_l,   &
     &          rhs_tbl, nod_fld, iele_fsmp_stack, n_int, i_vector,     &
     &          fem_wk, f_l)
!
      use cal_skv_to_ff_smp_type
      use nodal_fld_2_each_ele_type
      use fem_skv_vector_diff_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_scalar,                                     &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, fem_wk)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_phys_2_each_ele_type(mesh, nod_fld, k2,             &
     &      i_vector, fem_wk%vector_1)
        call fem_skv_div_to_linear_type(iele_fsmp_stack, n_int, k2,     &
     &      mesh%ele, jac_3d, jac_3d_l, fem_wk)
      end do
!
      call add1_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_l)
!
      end subroutine int_vol_div_vect_linear_type
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_solenoidal_co_type(mesh, jac_3d, jac_3d_l,     &
     &          rhs_tbl, nod_fld, iele_fsmp_stack, n_int, i_scalar,     &
     &          fem_wk, f_nl)
!
      use cal_skv_to_ff_smp_type
      use nodal_fld_2_each_ele_type
      use fem_skv_vector_diff_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
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
! -------- loop for shape function for the phsical values
      do k2=1, num_t_linear
        call scalar_phys_2_each_ele_type(mesh, nod_fld, k2,             &
     &      i_scalar, fem_wk%scalar_1)
        call fem_skv_linear_grad_type(iele_fsmp_stack, n_int, k2,       &
     &      mesh%ele, jac_3d, jac_3d_l, fem_wk)
      end do
!
      call add3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_solenoidal_co_type
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_scalar_diffuse_type(mesh, jac_3d, rhs_tbl,     &
     &          nod_fld, iele_fsmp_stack, n_int, coef_crank, ak_d,      &
     &          i_scalar, fem_wk, f_l)
!
      use cal_skv_to_ff_smp_type
      use nodal_fld_2_each_ele_type
      use fem_skv_diffusion_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, i_scalar
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(mesh%ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6_type(n_scalar,                                     &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, fem_wk)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call scalar_phys_2_each_ele_type(mesh, nod_fld, k2,             &
     &      i_scalar, fem_wk%scalar_1)
        call fem_skv_scalar_diffuse_type(iele_fsmp_stack, n_int, k2,    &
     &      ak_d, mesh%ele, jac_3d, fem_wk)
      end do
!
      call add1_skv_coef_to_ff_v_smp_type(mesh, rhs_tbl,                &
     &    coef_crank, fem_wk, f_l)
!
      end subroutine int_vol_scalar_diffuse_type
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_vector_diffuse_type(mesh, jac_3d, rhs_tbl,     &
     &          nod_fld, iele_fsmp_stack, n_int, coef_crank, ak_d,      &
     &          i_vector, fem_wk, f_l)
!
      use cal_skv_to_ff_smp_type
      use nodal_fld_2_each_ele_type
      use fem_skv_diffusion_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, i_vector
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(mesh%ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6_type(n_vector,                                     &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, fem_wk)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_phys_2_each_ele_type(mesh, nod_fld, k2,             &
     &      i_vector, fem_wk%vector_1)
        call fem_skv_vector_diffuse_type(iele_fsmp_stack, n_int, k2,    &
     &      ak_d, mesh%ele, jac_3d, fem_wk)
      end do
!
      call add3_skv_coef_to_ff_v_smp_type(mesh, rhs_tbl,                &
     &    coef_crank, fem_wk, f_l)
!
      end subroutine int_vol_vector_diffuse_type
!
!  ---------------------------------------------------------------------
!
      end module int_vol_fractional_type
