!int_vol_SGS_div_flux_type.f90
!     module int_vol_SGS_div_flux_type
!
!      Written by H. Matsui on june, 2005
!
!      subroutine int_vol_div_SGS_vec_flux_type                         &
!     &          (mesh, jac_3d, rhs_tbl, FEM_elens, nod_fld,            &
!     &           iele_fsmp_stack, n_int, i_vector, i_scalar,           &
!     &           i_SGS_flux, i_filter, ak_diff, coef, fem_wk, f_nl)
!      subroutine int_vol_div_SGS_tsr_flux_type                         &
!     &          (mesh, jac_3d, rhs_tbl, FEM_elens, nod_fld,            &
!     &           iele_fsmp_stack, n_int, i_vect, i_SGS_flux, i_filter, &
!     &           ak_diff, coef, fem_wk, f_nl)
!
!      subroutine int_vol_div_SGS_vflux_upw_type                        &
!     &          (mesh, jac_3d, rhs_tbl, FEM_elens, nod_fld,            &
!     &           iele_fsmp_stack, n_int, i_vector, i_scalar,           &
!     &           i_SGS_flux, i_filter, ak_diff,                        &
!     &           ncomp_ele, ie_upw, d_ele, coef, fem_wk, f_nl)
!      subroutine int_vol_div_SGS_tflux_upw_type                        &
!     &          (mesh, jac_3d, rhs_tbl, FEM_elens, nod_fld,            &
!     &           iele_fsmp_stack, n_int, i_vect, i_SGS_flux, i_filter, &
!     &           ak_diff, ncomp_ele, ie_upw, d_ele, coef, fem_wk, f_nl)
!
      module int_vol_SGS_div_flux_type
!
      use m_precision
      use m_control_parameter
!
      use m_phys_constants
      use t_mesh_data
      use t_phys_data
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
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
      subroutine int_vol_div_SGS_vec_flux_type                          &
     &          (mesh, jac_3d, rhs_tbl, FEM_elens, nod_fld,             &
     &           iele_fsmp_stack, n_int, i_vector, i_scalar,            &
     &           i_SGS_flux, i_filter, ak_diff, coef, fem_wk, f_nl)
!
      use m_int_vol_data
!
      use nodal_fld_cst_to_ele_type
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_type
      use fem_skv_div_sgs_flux_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vector, i_scalar
      integer(kind = kint), intent(in) :: i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(mesh%ele%numele)
      real(kind = kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_scalar, mesh%ele, fem_wk)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call SGS_vector_cst_each_ele_type(mesh, nod_fld, k2,            &
     &        i_vector, i_scalar, i_SGS_flux, coef,                     &
     &        fem_wk%sgs_v, fem_wk%vector_1)
        call fem_skv_div_sgs_vector(iele_fsmp_stack,                    &
     &        n_int, k2, i_filter, ak_diff, mesh%ele, jac_3d,           &
     &        FEM_elens, fem_wk%sgs_v, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_div_SGS_vec_flux_type
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_tsr_flux_type                          &
     &          (mesh, jac_3d, rhs_tbl, FEM_elens, nod_fld,             &
     &           iele_fsmp_stack, n_int, i_vect, i_SGS_flux, i_filter,  &
     &           ak_diff, coef, fem_wk, f_nl)
!
      use m_int_vol_data
!
      use nodal_fld_cst_to_ele_type
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_type
      use fem_skv_div_sgs_flux_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vect, i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(mesh%ele%numele)
      real(kind = kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_vector, mesh%ele, fem_wk)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call SGS_tensor_cst_each_ele_type(mesh, nod_fld, k2,            &
     &        i_vect, i_SGS_flux, coef, fem_wk%sgs_t, fem_wk%tensor_1)
        call fem_skv_div_sgs_tensor(iele_fsmp_stack,                    &
     &        n_int, k2, i_filter, ak_diff, mesh%ele, jac_3d,           &
     &        FEM_elens, fem_wk%sgs_t, fem_wk%tensor_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_div_SGS_tsr_flux_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_vflux_upw_type                         &
     &          (mesh, jac_3d, rhs_tbl, FEM_elens, nod_fld,             &
     &           iele_fsmp_stack, n_int, i_vector, i_scalar,            &
     &           i_SGS_flux, i_filter, ak_diff,                         &
     &           ncomp_ele, ie_upw, d_ele, coef, fem_wk, f_nl)
!
      use m_int_vol_data
!
      use nodal_fld_cst_to_ele_type
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_type
      use fem_skv_div_sgs_flux_upw_t
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vector, i_scalar
      integer(kind = kint), intent(in) :: i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter
!
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      real(kind=kreal), intent(in) :: ak_diff(mesh%ele%numele)
      real(kind = kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_scalar, mesh%ele, fem_wk)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call SGS_vector_cst_each_ele_type(mesh, nod_fld, k2,            &
     &        i_vector, i_scalar, i_SGS_flux, coef,                     &
     &        fem_wk%sgs_v, fem_wk%vector_1)
        call fem_skv_div_sgs_vector_upwind(iele_fsmp_stack,             &
     &      n_int, k2, i_filter, ak_diff, mesh%ele, jac_3d, FEM_elens,  &
     &      d_ele(1,ie_upw), fem_wk%sgs_v, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_div_SGS_vflux_upw_type
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_tflux_upw_type                         &
     &          (mesh, jac_3d, rhs_tbl, FEM_elens, nod_fld,             &
     &           iele_fsmp_stack, n_int, i_vect, i_SGS_flux, i_filter,  &
     &           ak_diff, ncomp_ele, ie_upw, d_ele, coef, fem_wk, f_nl)
!
      use m_int_vol_data
!
      use nodal_fld_cst_to_ele_type
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_type
      use fem_skv_div_sgs_flux_upw_t
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: i_vect, i_SGS_flux
      integer(kind = kint), intent(in) :: i_filter
!
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      real(kind=kreal), intent(in) :: ak_diff(mesh%ele%numele)
      real(kind = kreal), intent(in) :: coef
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6_type(n_vector, mesh%ele, fem_wk)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, mesh%ele%nnod_4_ele
        call SGS_tensor_cst_each_ele_type(mesh, nod_fld, k2,            &
     &        i_vect, i_SGS_flux, coef, fem_wk%sgs_t, fem_wk%tensor_1)
        call fem_skv_div_sgs_tensor_upwind(iele_fsmp_stack,             &
     &      n_int, k2, i_filter, ak_diff, mesh%ele, jac_3d, FEM_elens,  &
     &      d_ele(1,ie_upw), fem_wk%sgs_t, fem_wk%tensor_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_type(mesh, rhs_tbl, fem_wk, f_nl)
!
      end subroutine int_vol_div_SGS_tflux_upw_type
!
! ----------------------------------------------------------------------
!
      end module int_vol_SGS_div_flux_type
