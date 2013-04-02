!fem_skv_div_sgs_flux_type.f90
!      module fem_skv_div_sgs_flux_type
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on Aug., 2006
!
!      subroutine fem_skv_div_sgs_vector_type(iele_fsmp_stack,          &
!     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,  &
!     &          fem_wk)
!      subroutine fem_skv_div_sgs_tensor_type(iele_fsmp_stack,          &
!     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,  &
!     &          fem_wk)
!      subroutine fem_skv_div_sgs_asym_t_type(iele_fsmp_stack,          &
!     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,  &
!     &          fem_wk)
!
!      subroutine fem_skv_scl_inertia_sgs_type(iele_fsmp_stack,         &
!     &          n_int, k2, vxe, ele, jac_3d, fem_wk)
!      subroutine fem_skv_vec_inertia_sgs_type(iele_fsmp_stack,         &
!     &          n_int, k2, vxe, ele, jac_3d, fem_wk)
!      subroutine fem_skv_inertia_rot_sgs_type(iele_fsmp_stack,         &
!     &          n_int, k2, wxe, ele, jac_3d, fem_wk)
!
!      subroutine fem_skv_scl_inertia_modsgs_type(iele_fsmp_stack,      &
!     &          n_int, k2, i_filter, ak_diff, vxe,                     &
!     &          ele, jac_3d, FEM_elens, fem_wk)
!      subroutine fem_skv_vec_inertia_modsgs_type(iele_fsmp_stack,      &
!     &          n_int, k2, i_filter, ak_diff, vxe,                     &
!     &          ele, jac_3d, FEM_elens, fem_wk)
!
      module fem_skv_div_sgs_flux_type
!
      use m_precision
!
      use t_geometry_data
      use t_filter_elength
      use t_finite_element_mat
      use t_jacobians
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
      use m_fem_gauss_int_coefs
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_vector_type(iele_fsmp_stack,           &
     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,   &
     &          fem_wk)
!
      use fem_skv_div_vect_w_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_div_sgs_vector_pg                                    &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%sgs_v, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_sgs_vector_type
!
!   --------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_tensor_type(iele_fsmp_stack,           &
     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,   &
     &          fem_wk)
!
      use fem_skv_div_tsr_w_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_div_tensor_w_sgs_pg                                  &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%sgs_t, fem_wk%tensor_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_sgs_tensor_type
!
!   --------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_asym_t_type(iele_fsmp_stack,           &
     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,   &
     &          fem_wk)
!
      use fem_skv_div_ast_w_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_div_sgs_asym_t_pg                                    &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%sgs_v, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_sgs_asym_t_type
!
!   --------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scl_inertia_sgs_type(iele_fsmp_stack,          &
     &          n_int, k2, vxe, ele, jac_3d, fem_wk)
!
      use fem_skv_inertia_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_scalar_inertia_sgs                                   &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx,                           &
     &    fem_wk%scalar_1, fem_wk%sgs_v, vxe, fem_wk%sk6)
!
      end subroutine fem_skv_scl_inertia_sgs_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vec_inertia_sgs_type(iele_fsmp_stack,          &
     &          n_int, k2, vxe, ele, jac_3d, fem_wk)
!
      use fem_skv_inertia_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_vector_inertia_sgs                                   &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx,                           &
     &    fem_wk%vector_1, fem_wk%sgs_t, vxe, fem_wk%sk6)
!
      end subroutine fem_skv_vec_inertia_sgs_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_inertia_rot_sgs_type(iele_fsmp_stack,          &
     &          n_int, k2, wxe, ele, jac_3d, fem_wk)
!
      use fem_skv_inertia_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: wxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_inertia_rot_sgs                                      &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%an, jac_3d%dnx,                &
     &    fem_wk%vector_1, fem_wk%sgs_t, wxe, fem_wk%sk6)
!
      end subroutine fem_skv_inertia_rot_sgs_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scl_inertia_modsgs_type(iele_fsmp_stack,       &
     &          n_int, k2, i_filter, ak_diff, vxe,                      &
     &          ele, jac_3d, FEM_elens, fem_wk)
!
      use fem_skv_inertia1_sgsmod
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      call fem_skv_scalar_inertia_sgsmod                                &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%scalar_1, fem_wk%sgs_v, fem_wk%vector_1,      &
     &    vxe, fem_wk%sk6)
!
      end subroutine fem_skv_scl_inertia_modsgs_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vec_inertia_modsgs_type(iele_fsmp_stack,       &
     &          n_int, k2, i_filter, ak_diff, vxe,                      &
     &          ele, jac_3d, FEM_elens, fem_wk)
!
      use fem_skv_inertia3_sgsmod
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_vector_inertia_sgsmod                                &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%vector_1, fem_wk%sgs_t, fem_wk%tensor_1,      &
     &    vxe, fem_wk%sk6)
!
      end subroutine fem_skv_vec_inertia_modsgs_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_div_sgs_flux_type
