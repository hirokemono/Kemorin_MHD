!fem_skv_poisson_sgs_bc_t.f90
!     module fem_skv_poisson_sgs_bc_t
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on AUg., 2007
!
!      subroutine fem_skv_poisson_sgs_fix_type                          &
!     &        (num_index_ibc, ele_bc_id, ibc_stack_smp, k2, n_int,     &
!     &         i_filter, ak_diff, ele, jac_3d_l, FEM_elens, fem_wk)
!      subroutine fem_skv_diffuse_sgs_fix_type                          &
!     &        (num_index_ibc, ele_bc_id, ibc_stack_smp, k2, nd, n_int, &
!     &         i_filter, ak_diff, ak_d, ele, jac_3d, FEM_elens, fem_wk)
!
      module fem_skv_poisson_sgs_bc_t
!
      use m_precision
      use m_constants
      use m_geometry_constants
      use m_phys_constants
      use m_machine_parameter
!
      use t_geometry_data
      use t_jacobians
      use t_filter_elength
      use t_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_sgs_fix_type                           &
     &        (num_index_ibc, ele_bc_id, ibc_stack_smp, k2, n_int,      &
     &         i_filter, ak_diff, ele, jac_3d_l, FEM_elens, fem_wk)
!
      use fem_skv_poisson_sgs_bc
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
      integer(kind=kint), intent(in) :: i_filter
      integer(kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_poisson_sgs_fixed                                    &
     &   (ele%numele, num_t_linear, num_t_linear, np_smp,               &
     &    num_index_ibc, ele_bc_id, ibc_stack_smp, k2, n_int,           &
     &  jac_3d_l%ntot_int, jac_3d_l%xjac, jac_3d_l%dnx, jac_3d_l%dnx,   &
     &  FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM_elens%nele_filter_mom,                                      &
     &  FEM_elens%elen_ele%diff2%df_x2, FEM_elens%elen_ele%diff2%df_y2, &
     &  FEM_elens%elen_ele%diff2%df_z2, FEM_elens%elen_ele%diff2%df_xy, &
     &  FEM_elens%elen_ele%diff2%df_yz, FEM_elens%elen_ele%diff2%df_zx, &
     &  ak_diff, fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_poisson_sgs_fix_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_diffuse_sgs_fix_type                           &
     &        (num_index_ibc, ele_bc_id, ibc_stack_smp, k2, nd, n_int,  &
     &         i_filter, ak_diff, ak_d, ele, jac_3d, FEM_elens, fem_wk)
!
      use fem_skv_diffuse_sgs_bc
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: num_index_ibc
      integer(kind=kint), intent(in) :: ele_bc_id(num_index_ibc)
      integer(kind=kint), intent(in) :: ibc_stack_smp(0:np_smp)
      integer(kind=kint), intent(in) :: i_filter
      integer(kind=kint), intent(in) :: n_int, k2, nd
!
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_diffuse_sgs_fixed                                    &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,           &
     &    num_index_ibc, ele_bc_id, ibc_stack_smp, k2, nd, n_int,       &
     &  jac_3d%ntot_int, jac_3d%xjac, jac_3d%dnx, jac_3d%dnx,           &
     &  FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM_elens%nele_filter_mom,                                      &
     &  FEM_elens%elen_ele%diff2%df_x2, FEM_elens%elen_ele%diff2%df_y2, &
     &  FEM_elens%elen_ele%diff2%df_z2, FEM_elens%elen_ele%diff2%df_xy, &
     &  FEM_elens%elen_ele%diff2%df_yz, FEM_elens%elen_ele%diff2%df_zx, &
     &  ak_diff, ak_d, fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_diffuse_sgs_fix_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_poisson_sgs_bc_t
