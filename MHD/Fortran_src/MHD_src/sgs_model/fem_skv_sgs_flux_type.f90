!fem_skv_sgs_flux_type.f90
!      module fem_skv_sgs_flux_type
!
!     programmed by H. Matsui in April, 2012
!
!      subroutine fem_skv_sgs_flux_pg_type(iele_fsmp_stack, n_int, k2,  &
!     &          dvx, i_filter, nd_t, ele, jac_3d, FEM_elens, fem_wk)
!      subroutine fem_skv_sgs_flux_upw_type(iele_fsmp_stack, n_int, k2, &
!     &          dvx, i_filter, nd_t, ele, jac_3d, FEM_elens, fem_wk)
!
!      subroutine fem_skv_sgs_uxb_pg_type(iele_fsmp_stack, n_int, k2,   &
!     &          dvx, i_filter, nd, ele, jac_3d, FEM_elens, fem_wk)
!      subroutine fem_skv_sgs_uxb_upw_type(iele_fsmp_stack, n_int, k2,  &
!     &          dvx, i_filter, nd, ele, jac_3d, FEM_elens, fem_wk)
!
      module fem_skv_sgs_flux_type
!
      use m_precision
!
      use t_geometry_data
      use t_finite_element_mat
      use t_jacobians
      use t_filter_elength
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_flux_pg_type(iele_fsmp_stack, n_int, k2,   &
     &          dvx, i_filter, nd_t, ele, jac_3d, FEM_elens, fem_wk)
!
      use fem_skv_sgs_flux
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_filter, nd_t
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: dvx(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_sgs_flux_pg(ele%numele, ele%nnod_4_ele,              &
     &    ele%nnod_4_ele, iele_fsmp_stack, n_int, k2,                   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    fem_wk%scalar_1, dvx, nd_t, fem_wk%sk6)
!
      end subroutine fem_skv_sgs_flux_pg_type
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_flux_upw_type(iele_fsmp_stack, n_int, k2,  &
     &          dvx, i_filter, nd_t, ele, jac_3d, FEM_elens, fem_wk)
!
      use m_t_int_parameter
      use fem_skv_sgs_flux
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, nd_t, k2
      integer (kind=kint), intent(in) :: i_filter
!
      real (kind=kreal), intent(in) :: dvx(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_sgs_flux_upw(ele%numele, ele%nnod_4_ele,             &
     &    ele%nnod_4_ele, iele_fsmp_stack, n_int, k2,                   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx, dt,      &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    fem_wk%scalar_1, fem_wk%vxe, dvx, nd_t, fem_wk%sk6)
!
      end subroutine fem_skv_sgs_flux_upw_type
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_uxb_pg_type(iele_fsmp_stack, n_int, k2,    &
     &          dvx, i_filter, nd, ele, jac_3d, FEM_elens, fem_wk)
!
      use m_t_int_parameter
      use fem_skv_sgs_uxb
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: dvx(ele%numele,9)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_sgs_uxb_pg(ele%numele, ele%nnod_4_ele,               &
     &    ele%nnod_4_ele, iele_fsmp_stack, n_int, k2,                   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    fem_wk%vector_1, dvx, nd, fem_wk%sk6)
!
      end subroutine fem_skv_sgs_uxb_pg_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_uxb_upw_type(iele_fsmp_stack, n_int, k2,   &
     &          dvx, i_filter, nd, ele, jac_3d, FEM_elens, fem_wk)
!
      use m_t_int_parameter
      use fem_skv_sgs_uxb
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: dvx(ele%numele,9)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_sgs_uxb_upw(ele%numele, ele%nnod_4_ele,              &
     &    ele%nnod_4_ele, iele_fsmp_stack, n_int, k2,                   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx, dt,      &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    fem_wk%vector_1, fem_wk%vxe, dvx, nd, fem_wk%sk6)
!
      end subroutine fem_skv_sgs_uxb_upw_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_induct_t_pg_type(iele_fsmp_stack,          &
     &          n_int, k2, dvx, dbx, i_filter, nd, ele, jac_3d,         &
     &          FEM_elens, fem_wk)
!
      use fem_skv_sgs_induct_t
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
!
      real(kind=kreal), intent(in) :: dvx(ele%numele,3)
      real(kind=kreal), intent(in) :: dbx(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_sgs_induct_t_pg(ele%numele, ele%nnod_4_ele,          &
     &    ele%nnod_4_ele, iele_fsmp_stack, n_int, k2,                   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    fem_wk%vector_1, dvx, dbx, nd, fem_wk%sk6)
!
      end subroutine fem_skv_sgs_induct_t_pg_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_induct_t_upw_type(iele_fsmp_stack,         &
     &          n_int, k2, dvx, dbx, i_filter, nd, ele, jac_3d,         &
     &          FEM_elens, fem_wk)
!
      use m_t_int_parameter
      use fem_skv_sgs_induct_t
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
!
      real(kind=kreal), intent(in) :: dvx(ele%numele,3)
      real(kind=kreal), intent(in) :: dbx(ele%numele,3)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_sgs_induct_t_upw(ele%numele, ele%nnod_4_ele,         &
     &    ele%nnod_4_ele, iele_fsmp_stack, n_int, k2,                   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx, dt,      &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    fem_wk%vector_1, fem_wk%vxe, dvx, dbx, nd, fem_wk%sk6)
!
      end subroutine fem_skv_sgs_induct_t_upw_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_sgs_flux_type
