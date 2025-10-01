!
!      module cal_lsq_model_coefs
!
!     Written by H. Matsui on Oct. 2005
!
!!      subroutine cal_model_coef_4_flux                                &
!!     &        (iflag_Csim_marging, layer_tbl, node, ele, iSGS_wk,     &
!!     &         nod_fld, g_FEM, jac_3d_q, jac_3d_l, numdir, n_int,     &
!!     &         nlayer_SGS, cor_sgs, cor_sgs_w, sgs_f_coef, sgs_c_coef,&
!!     &         sgs_f_whole, sgs_c_whole, wk_lsq)
!!      subroutine cal_lsq_diff_coef                                    &
!!     &         (iflag_Csim_marging, iele_fsmp_stack, node, ele,       &
!!     &          iSGS_wk, nod_fld, g_FEM, jac_3d_q, jac_3d_l,          &
!!     &          numdir, n_int, cor_diff_w,                            &
!!     &          diff_f_whole, diff_c_whole, wk_lsq)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(dynamic_SGS_work_address), intent(in) :: iSGS_wk
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(dynamic_least_suare_data), intent(inout) :: wk_lsq
!
      module cal_lsq_model_coefs
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data
      use t_SGS_model_coef_labels
      use t_phys_data
      use t_layering_ele_list
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_material_property
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
!
      use set_sgs_diff_model_coefs
      use merge_dynamic_coefs
      use merge_coefs_whole_dynamic
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_model_coef_4_flux                                  &
     &        (iflag_Csim_marging, layer_tbl, node, ele, iSGS_wk,       &
     &         nod_fld, g_FEM, jac_3d_q, jac_3d_l, numdir, n_int,       &
     &         nlayer_SGS, cor_sgs, cor_sgs_w, sgs_f_coef, sgs_c_coef,  &
     &         sgs_f_whole, sgs_c_whole, wk_lsq)
!
      use t_group_data
      use int_vol_4_model_coef
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(dynamic_SGS_work_address), intent(in) :: iSGS_wk
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind = kint), intent(in) :: iflag_Csim_marging
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: n_int
      integer (kind = kint), intent(in) :: nlayer_SGS
      real(kind = kreal), intent(in) :: cor_sgs(nlayer_SGS,numdir)
      real(kind = kreal), intent(in) :: cor_sgs_w(numdir)
!
      real(kind=kreal), intent(inout) :: sgs_f_coef(nlayer_SGS)
      real(kind=kreal), intent(inout) :: sgs_c_coef(nlayer_SGS,numdir)
      real(kind=kreal), intent(inout) :: sgs_f_whole
      real(kind=kreal), intent(inout) :: sgs_c_whole(numdir)
!
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
!
!
!  Volume integration:                      int_vol_model_coef
      call int_vol_model_coef(layer_tbl, node, ele, iSGS_wk,            &
     &    nod_fld, g_FEM, jac_3d_q, jac_3d_l, numdir, n_int, wk_lsq)
!
!    model coefficients for each components: sum_lsq_coefs_4_comps
      call sum_lsq_coefs_4_comps(ncomp_lsq, wk_lsq)
!
      call merge_coefs_4_dynamic                                        &
     &   (iflag_Csim_marging, numdir, layer_tbl%e_grp%num_grp,          &
     &    cor_sgs, wk_lsq%slsq, wk_lsq%dnorm, sgs_c_coef, sgs_f_coef)
!
      call sum_lsq_whole_coefs(ncomp_lsq, wk_lsq)
!
      call s_merge_coefs_w_dynamic                                      &
     &   (iflag_Csim_marging, numdir, cor_sgs_w, wk_lsq%wlsq,  &
     &    sgs_c_whole, sgs_f_whole)
!
      end subroutine cal_model_coef_4_flux
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_lsq_diff_coef                                      &
     &         (iflag_Csim_marging, iele_fsmp_stack, node, ele,         &
     &          iSGS_wk, nod_fld, g_FEM, jac_3d_q, jac_3d_l,            &
     &          numdir, n_int, cor_diff_w,                              &
     &          diff_f_whole, diff_c_whole, wk_lsq)
!
      use int_vol_4_model_coef
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(dynamic_SGS_work_address), intent(in) :: iSGS_wk
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind = kint), intent(in) :: iflag_Csim_marging
      integer(kind=kint), intent(in) :: numdir
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: cor_diff_w(numdir)
!
      real(kind = kreal), intent(inout) :: diff_f_whole
      real(kind = kreal), intent(inout) :: diff_c_whole(numdir)
!
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
!
!
!  Volume integration: int_vol_diff_coef
      call int_vol_diff_coef(iele_fsmp_stack, node, ele, iSGS_wk,       &
     &    nod_fld, g_FEM, jac_3d_q, jac_3d_l, numdir, n_int, wk_lsq)
!
      call sum_lsq_whole_coefs(ncomp_lsq, wk_lsq)
!
      call s_merge_coefs_w_dynamic(iflag_Csim_marging, numdir,          &
     &    cor_diff_w, wk_lsq%wlsq, diff_c_whole, diff_f_whole)
!
      end subroutine cal_lsq_diff_coef
!
!-----------------------------------------------------------------------
!
      end module cal_lsq_model_coefs
