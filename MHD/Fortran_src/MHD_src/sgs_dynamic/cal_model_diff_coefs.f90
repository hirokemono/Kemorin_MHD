!>@file   cal_model_diff_coefs.f90
!!        module cal_model_diff_coefs
!!
!!>@author H. Matsui
!>@date Programmed in Aug., 2007
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine cal_model_coefs(SGS_par, layer_tbl, node, ele,       &
!!     &          iphys_SGS_wk, nod_fld, jacs, itype_csim, n_int,       &
!!     &          wk_cor, wk_lsq, wk_sgs, Csim)
!!
!!      subroutine cal_diff_coef                                        &
!!     &         (iflag_SGS_initial, SGS_param, cmt_param,              &
!!     &          layer_tbl, node, ele, iphys_SGS_wk, nod_fld, jacs,    &
!!      &         numdir, n_int, wk_cor, wk_lsq, wk_diff, Cdiff)
!!      subroutine cal_diff_coef_fluid                                  &
!!     &         (iflag_SGS_initial, SGS_param, cmt_param, layer_tbl,   &
!!     &          node, ele, fluid, iphys_SGS_wk, nod_fld, jacs,        &
!!     &          numdir, n_int, wk_cor, wk_lsq, wk_diff, Cdiff)
!!      subroutine cal_diff_coef_conduct                                &
!!     &         (iflag_SGS_initial, SGS_param, cmt_param, layer_tbl,   &
!!     &          node, ele, conduct, iphys_SGS_wk, nod_fld, jacs,      &
!!     &          numdir, n_int, wk_cor, wk_lsq, wk_diff, Cdiff)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        integer(kind = kint), intent(in) :: iflag_SGS_initial
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(dynamic_correlation_data), intent(inout) :: wk_cor
!!        type(dynamic_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(SGS_model_coefficient), intent(inout) :: Csim
!!        type(SGS_model_coefficient), intent(inout) :: Cdiff
!!@endverbatim
!
!
      module cal_model_diff_coefs
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_geometry_data_MHD
      use t_geometry_data
      use t_SGS_model_coef_labels
      use t_phys_data
      use t_layering_ele_list
      use t_jacobians
      use t_material_property
      use t_FEM_SGS_model_coefs
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
!
      implicit none
!
      private :: cal_layerd_diff_coef, cal_whole_diff_coef
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_model_coefs(SGS_par, layer_tbl, node, ele,         &
     &          iphys_SGS_wk, nod_fld, jacs, itype_csim, n_int,         &
     &          wk_cor, wk_lsq, wk_sgs, Csim)
!
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
      integer (kind = kint), intent(in) :: itype_csim
      integer (kind = kint), intent(in) :: n_int
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_model_coefficient), intent(inout) :: Csim
!
!
      call cal_ave_rms_sgs_dynamic                                      &
     &   (layer_tbl, node, ele, iphys_SGS_wk, nod_fld,                  &
     &    jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l,                       &
     &    Csim%num_comp, Csim%icomp_Csim, n_int,                        &
     &    wk_sgs%nlayer, wk_sgs%ntot_comp,                              &
     &    wk_sgs%ave_simi, wk_sgs%ave_grad, wk_sgs%rms_simi,            &
     &    wk_sgs%rms_grad, wk_sgs%ratio,                                &
     &    wk_sgs%ave_simi_w, wk_sgs%ave_grad_w, wk_sgs%rms_simi_w,      &
     &    wk_sgs%rms_grad_w, wk_sgs%ratio_w, wk_cor)
!
      call cal_correlate_sgs_dynamic                                    &
     &   (layer_tbl, node, ele, iphys_SGS_wk, nod_fld,                  &
     &    jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l,                       &
     &    Csim%num_comp, Csim%icomp_Csim, n_int,                        &
     &    wk_sgs%nlayer, wk_sgs%ntot_comp, wk_sgs%ave_simi,             &
     &    wk_sgs%ave_grad, wk_sgs%corrilate, wk_sgs%covariant,          &
     &    wk_sgs%corrilate_w, wk_sgs%covariant_w, wk_cor)
!
      call cal_model_coef_4_flux(SGS_par%model_p%iflag_Csim_marging,    &
     &    layer_tbl, node, ele, iphys_SGS_wk, nod_fld,                  &
     &    jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l,                       &
     &    Csim%num_comp, Csim%iak_Csim, Csim%icomp_Csim, n_int,         &
     &    wk_sgs%nlayer, wk_sgs%num_kinds, wk_sgs%ntot_comp,            &
     &    wk_sgs%corrilate, wk_sgs%corrilate_w, wk_sgs%fld_coef,        &
     &    wk_sgs%comp_coef, wk_sgs%fld_whole, wk_sgs%comp_whole,        &
     &    wk_lsq)
!
      call clippging_sgs_diff_coefs                                     &
     &   (SGS_par%iflag_SGS_initial, SGS_par%model_p,                   &
     &    Csim%num_comp, Csim%iak_Csim, Csim%icomp_Csim, wk_sgs)
!
      call sel_model_coefs_2_ele(ele, layer_tbl%e_grp, itype_csim,      &
     &                           wk_sgs, Csim)
!
      end subroutine cal_model_coefs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_diff_coef                                          &
     &         (iflag_SGS_initial, SGS_param, cmt_param,                &
     &          layer_tbl, node, ele, iphys_SGS_wk, nod_fld, jacs,      &
     &          numdir, n_int, wk_cor, wk_lsq, wk_diff, Cdiff)
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) ::  n_int
!
      integer(kind = kint), intent(in) :: iflag_SGS_initial
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_model_coefficient), intent(inout) :: Cdiff
!
!
      if (cmt_param%iflag_layerd_DIFF_coefs .eq. 1) then
        call cal_layerd_diff_coef(iflag_SGS_initial, SGS_param,         &
     &      layer_tbl, node, ele, iphys_SGS_wk, nod_fld,                &
     &      jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l,                     &
     &      numdir, n_int, wk_cor, wk_lsq, wk_diff, Cdiff)
      else
        call cal_whole_diff_coef                                        &
     &     (layer_tbl, ele%istack_ele_smp, iflag_SGS_initial,           &
     &      SGS_param, node, ele, iphys_SGS_wk, nod_fld,                &
     &      jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l,                     &
     &      numdir, n_int, ele%volume, wk_cor, wk_lsq, wk_diff, Cdiff)
      end if
!
      end subroutine cal_diff_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_coef_fluid                                    &
     &         (iflag_SGS_initial, SGS_param, cmt_param, layer_tbl,     &
     &          node, ele, fluid, iphys_SGS_wk, nod_fld, jacs,          &
     &          numdir, n_int, wk_cor, wk_lsq, wk_diff, Cdiff)
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: iflag_SGS_initial
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_model_coefficient), intent(inout) :: Cdiff
!
!
      if (cmt_param%iflag_layerd_DIFF_coefs .eq. 1) then
        call cal_layerd_diff_coef(iflag_SGS_initial, SGS_param,         &
     &      layer_tbl, node, ele, iphys_SGS_wk, nod_fld,                &
     &      jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l,                     &
     &      numdir, n_int, wk_cor, wk_lsq, wk_diff, Cdiff)
      else
        call cal_whole_diff_coef                                        &
     &     (layer_tbl, fluid%istack_ele_fld_smp, iflag_SGS_initial,     &
     &      SGS_param, node, ele, iphys_SGS_wk, nod_fld,                &
     &      jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l, numdir, n_int,      &
     &      fluid%volume, wk_cor, wk_lsq, wk_diff, Cdiff)
      end if
!
      end subroutine cal_diff_coef_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_coef_conduct                                  &
     &         (iflag_SGS_initial, SGS_param, cmt_param, layer_tbl,     &
     &          node, ele, conduct, iphys_SGS_wk, nod_fld, jacs,        &
     &          numdir, n_int, wk_cor, wk_lsq, wk_diff, Cdiff)
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) ::  n_int
!
      integer(kind = kint), intent(in) :: iflag_SGS_initial
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(layering_tbl), intent(in) :: layer_tbl
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_model_coefficient), intent(inout) :: Cdiff
!
!
      if (cmt_param%iflag_layerd_DIFF_coefs .eq. 1) then
        call cal_layerd_diff_coef(iflag_SGS_initial, SGS_param,         &
     &      layer_tbl, node, ele, iphys_SGS_wk, nod_fld,                &
     &      jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l,                     &
     &      numdir, n_int, wk_cor, wk_lsq, wk_diff, Cdiff)
      else
        call cal_whole_diff_coef                                        &
     &     (layer_tbl, conduct%istack_ele_fld_smp, iflag_SGS_initial,   &
     &      SGS_param, node, ele, iphys_SGS_wk, nod_fld,                &
     &      jacs%g_FEM, jacs%jac_3d, jacs%jac_3d_l, numdir, n_int,      &
     &      conduct%volume, wk_cor, wk_lsq, wk_diff, Cdiff)
      end if
!
      end subroutine cal_diff_coef_conduct
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_layerd_diff_coef                                   &
     &         (iflag_SGS_initial, SGS_param, layer_tbl, node, ele,     &
     &          iphys_SGS_wk, nod_fld, g_FEM, jac_3d_q, jac_3d_l,       &
     &          numdir, n_int, wk_cor, wk_lsq, wk_diff, Cdiff)
!
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: iflag_SGS_initial
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_model_coefficient), intent(inout) :: Cdiff
!
!
      call cal_ave_rms_sgs_dynamic(layer_tbl, node, ele, iphys_SGS_wk,  &
     &    nod_fld, g_FEM, jac_3d_q, jac_3d_l, numdir, Cdiff%icomp_Csim, &
     &    n_int, wk_diff%nlayer, wk_diff%ntot_comp,                     &
     &    wk_diff%ave_simi, wk_diff%ave_grad, wk_diff%rms_simi,         &
     &    wk_diff%rms_grad, wk_diff%ratio,                              &
     &    wk_diff%ave_simi_w, wk_diff%ave_grad_w, wk_diff%rms_simi_w,   &
     &    wk_diff%rms_grad_w, wk_diff%ratio_w, wk_cor)
!
      call cal_correlate_sgs_dynamic                                    &
     &   (layer_tbl, node, ele, iphys_SGS_wk, nod_fld,                  &
     &    g_FEM, jac_3d_q, jac_3d_l, numdir, Cdiff%icomp_Csim, n_int,   &
     &    wk_diff%nlayer, wk_diff%ntot_comp, wk_diff%ave_simi,          &
     &    wk_diff%ave_grad, wk_diff%corrilate, wk_diff%covariant,       &
     &    wk_diff%corrilate_w, wk_diff%covariant_w, wk_cor)
!
      call cal_model_coef_4_flux                                        &
    &    (SGS_param%iflag_Csim_marging, layer_tbl,                      &
     &    node, ele, iphys_SGS_wk, nod_fld, g_FEM, jac_3d_q, jac_3d_l,  &
     &    numdir, Cdiff%iak_Csim, Cdiff%icomp_Csim, n_int,              &
     &    wk_diff%nlayer, wk_diff%num_kinds, wk_diff%ntot_comp,         &
     &    wk_diff%corrilate, wk_diff%corrilate_w, wk_diff%fld_coef,     &
     &    wk_diff%comp_coef, wk_diff%fld_whole, wk_diff%comp_whole,     &
     &    wk_lsq)
      call clippging_sgs_diff_coefs(iflag_SGS_initial, SGS_param,       &
     &    numdir, Cdiff%iak_Csim, Cdiff%icomp_Csim, wk_diff)
!
      call set_diff_coefs_layer_ele                                     &
     &   (ele, layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,       &
     &    layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp,     &
     &    wk_diff%fld_clip(1,Cdiff%iak_Csim), Cdiff%coef(1,1))
!
      end subroutine cal_layerd_diff_coef
!
!  ---------------------------------------------------------------------
!
      subroutine cal_whole_diff_coef(layer_tbl, iele_fsmp_stack,        &
     &         iflag_SGS_initial, SGS_param, node, ele,                 &
     &         iphys_SGS_wk, nod_fld, g_FEM, jac_3d_q, jac_3d_l,        &
     &         numdir, n_int, volume_d, wk_cor, wk_lsq, wk_diff, Cdiff)
!
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: n_int
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: volume_d
!
      integer(kind = kint), intent(in) :: iflag_SGS_initial
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_model_coefficient), intent(inout) :: Cdiff
!
!
      call cal_ave_rms_diff_area(iele_fsmp_stack,                       &
     &    node, ele, iphys_SGS_wk, nod_fld, g_FEM, jac_3d_q, jac_3d_l,  &
     &    numdir, Cdiff%icomp_Csim, n_int, volume_d, wk_diff%ntot_comp, &
     &    wk_diff%ave_simi_w, wk_diff%ave_grad_w, wk_diff%rms_simi_w,   &
     &    wk_diff%rms_grad_w, wk_diff%ratio_w, wk_cor)
!
      call cal_correlate_diff_area(layer_tbl, iele_fsmp_stack,          &
     &    node, ele, iphys_SGS_wk, nod_fld, g_FEM, jac_3d_q, jac_3d_l,  &
     &    numdir, Cdiff%icomp_Csim, n_int, wk_diff%ntot_comp,           &
     &    wk_diff%ave_simi_w, wk_diff%ave_grad_w,                       &
     &    wk_diff%corrilate_w, wk_diff%covariant_w, wk_cor)
!
      call cal_lsq_diff_coef                                            &
     &   (SGS_param%iflag_Csim_marging, iele_fsmp_stack,                &
     &    node, ele, iphys_SGS_wk, nod_fld, g_FEM, jac_3d_q, jac_3d_l,  &
     &    numdir, Cdiff%iak_Csim, Cdiff%icomp_Csim, n_int,              &
     &    wk_diff%num_kinds, wk_diff%ntot_comp, wk_diff%corrilate_w,    &
     &    wk_diff%fld_whole, wk_diff%comp_whole, wk_lsq)
      call clippging_sgs_diff_coefs(iflag_SGS_initial, SGS_param,       &
     &    numdir, Cdiff%iak_Csim, Cdiff%icomp_Csim, wk_diff)
      call set_diff_coefs_whole_ele(ele, iele_fsmp_stack,               &
     &    wk_diff%fld_whole_clip(Cdiff%iak_Csim), Cdiff%coef(1,1))
!
      end subroutine cal_whole_diff_coef
!
!-----------------------------------------------------------------------
!
      end module cal_model_diff_coefs
