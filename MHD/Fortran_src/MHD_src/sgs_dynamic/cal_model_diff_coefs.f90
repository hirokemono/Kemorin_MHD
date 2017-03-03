!
!      module cal_model_diff_coefs
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine cal_model_coefs(SGS_par, layer_tbl,                  &
!!     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,        &
!!     &          itype_csim, n_tensor, ifield_d, icomp_f, n_int,       &
!!     &          wk_cor, wk_lsq, wk_sgs, sgs_coefs)
!!
!!      subroutine cal_diff_coef                                        &
!!     &         (SGS_par, layer_tbl, node, ele,  iphys, nod_fld,       &
!!     &          jac_3d_q, jac_3d_l, numdir, ifield_d, icomp_f, n_int, &
!!     &          wk_cor, wk_lsq, wk_diff, diff_coefs)
!!      subroutine cal_diff_coef_fluid(SGS_par, layer_tbl, node, ele,   &
!!     &          fluid, iphys, nod_fld, jac_3d_q, jac_3d_l,            &
!!     &          numdir, ifield_d, icomp_f, n_int,                     &
!!     &          wk_cor, wk_lsq, wk_diff, diff_coefs)
!!      subroutine cal_diff_coef_conduct(SGS_par, layer_tbl, node, ele, &
!!     &          conduct, iphys, nod_fld, jac_3d_q, jac_3d_l,          &
!!     &          numdir, ifield_d, icomp_f, n_int,                     &
!!     &          wk_cor, wk_lsq, wk_diff, diff_coefs)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(dynamis_correlation_data), intent(inout) :: wk_cor
!!        type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
      module cal_model_diff_coefs
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_layering_ele_list
      use t_jacobians
      use t_material_property
      use t_SGS_model_coefs
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
      subroutine cal_model_coefs(SGS_par, layer_tbl,                    &
     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,          &
     &          itype_csim, n_tensor, ifield_d, icomp_f, n_int,         &
     &          wk_cor, wk_lsq, wk_sgs, sgs_coefs)
!
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
      integer (kind = kint), intent(in) :: itype_csim, n_tensor
      integer (kind = kint), intent(in) :: n_int, ifield_d, icomp_f
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
!
      call cal_ave_rms_sgs_dynamic(layer_tbl,                           &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    n_tensor, icomp_f, n_int, wk_sgs%nlayer, wk_sgs%ntot_comp,    &
     &    wk_sgs%ave_simi, wk_sgs%ave_grad, wk_sgs%rms_simi,            &
     &    wk_sgs%rms_grad, wk_sgs%ratio,                                &
     &    wk_sgs%ave_simi_w, wk_sgs%ave_grad_w, wk_sgs%rms_simi_w,      &
     &    wk_sgs%rms_grad_w, wk_sgs%ratio_w, wk_cor)
!
      call cal_correlate_sgs_dynamic(layer_tbl, node, ele,              &
     &    iphys, nod_fld, jac_3d_q, jac_3d_l, n_tensor, icomp_f, n_int, &
     &    wk_sgs%nlayer, wk_sgs%ntot_comp, wk_sgs%ave_simi,             &
     &    wk_sgs%ave_grad, wk_sgs%corrilate, wk_sgs%covariant,          &
     &    wk_sgs%corrilate_w, wk_sgs%covariant_w, wk_cor)
!
      call cal_model_coef_4_flux                                        &
     &   (SGS_par%model_p%iflag_Csim_marging, layer_tbl,                &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    n_tensor, ifield_d, icomp_f, n_int,                           &
     &    wk_sgs%nlayer, wk_sgs%num_kinds, wk_sgs%ntot_comp,            &
     &    wk_sgs%corrilate, wk_sgs%corrilate_w, wk_sgs%fld_coef,        &
     &    wk_sgs%comp_coef, wk_sgs%fld_whole, wk_sgs%comp_whole,        &
     &    wk_lsq)
!
      call clippging_sgs_diff_coefs                                     &
     &   (n_tensor, ifield_d, icomp_f, SGS_par, wk_sgs)
!
      call clear_model_coefs_2_ele(ele, n_tensor, icomp_f,              &
     &    sgs_coefs%ntot_comp, sgs_coefs%ak)
      call set_model_coefs_2_ele                                        &
     &   (ele, itype_csim, n_tensor, ifield_d, icomp_f,                 &
     &    layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,            &
     &    layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp,     &
     &    sgs_coefs%num_field, sgs_coefs%ntot_comp,                     &
     &    wk_sgs%fld_clip, wk_sgs%comp_clip, sgs_coefs%ak)
!
      end subroutine cal_model_coefs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_diff_coef                                          &
     &         (SGS_par, layer_tbl, node, ele,  iphys, nod_fld,         &
     &          jac_3d_q, jac_3d_l, numdir, ifield_d, icomp_f, n_int,   &
     &          wk_cor, wk_lsq, wk_diff, diff_coefs)
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
      if (SGS_par%commute_p%iset_DIFF_coefs .eq. 1) then
        call cal_layerd_diff_coef(SGS_par, layer_tbl,                   &
     &      node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &      numdir, ifield_d, icomp_f, n_int,                           &
     &      wk_cor, wk_lsq, wk_diff, diff_coefs)
      else
        call cal_whole_diff_coef(layer_tbl, ele%istack_ele_smp,         &
     &      SGS_par, node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,     &
     &      numdir, ifield_d, icomp_f, n_int, ele%volume,               &
     &      wk_cor, wk_lsq, wk_diff, diff_coefs)
      end if
!
      end subroutine cal_diff_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_coef_fluid(SGS_par, layer_tbl, node, ele,     &
     &          fluid, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &          numdir, ifield_d, icomp_f, n_int,                       &
     &          wk_cor, wk_lsq, wk_diff, diff_coefs)
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
      if (SGS_par%commute_p%iset_DIFF_coefs .eq. 1) then
        call cal_layerd_diff_coef(SGS_par, layer_tbl,                   &
     &      node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &      numdir, ifield_d, icomp_f, n_int,                           &
     &      wk_cor, wk_lsq, wk_diff, diff_coefs)
      else
        call cal_whole_diff_coef                                        &
     &     (layer_tbl, fluid%istack_ele_fld_smp, SGS_par,               &
     &      node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &      numdir, ifield_d, icomp_f, n_int, fluid%volume,             &
     &      wk_cor, wk_lsq, wk_diff, diff_coefs)
      end if
!
      end subroutine cal_diff_coef_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_coef_conduct(SGS_par, layer_tbl, node, ele,   &
     &          conduct, iphys, nod_fld, jac_3d_q, jac_3d_l,            &
     &          numdir, ifield_d, icomp_f, n_int,                       &
     &          wk_cor, wk_lsq, wk_diff, diff_coefs)
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
      if (SGS_par%commute_p%iset_DIFF_coefs .eq. 1) then
        call cal_layerd_diff_coef(SGS_par, layer_tbl,                   &
     &      node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &      numdir, ifield_d, icomp_f, n_int,                           &
     &      wk_cor, wk_lsq, wk_diff, diff_coefs)
      else
        call cal_whole_diff_coef                                        &
     &     (layer_tbl, conduct%istack_ele_fld_smp, SGS_par,             &
     &      node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &      numdir, ifield_d, icomp_f, n_int, conduct%volume,           &
     &      wk_cor, wk_lsq, wk_diff, diff_coefs)
      end if
!
      end subroutine cal_diff_coef_conduct
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_layerd_diff_coef(SGS_par, layer_tbl,               &
     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,          &
     &          numdir, ifield_d, icomp_f, n_int,                       &
     &          wk_cor, wk_lsq, wk_diff, diff_coefs)
!
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: n_int, ifield_d, icomp_f
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
        type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
      call cal_ave_rms_sgs_dynamic(layer_tbl,                           &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    numdir, icomp_f, n_int, wk_diff%nlayer, wk_diff%ntot_comp,    &
     &    wk_diff%ave_simi, wk_diff%ave_grad, wk_diff%rms_simi,         &
     &    wk_diff%rms_grad, wk_diff%ratio,                              &
     &    wk_diff%ave_simi_w, wk_diff%ave_grad_w, wk_diff%rms_simi_w,   &
     &    wk_diff%rms_grad_w, wk_diff%ratio_w, wk_cor)
!
      call cal_correlate_sgs_dynamic(layer_tbl, node, ele,              &
     &    iphys, nod_fld, jac_3d_q, jac_3d_l, numdir, icomp_f, n_int,   &
     &    wk_diff%nlayer, wk_diff%ntot_comp, wk_diff%ave_simi,          &
     &    wk_diff%ave_grad, wk_diff%corrilate, wk_diff%covariant,       &
     &    wk_diff%corrilate_w, wk_diff%covariant_w, wk_cor)
!
      call cal_model_coef_4_flux                                        &
    &    (SGS_par%model_p%iflag_Csim_marging, layer_tbl,                &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    numdir, ifield_d, icomp_f, n_int,                             &
     &    wk_diff%nlayer, wk_diff%num_kinds, wk_diff%ntot_comp,         &
     &    wk_diff%corrilate, wk_diff%corrilate_w, wk_diff%fld_coef,     &
     &    wk_diff%comp_coef, wk_diff%fld_whole, wk_diff%comp_whole,     &
     &    wk_lsq)
      call clippging_sgs_diff_coefs                                     &
     &   (numdir, ifield_d, icomp_f, SGS_par, wk_diff)
!
      call set_diff_coefs_layer_ele(ele, ifield_d,                      &
     &    layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,            &
     &    layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp,     &
     &    diff_coefs%ntot_comp, wk_diff%fld_clip, diff_coefs%ak)
!
      end subroutine cal_layerd_diff_coef
!
!  ---------------------------------------------------------------------
!
      subroutine cal_whole_diff_coef                                    &
     &         (layer_tbl, iele_fsmp_stack, SGS_par,                    &
     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,          &
     &          numdir, ifield_d, icomp_f, n_int, volume_d,             &
     &          wk_cor, wk_lsq, wk_diff, diff_coefs)
!
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: n_int, ifield_d, icomp_f
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: volume_d
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
      call cal_ave_rms_diff_area(iele_fsmp_stack,                       &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    numdir, icomp_f, n_int, volume_d, wk_diff%ntot_comp,          &
     &    wk_diff%ave_simi_w, wk_diff%ave_grad_w, wk_diff%rms_simi_w,   &
     &    wk_diff%rms_grad_w, wk_diff%ratio_w, wk_cor)
!
      call cal_correlate_diff_area(layer_tbl, iele_fsmp_stack,          &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    numdir, icomp_f, n_int, wk_diff%ntot_comp,                    &
     &    wk_diff%ave_simi_w, wk_diff%ave_grad_w,                       &
     &    wk_diff%corrilate_w, wk_diff%covariant_w, wk_cor)
!
      call cal_lsq_diff_coef                                            &
     &   (SGS_par%model_p%iflag_Csim_marging, iele_fsmp_stack,          &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    numdir, ifield_d, icomp_f, n_int,                             &
     &    wk_diff%num_kinds, wk_diff%ntot_comp, wk_diff%corrilate_w,    &
     &    wk_diff%fld_whole, wk_diff%comp_whole, wk_lsq)
      call clippging_sgs_diff_coefs                                     &
     &   (numdir, ifield_d, icomp_f, SGS_par, wk_diff)
      call set_diff_coefs_whole_ele(ele, iele_fsmp_stack, ifield_d,     &
     &    diff_coefs%ntot_comp, wk_diff%fld_whole_clip, diff_coefs%ak)
!
!
      end subroutine cal_whole_diff_coef
!
!-----------------------------------------------------------------------
!
      end module cal_model_diff_coefs
