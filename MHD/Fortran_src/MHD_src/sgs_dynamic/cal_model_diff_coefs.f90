!
!      module cal_model_diff_coefs
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine cal_model_coefs(layer_tbl,                           &
!!     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,        &
!!     &          itype_csim, n_tensor, ifield_d, icomp_f, n_int,       &
!!     &          sgs_coefs)
!!
!!      subroutine cal_diff_coef(layer_tbl,                             &
!!     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,        &
!!     &          numdir, ifield_d, icomp_f, n_int, diff_coefs)
!!      subroutine cal_diff_coef_fluid(layer_tbl, node, ele, fluid,     &
!!     &          iphys, nod_fld, jac_3d_q, jac_3d_l,                   &
!!     &          numdir, ifield_d, icomp_f, n_int, diff_coefs)
!!      subroutine cal_diff_coef_conduct(layer_tbl, node, ele, conduct, &
!!     &          iphys, nod_fld, jac_3d_q, jac_3d_l,                   &
!!     &          numdir, ifield_d, icomp_f, n_int, diff_coefs)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(MHD_coefficients_type), intent(inout) :: sgs_coefs
!!        type(MHD_coefficients_type), intent(inout) :: diff_coefs
!
!
      module cal_model_diff_coefs
!
      use m_precision
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_layering_ele_list
      use t_jacobians
      use t_material_property
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
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
      subroutine cal_model_coefs(layer_tbl,                             &
     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,          &
     &          itype_csim, n_tensor, ifield_d, icomp_f, n_int,         &
     &          sgs_coefs)
!
      use m_work_4_dynamic_model
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
      integer (kind = kint), intent(in) :: itype_csim, n_tensor
      integer (kind = kint), intent(in) :: n_int, ifield_d, icomp_f
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
!      type(dynamis_least_suare_data), intent(inout) :: wk_lsq1
!      type(dynamic_model_data), intent(inout) :: wk_sgs1
      type(MHD_coefficients_type), intent(inout) :: sgs_coefs
!
!
      call cal_ave_rms_sgs_dynamic(layer_tbl,                           &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    n_tensor, icomp_f, n_int, wk_sgs1%nlayer, wk_sgs1%ntot_comp,  &
     &    wk_sgs1%ave_simi, wk_sgs1%ave_grad, wk_sgs1%rms_simi,         &
     &    wk_sgs1%rms_grad, wk_sgs1%ratio,                              &
     &    wk_sgs1%ave_simi_w, wk_sgs1%ave_grad_w,                       &
     &    wk_sgs1%rms_simi_w, wk_sgs1%rms_grad_w, wk_sgs1%ratio_w)
!
      call cal_correlate_sgs_dynamic(layer_tbl, node, ele,              &
     &    iphys, nod_fld, jac_3d_q, jac_3d_l, n_tensor, icomp_f, n_int, &
     &    wk_sgs1%nlayer, wk_sgs1%ntot_comp, wk_sgs1%ave_simi,          &
     &    wk_sgs1%ave_grad, wk_sgs1%corrilate, wk_sgs1%covariant,       &
     &    wk_sgs1%corrilate_w, wk_sgs1%covariant_w)
!
      call cal_model_coef_4_flux(layer_tbl,                             &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    n_tensor, ifield_d, icomp_f, n_int,                           &
     &    wk_sgs1%nlayer, wk_sgs1%num_kinds, wk_sgs1%ntot_comp,         &
     &    wk_sgs1%corrilate, wk_sgs1%corrilate_w, wk_sgs1%fld_coef,     &
     &    wk_sgs1%comp_coef, wk_sgs1%fld_whole, wk_sgs1%comp_whole,     &
     &    wk_lsq1)
!
      call clippging_sgs_diff_coefs                                     &
     &   (n_tensor, ifield_d, icomp_f, wk_sgs1)
!
      call clear_model_coefs_2_ele(ele, n_tensor, icomp_f,              &
     &    sgs_coefs%ntot_comp, sgs_coefs%ak)
      call set_model_coefs_2_ele                                        &
     &   (ele, itype_csim, n_tensor, ifield_d, icomp_f,                 &
     &    layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,            &
     &    layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp,     &
     &    sgs_coefs%num_field, sgs_coefs%ntot_comp,                     &
     &    wk_sgs1%fld_clip, wk_sgs1%comp_clip, sgs_coefs%ak)
!
      end subroutine cal_model_coefs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_diff_coef(layer_tbl,                               &
     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,          &
     &          numdir, ifield_d, icomp_f, n_int, diff_coefs)
!
      use m_control_parameter
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(MHD_coefficients_type), intent(inout) :: diff_coefs
!
!
      if (iset_DIFF_model_coefs .eq. 1) then
        call cal_layerd_diff_coef(layer_tbl,                            &
     &      node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &      numdir, ifield_d, icomp_f, n_int, diff_coefs)
      else
        call cal_whole_diff_coef(layer_tbl, ele%istack_ele_smp,         &
     &      node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &      numdir, ifield_d, icomp_f, n_int, ele%volume, diff_coefs)
      end if
!
      end subroutine cal_diff_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_coef_fluid(layer_tbl, node, ele, fluid,       &
     &          iphys, nod_fld, jac_3d_q, jac_3d_l,                     &
     &          numdir, ifield_d, icomp_f, n_int, diff_coefs)
!
      use m_control_parameter
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(MHD_coefficients_type), intent(inout) :: diff_coefs
!
!
      if (iset_DIFF_model_coefs .eq. 1) then
        call cal_layerd_diff_coef(layer_tbl,                            &
     &      node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &      numdir, ifield_d, icomp_f, n_int, diff_coefs)
      else
        call cal_whole_diff_coef(layer_tbl, fluid%istack_ele_fld_smp,   &
     &      node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &      numdir, ifield_d, icomp_f, n_int, fluid%volume, diff_coefs)
      end if
!
      end subroutine cal_diff_coef_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_coef_conduct(layer_tbl, node, ele, conduct,   &
     &          iphys, nod_fld, jac_3d_q, jac_3d_l,                     &
     &          numdir, ifield_d, icomp_f, n_int, diff_coefs)
!
      use m_control_parameter
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(MHD_coefficients_type), intent(inout) :: diff_coefs
!
!
      if (iset_DIFF_model_coefs .eq. 1) then
        call cal_layerd_diff_coef(layer_tbl,                            &
     &      node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &      numdir, ifield_d, icomp_f, n_int, diff_coefs)
      else
        call cal_whole_diff_coef                                        &
     &     (layer_tbl, conduct%istack_ele_fld_smp,                      &
     &      node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,              &
     &      numdir, ifield_d, icomp_f, n_int, conduct%volume,           &
     &      diff_coefs)
      end if
!
      end subroutine cal_diff_coef_conduct
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_layerd_diff_coef(layer_tbl,                        &
     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,          &
     &          numdir, ifield_d, icomp_f, n_int, diff_coefs)
!
      use m_work_4_dynamic_model
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: n_int, ifield_d, icomp_f
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(MHD_coefficients_type), intent(inout) :: diff_coefs
!
!
      call cal_ave_rms_sgs_dynamic(layer_tbl,                           &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    numdir, icomp_f, n_int, wk_diff1%nlayer, wk_diff1%ntot_comp,  &
     &    wk_diff1%ave_simi, wk_diff1%ave_grad, wk_diff1%rms_simi,      &
     &    wk_diff1%rms_grad, wk_diff1%ratio,                            &
     &    wk_diff1%ave_simi_w, wk_diff1%ave_grad_w,                     &
     &    wk_diff1%rms_simi_w, wk_diff1%rms_grad_w, wk_diff1%ratio_w)
!
      call cal_correlate_sgs_dynamic(layer_tbl, node, ele,              &
     &    iphys, nod_fld, jac_3d_q, jac_3d_l, numdir, icomp_f, n_int,   &
     &    wk_diff1%nlayer, wk_diff1%ntot_comp, wk_diff1%ave_simi,       &
     &    wk_diff1%ave_grad, wk_diff1%corrilate, wk_diff1%covariant,    &
     &    wk_diff1%corrilate_w, wk_diff1%covariant_w)
!
      call cal_model_coef_4_flux(layer_tbl,                             &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    numdir, ifield_d, icomp_f, n_int,                             &
     &    wk_diff1%nlayer, wk_diff1%num_kinds, wk_diff1%ntot_comp,      &
     &    wk_diff1%corrilate, wk_diff1%corrilate_w, wk_diff1%fld_coef,  &
     &    wk_diff1%comp_coef, wk_diff1%fld_whole, wk_diff1%comp_whole,  &
     &    wk_lsq1)
      call clippging_sgs_diff_coefs                                     &
     &   (numdir, ifield_d, icomp_f, wk_diff1)
!
      call set_diff_coefs_layer_ele(ele, ifield_d,                      &
     &    layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,            &
     &    layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp,     &
     &    diff_coefs%ntot_comp, wk_diff1%fld_clip, diff_coefs%ak)
!
      end subroutine cal_layerd_diff_coef
!
!  ---------------------------------------------------------------------
!
      subroutine cal_whole_diff_coef(layer_tbl, iele_fsmp_stack,        &
     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,          &
     &          numdir, ifield_d, icomp_f, n_int, volume_d, diff_coefs)
!
      use m_work_4_dynamic_model
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
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(MHD_coefficients_type), intent(inout) :: diff_coefs
!
!
      call cal_ave_rms_diff_area(iele_fsmp_stack,                       &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    numdir, icomp_f, n_int, volume_d, wk_diff1%ntot_comp,         &
     &    wk_diff1%ave_simi_w, wk_diff1%ave_grad_w,                     &
     &    wk_diff1%rms_simi_w, wk_diff1%rms_grad_w, wk_diff1%ratio_w)
!
      call cal_correlate_diff_area(layer_tbl, iele_fsmp_stack,          &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    numdir, icomp_f, n_int, wk_diff1%ntot_comp,                   &
     &    wk_diff1%ave_simi_w, wk_diff1%ave_grad_w,                     &
     &    wk_diff1%corrilate_w, wk_diff1%covariant_w)
!
      call cal_lsq_diff_coef(iele_fsmp_stack,                           &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    numdir, ifield_d, icomp_f, n_int,                             &
     &    wk_diff1%num_kinds, wk_diff1%ntot_comp, wk_diff1%corrilate_w, &
     &    wk_diff1%fld_whole, wk_diff1%comp_whole, wk_lsq1)
      call clippging_sgs_diff_coefs                                     &
     &   (numdir, ifield_d, icomp_f, wk_diff1)
      call set_diff_coefs_whole_ele(ele, iele_fsmp_stack, ifield_d,     &
     &    diff_coefs%ntot_comp, wk_diff1%fld_whole_clip, diff_coefs%ak)
!
!
      end subroutine cal_whole_diff_coef
!
!-----------------------------------------------------------------------
!
      end module cal_model_diff_coefs
