!cal_ave_rms_4_dynamic.f90
!      module cal_ave_rms_4_dynamic
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine cal_ave_rms_sgs_dynamic(layer_tbl,                   &
!!     &         node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,         &
!!     &         numdir, icomp_f, n_int, nlayer_SGS, num_sgs_coefs,     &
!!     &         ave_sgs_simi, ave_sgs_grad, rms_sgs_simi, rms_sgs_grad,&
!!     &         ratio_sgs, ave_sgs_simi_w, ave_sgs_grad_w,             &
!!     &         rms_sgs_simi_w, rms_sgs_grad_w, ratio_sgs_w, wk_cor)
!!
!!      subroutine cal_ave_rms_diff_area(iele_fsmp_stack,               &
!!     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,        &
!!     &          numdir, icomp_f, n_int, volume_d, num_diff_coefs,     &
!!     &          ave_diff_simi_w, ave_diff_grad_w,                     &
!!     &          rms_diff_simi_w, rms_diff_grad_w, ratio_diff_w, wk_cor)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(dynamic_correlation_data), intent(inout) :: wk_cor
!
      module cal_ave_rms_4_dynamic
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_layering_ele_list
      use t_jacobians
      use t_work_layer_correlate
!
      use cal_layerd_ave_correlate
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_ave_rms_sgs_dynamic(layer_tbl,                     &
     &         node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,           &
     &         numdir, icomp_f, n_int, nlayer_SGS, num_sgs_coefs,       &
     &         ave_sgs_simi, ave_sgs_grad, rms_sgs_simi, rms_sgs_grad,  &
     &         ratio_sgs, ave_sgs_simi_w, ave_sgs_grad_w,               &
     &         rms_sgs_simi_w, rms_sgs_grad_w, ratio_sgs_w, wk_cor)
!
      use int_vol_4_model_coef
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind = kint), intent(in) :: nlayer_SGS, num_sgs_coefs
      integer (kind = kint), intent(in) :: n_int, numdir, icomp_f
!
      real(kind = kreal), intent(inout)                                 &
     &          :: ave_sgs_simi(nlayer_SGS,num_sgs_coefs)
      real(kind = kreal), intent(inout)                                 &
     &          :: ave_sgs_grad(nlayer_SGS,num_sgs_coefs)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sgs_simi(nlayer_SGS,num_sgs_coefs)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_sgs_grad(nlayer_SGS,num_sgs_coefs)
      real(kind = kreal), intent(inout)                                 &
     &          :: ratio_sgs(nlayer_SGS,num_sgs_coefs)
!
      real(kind = kreal), intent(inout) :: ave_sgs_simi_w(num_sgs_coefs)
      real(kind = kreal), intent(inout) :: ave_sgs_grad_w(num_sgs_coefs)
      real(kind = kreal), intent(inout) :: rms_sgs_simi_w(num_sgs_coefs)
      real(kind = kreal), intent(inout) :: rms_sgs_grad_w(num_sgs_coefs)
      real(kind = kreal), intent(inout) :: ratio_sgs_w(num_sgs_coefs)
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
!
!
      call int_vol_rms_ave_dynamic(layer_tbl, node, ele,                &
     &    iphys, nod_fld, jac_3d_q, jac_3d_l, numdir, n_int, wk_cor)
!
      call sum_layerd_averages(layer_tbl%e_grp%num_grp, wk_cor)
      call divide_layers_ave_by_vol                                     &
     &   (wk_cor%nlayer, wk_cor%ncomp_sgl, wk_cor%ncomp_dble, numdir,   &
     &    layer_tbl%a_vol_layer, wk_cor%ave_les, wk_cor%rms_les,        &
     &    ave_sgs_simi(1,icomp_f), ave_sgs_grad(1,icomp_f),             &
     &    rms_sgs_simi(1,icomp_f), rms_sgs_grad(1,icomp_f),             &
     &    ratio_sgs(1,icomp_f) )
!
      call sum_whole_averages(wk_cor)
      call divide_all_layer_ave_by_vol                                  &
     &   (wk_cor%ncomp_sgl, wk_cor%ncomp_dble, numdir,                  &
     &    layer_tbl%vol_total_layer(1), wk_cor%ave_wg, wk_cor%rms_wg,   &
     &    ave_sgs_simi_w(icomp_f), ave_sgs_grad_w(icomp_f),             &
     &    rms_sgs_simi_w(icomp_f), rms_sgs_grad_w(icomp_f),             &
     &    ratio_sgs_w(icomp_f) )
!
      end subroutine cal_ave_rms_sgs_dynamic
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_ave_rms_diff_area(iele_fsmp_stack,                 &
     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,          &
     &          numdir, icomp_f, n_int, volume_d, num_diff_coefs,       &
     &          ave_diff_simi_w, ave_diff_grad_w,                       &
     &          rms_diff_simi_w, rms_diff_grad_w, ratio_diff_w, wk_cor)
!
      use int_vol_4_model_coef
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: num_diff_coefs
      integer (kind = kint), intent(in) :: n_int, icomp_f
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: volume_d
!
      real(kind = kreal), intent(inout)                                 &
     &          :: ave_diff_simi_w(num_diff_coefs)
      real(kind = kreal), intent(inout)                                 &
     &          :: ave_diff_grad_w(num_diff_coefs)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_diff_simi_w(num_diff_coefs)
      real(kind = kreal), intent(inout)                                 &
     &          :: rms_diff_grad_w(num_diff_coefs)
      real(kind = kreal), intent(inout)                                 &
     &          :: ratio_diff_w(num_diff_coefs)
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
!
!
      call int_vol_rms_ave_diff(iele_fsmp_stack,                        &
     &    node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,                &
     &    numdir, n_int, wk_cor)
!
      call sum_whole_averages(wk_cor)
      call divide_all_layer_ave_by_vol                                  &
     &   (wk_cor%ncomp_sgl, wk_cor%ncomp_dble, numdir,                  &
     &    volume_d, wk_cor%ave_wg, wk_cor%rms_wg,                       &
     &    ave_diff_simi_w(icomp_f), ave_diff_grad_w(icomp_f),           &
     &    rms_diff_simi_w(icomp_f), rms_diff_grad_w(icomp_f),           &
     &    ratio_diff_w(icomp_f) )
!
      end subroutine cal_ave_rms_diff_area
!
!  ---------------------------------------------------------------------
!
      end module cal_ave_rms_4_dynamic
