!cal_correlate_4_dynamic.f90
!      module cal_correlate_4_dynamic
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine cal_correlate_sgs_dynamic                            &
!!     &         (layer_tbl, node, ele, iphys, nod_fld,                 &
!!     &          jac_3d_q, jac_3d_l, n_tensor, icomp_f, n_int,         &
!!     &          nlayer_SGS, num_sgs_coefs, ave_sgs_simi, ave_sgs_grad,&
!!     &          cor_sgs, cov_sgs, cor_sgs_w, cov_sgs_w, wk_cor)
!!      subroutine cal_correlate_diff_area(layer_tbl, iele_fsmp_stack,  &
!!     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,        &
!!     &          n_tensor, icomp_f, n_int, num_sgs_coefs,              &
!!     &          ave_diff_simi_w, ave_diff_grad_w,                     &
!!     &          cor_diff_w, cov_diff_w, wk_cor)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!
!!        type(dynamic_correlation_data), intent(inout) :: wk_cor
!
      module cal_correlate_4_dynamic
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
      use m_fem_gauss_int_coefs
      use t_jacobians
      use t_work_layer_correlate
!
      use int_vol_4_model_coef
      use cal_layerd_ave_correlate
!
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_correlate_sgs_dynamic                              &
     &         (layer_tbl, node, ele, iphys, nod_fld,                   &
     &          jac_3d_q, jac_3d_l, n_tensor, icomp_f, n_int,           &
     &          nlayer_SGS, num_sgs_coefs, ave_sgs_simi, ave_sgs_grad,  &
     &          cor_sgs, cov_sgs, cor_sgs_w, cov_sgs_w, wk_cor)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      integer (kind = kint), intent(in) :: nlayer_SGS, num_sgs_coefs
      integer (kind = kint), intent(in) :: n_int, n_tensor, icomp_f
!
      real(kind = kreal), intent(in)                                    &
     &          :: ave_sgs_simi(nlayer_SGS,num_sgs_coefs)
      real(kind = kreal), intent(in)                                    &
     &          :: ave_sgs_grad(nlayer_SGS,num_sgs_coefs)
!
      real(kind = kreal), intent(inout)                                 &
     &          :: cor_sgs(nlayer_SGS,num_sgs_coefs)
      real(kind = kreal), intent(inout)                                 &
     &          :: cov_sgs(nlayer_SGS,num_sgs_coefs)
!
      real(kind = kreal), intent(inout) :: cor_sgs_w(num_sgs_coefs)
      real(kind = kreal), intent(inout) :: cov_sgs_w(num_sgs_coefs)
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
!
!
!  Volume integration:                        int_vol_layer_correlate
      call int_vol_layer_correlate(layer_tbl,                           &
     &    node, ele, iphys, nod_fld, g_FEM1, jac_3d_q, jac_3d_l,        &
     &    n_tensor, n_int, ave_sgs_simi(1,icomp_f),                     &
     &    ave_sgs_grad(1,icomp_f), wk_cor)
!
      call sum_layerd_correlation(layer_tbl%e_grp%num_grp, wk_cor)
      call cal_layered_correlation                                      &
     &   (wk_cor%nlayer, wk_cor%ncomp_sgl, wk_cor%ncomp_dble, n_tensor, &
     &    layer_tbl%a_vol_layer, wk_cor%cov_les, wk_cor%sig_les,        &
     &    cor_sgs(1,icomp_f), cov_sgs(1,icomp_f))
!
      call sum_whole_correlation(wk_cor)
      call cal_all_layer_correlation                                    &
     &   (wk_cor%ncomp_sgl, wk_cor%ncomp_dble, n_tensor,                &
     &    layer_tbl%vol_total_layer(1), wk_cor%cov_wg, wk_cor%sig_wg,   &
     &    cor_sgs_w(icomp_f), cov_sgs_w(icomp_f) )
!
      end subroutine cal_correlate_sgs_dynamic
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_correlate_diff_area(layer_tbl, iele_fsmp_stack,    &
     &          node, ele, iphys, nod_fld, jac_3d_q, jac_3d_l,          &
     &          n_tensor, icomp_f, n_int, num_sgs_coefs,                &
     &          ave_diff_simi_w, ave_diff_grad_w,                       &
     &          cor_diff_w, cov_diff_w, wk_cor)
!
      use int_vol_4_model_coef
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      integer (kind = kint), intent(in) :: n_tensor
      integer (kind = kint), intent(in) :: n_int, icomp_f
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: num_sgs_coefs
      real(kind = kreal), intent(in) :: ave_diff_simi_w(num_sgs_coefs)
      real(kind = kreal), intent(in) :: ave_diff_grad_w(num_sgs_coefs)
!
      real(kind = kreal), intent(inout) :: cor_diff_w(num_sgs_coefs)
      real(kind = kreal), intent(inout) :: cov_diff_w(num_sgs_coefs)
!
      type(dynamic_correlation_data), intent(inout) :: wk_cor
!
!
!  Volume integration:                      int_vol_diff_correlate
      call int_vol_diff_correlate(iele_fsmp_stack,                      &
     &    node, ele, iphys, nod_fld, g_FEM1, jac_3d_q, jac_3d_l,        &
     &    n_tensor, n_int, ave_diff_simi_w(icomp_f),                    &
     &    ave_diff_grad_w(icomp_f), wk_cor)
!
      call sum_whole_correlation(wk_cor)
      call cal_all_layer_correlation                                    &
     &   (wk_cor%ncomp_sgl, wk_cor%ncomp_dble, n_tensor,                &
     &    layer_tbl%vol_total_layer(1), wk_cor%cov_wg, wk_cor%sig_wg,   &
     &    cor_diff_w(icomp_f), cov_diff_w(icomp_f) )
!
      end subroutine cal_correlate_diff_area
!
!  ---------------------------------------------------------------------
!
      end module cal_correlate_4_dynamic
      