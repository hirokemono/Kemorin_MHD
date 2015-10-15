!cal_correlate_4_dynamic.f90
!      module cal_correlate_4_dynamic
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine cal_correlate_sgs_dynamic(n_tensor, icomp_f, n_int)
!      subroutine cal_correlate_diff_dynamic(n_tensor, icomp_f, n_int)
!      subroutine cal_correlate_diff_area(iele_fsmp_stack, n_tensor,  &
!     &          icomp_f, n_int)
!
      module cal_correlate_4_dynamic
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_ele_info_4_dynamical
      use m_work_layer_correlate
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
      subroutine cal_correlate_sgs_dynamic(n_tensor, icomp_f, n_int)
!
      use m_layering_ele_list
!
      integer (kind = kint), intent(in) :: n_tensor
      integer (kind = kint), intent(in) :: n_int, icomp_f
!
!
!  Volume integration:                      int_vol_model_coef
      call int_vol_layer_correlate(n_tensor, n_int,                     &
     &      ave_sgs_simi(1,icomp_f), ave_sgs_grad(1,icomp_f) )
!
      call sum_layerd_correlation(layer_tbl1%e_grp%num_grp)
      call cal_layered_correlation                                      &
     &   (layer_tbl1%e_grp%num_grp, n_tensor, layer_tbl1%a_vol_layer,   &
     &    cor_sgs(1,icomp_f), cov_sgs(1,icomp_f))
!
      call sum_whole_correlation
      call cal_all_layer_correlation                                    &
     &   (n_tensor, layer_tbl1%vol_total_layer(1),                      &
     &    cor_sgs_w(icomp_f), cov_sgs_w(icomp_f) )
!
      end subroutine cal_correlate_sgs_dynamic
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_correlate_diff_dynamic(n_tensor, icomp_f, n_int)
!
      use m_layering_ele_list
!
      integer (kind = kint), intent(in) :: n_tensor
      integer (kind = kint), intent(in) :: n_int, icomp_f
!
!
!  Volume integration:                      int_vol_model_coef
      call int_vol_layer_correlate(n_tensor, n_int,                     &
     &      ave_diff_simi(1,icomp_f), ave_diff_grad(1,icomp_f) )
!
      call sum_layerd_correlation(layer_tbl1%e_grp%num_grp)
      call cal_layered_correlation                                      &
     &   (layer_tbl1%e_grp%num_grp, n_tensor, layer_tbl1%a_vol_layer,   &
     &    cor_diff(1,icomp_f), cov_diff(1,icomp_f) )
!
      call sum_whole_correlation
      call cal_all_layer_correlation                                    &
     &   (n_tensor, layer_tbl1%vol_total_layer(1),                      &
     &    cor_diff_w(icomp_f), cov_diff_w(icomp_f) )
!
      end subroutine cal_correlate_diff_dynamic
!
!  ---------------------------------------------------------------------
!
      subroutine cal_correlate_diff_area(iele_fsmp_stack, n_tensor,     &
     &          icomp_f, n_int)
!
      use m_layering_ele_list
      use int_vol_4_model_coef
!
      integer (kind = kint), intent(in) :: n_tensor
      integer (kind = kint), intent(in) :: n_int, icomp_f
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      sig_w(1:18) = 0.0d0
      cov_w(1:9) =  0.0d0
!
!  Volume integration:                      int_vol_model_coef
      call int_vol_diff_correlate(iele_fsmp_stack, n_tensor, n_int,     &
     &     ave_diff_grad_w(icomp_f), ave_diff_grad_w(icomp_f) )
!
      call sum_whole_correlation
      call cal_all_layer_correlation                                    &
     &   (n_tensor, layer_tbl1%vol_total_layer(1),                      &
     &    cor_diff_w(icomp_f), cov_diff_w(icomp_f) )
!
      end subroutine cal_correlate_diff_area
!
!  ---------------------------------------------------------------------
!
      end module cal_correlate_4_dynamic
      