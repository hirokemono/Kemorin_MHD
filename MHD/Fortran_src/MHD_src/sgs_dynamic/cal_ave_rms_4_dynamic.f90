!cal_ave_rms_4_dynamic.f90
!      module cal_ave_rms_4_dynamic
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine cal_ave_rms_sgs_dynamic(n_tensor, icomp_f, n_int)
!      subroutine cal_ave_rms_diff_layerd(n_tensor, icomp_f, n_int)
!      subroutine cal_ave_rms_diff_area(iele_fsmp_stack, n_tensor,      &
!     &          icomp_f, n_int, volume_d)
!
      module cal_ave_rms_4_dynamic
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_ele_info_4_dynamical
      use m_work_layer_correlate
!
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
      subroutine cal_ave_rms_sgs_dynamic(n_tensor, icomp_f, n_int)
!
      use m_layering_ele_list
      use int_vol_4_model_coef
!
      integer (kind = kint), intent(in) :: n_tensor
      integer (kind = kint), intent(in) :: n_int, icomp_f
!
!
      call int_vol_rms_ave_dynamic(n_tensor, n_int)
!
      call sum_layerd_averages(layer_tbl1%e_grp%num_grp)
      call divide_layers_ave_by_vol(layer_tbl1%e_grp%num_grp, n_tensor, &
     &    layer_tbl1%a_vol_layer, ave_sgs_simi(1,icomp_f),              &
     &    ave_sgs_grad(1,icomp_f), rms_sgs_simi(1,icomp_f),             &
     &    rms_sgs_grad(1,icomp_f), ratio_sgs(1,icomp_f) )
!
      call sum_whole_averages
      call divide_all_layer_ave_by_vol                                  &
     &     (n_tensor, layer_tbl1%vol_total_layer(1),                    &
     &      ave_sgs_simi_w(icomp_f), ave_sgs_grad_w(icomp_f),           &
     &      rms_sgs_simi_w(icomp_f), rms_sgs_grad_w(icomp_f),           &
     &      ratio_sgs_w(icomp_f) )
!
      end subroutine cal_ave_rms_sgs_dynamic
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_ave_rms_diff_layerd(n_tensor, icomp_f, n_int)
!
      use m_layering_ele_list
      use int_vol_4_model_coef
!
      integer (kind = kint), intent(in) :: n_tensor
      integer (kind = kint), intent(in) :: n_int, icomp_f
!
!
      call int_vol_rms_ave_dynamic(n_tensor, n_int)
!
      call sum_layerd_averages(layer_tbl1%e_grp%num_grp)
      call divide_layers_ave_by_vol(layer_tbl1%e_grp%num_grp, n_tensor, &
     &    layer_tbl1%a_vol_layer, ave_diff_simi(1,icomp_f),             &
     &    ave_diff_grad(1,icomp_f), rms_diff_simi(1,icomp_f),           &
     &    rms_diff_grad(1,icomp_f), ratio_diff(1,icomp_f) )
!
      call sum_whole_averages
      call divide_all_layer_ave_by_vol                                  &
     &     (n_tensor, layer_tbl1%vol_total_layer(1),                    &
     &      ave_diff_simi_w(icomp_f), ave_diff_grad_w(icomp_f),         &
     &      rms_diff_simi_w(icomp_f), rms_diff_grad_w(icomp_f),         &
     &      ratio_diff_w(icomp_f) )
!
      end subroutine cal_ave_rms_diff_layerd
!
!  ---------------------------------------------------------------------
!
      subroutine cal_ave_rms_diff_area(iele_fsmp_stack, n_tensor,       &
     &          icomp_f, n_int, volume_d)
!
      use int_vol_4_model_coef
!
      integer (kind = kint), intent(in) :: n_tensor
      integer (kind = kint), intent(in) :: n_int, icomp_f
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: volume_d
!
!
      call int_vol_rms_ave_diff(iele_fsmp_stack, n_tensor, n_int)
!
      call sum_whole_averages
      call divide_all_layer_ave_by_vol(n_tensor, volume_d,              &
     &      ave_diff_simi_w(icomp_f), ave_diff_grad_w(icomp_f),         &
     &      rms_diff_simi_w(icomp_f), rms_diff_grad_w(icomp_f),         &
     &      ratio_diff_w(icomp_f) )
!
      end subroutine cal_ave_rms_diff_area
!
!  ---------------------------------------------------------------------
!
      end module cal_ave_rms_4_dynamic
