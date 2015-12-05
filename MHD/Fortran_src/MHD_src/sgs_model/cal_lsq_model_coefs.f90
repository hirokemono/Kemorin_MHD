!
!      module cal_lsq_model_coefs
!
!     Written by H. Matsui on Oct. 2005
!
!      subroutine cal_model_coef_4_flux(layer_tbl,                      &
!     &          itype_csim, numdir, ifield_d, icomp_f, n_int)
!        type(layering_tbl), intent(in) :: layer_tbl
!
!      subroutine cal_lsq_diff_coef(iele_fsmp_stack, numdir, ifield_d,  &
!     &          icomp_f, n_int)
!      subroutine cal_lsq_layerd_diff_coef                              &
!     &         (layer_tbl, numdir, ifield_d, icomp_f, n_int)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_lsq_model_coefs
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_machine_parameter
      use m_ele_info_4_dynamical
      use m_work_4_dynamic_model
!
      use t_layering_ele_list
!
      use set_sgs_diff_model_coefs
      use merge_dynamic_coefs
      use merge_coefs_whole_dynamic
!
      implicit none
!
      integer(kind = kint), parameter :: ncomp_sgs = 18
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_model_coef_4_flux(layer_tbl,                       &
     &          itype_csim, numdir, ifield_d, icomp_f, n_int)
!
      use t_group_data
      use int_vol_4_model_coef
!
      type(layering_tbl), intent(in) :: layer_tbl
      integer (kind = kint), intent(in) :: itype_csim, numdir
      integer (kind = kint), intent(in) :: n_int, ifield_d, icomp_f
!
!
!  Volume integration:                      int_vol_model_coef
      call int_vol_model_coef(layer_tbl, numdir, n_int)
!
!    model coefficients for each components: lsq_model_coefs_4_comps
      call lsq_model_coefs_4_comps(layer_tbl%e_grp%num_grp, ncomp_sgs)
!
      call merge_coefs_4_dynamic(numdir, layer_tbl%e_grp%num_grp,       &
     &    sgs_c_coef(1,icomp_f), sgs_f_coef(1,ifield_d),                &
     &    cor_sgs(1,icomp_f))
!
      call lsq_whole_model_coefs(ncomp_sgs)
!
      call s_merge_coefs_w_dynamic(numdir, sgs_c_whole(icomp_f),        &
     &    sgs_f_whole(ifield_d), cor_sgs_w(icomp_f))
!
      call clippging_sgs_coefs(numdir, ifield_d, icomp_f)
!
      call set_model_coefs_2_ele(itype_csim, numdir, ifield_d, icomp_f, &
     &    layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,            &
     &    layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp)
!
      end subroutine cal_model_coef_4_flux
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_lsq_diff_coef(iele_fsmp_stack, numdir, ifield_d,   &
     &          icomp_f, n_int)
!
      use int_vol_4_model_coef
!
      integer(kind=kint), intent(in) :: numdir
      integer(kind=kint), intent(in) :: ifield_d, icomp_f, n_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
!  Volume integration: int_vol_diff_coef
      call int_vol_diff_coef(iele_fsmp_stack, numdir, n_int)
!
      call lsq_whole_model_coefs(ncomp_sgs)
!
      call s_merge_coefs_w_dynamic(numdir, diff_c_whole(icomp_f),       &
     &    diff_f_whole(ifield_d), cor_diff_w(icomp_f) )
!
      call clippging_sgs_diff_coefs(numdir, ifield_d, icomp_f)
!
      call set_diff_coefs_whole_ele(iele_fsmp_stack, ifield_d)
!
      end subroutine cal_lsq_diff_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_lsq_layerd_diff_coef                               &
     &         (layer_tbl, numdir, ifield_d, icomp_f, n_int)
!
      use t_group_data
      use int_vol_4_model_coef
!
      type(layering_tbl), intent(in) :: layer_tbl
      integer (kind = kint), intent(in) :: numdir, n_int
      integer (kind = kint), intent(in) :: ifield_d, icomp_f
!
!
!  Volume integration:                      int_vol_model_coef
      call int_vol_model_coef(layer_tbl, numdir, n_int)
!
!    model coefficients for each components: lsq_model_coefs_4_comps
      call lsq_model_coefs_4_comps(layer_tbl%e_grp%num_grp, ncomp_sgs)
!
      call merge_coefs_4_dynamic(numdir, layer_tbl%e_grp%num_grp,       &
     &    diff_c_coef(1,icomp_f), diff_f_coef(1,ifield_d),              &
     &    cor_diff(1,icomp_f) )
!
      call lsq_whole_model_coefs(ncomp_sgs)
!
      call s_merge_coefs_w_dynamic(numdir, diff_c_whole(icomp_f),       &
     &    diff_f_whole(ifield_d), cor_diff_w(icomp_f))
!
      call clippging_sgs_diff_coefs(numdir, ifield_d, icomp_f)
!
      call set_diff_coefs_layer_ele                                     &
     &   (ifield_d, layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,  &
     &    layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp)
!
      end subroutine cal_lsq_layerd_diff_coef
!
!  ---------------------------------------------------------------------
!
      end module cal_lsq_model_coefs
