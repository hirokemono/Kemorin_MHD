!
!      module cal_model_diff_coefs
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine cal_model_coefs(itype_csim, n_tensor,                 &
!     &          ifield_d, icomp_f, n_int)
!
!      subroutine cal_layerd_diff_coef(numdir, ifield_d, icomp_f, n_int)
!      subroutine cal_whole_diff_coef(iele_fsmp_stack, numdir, ifield_d,&
!     &          icomp_f, n_int, volume_d)
!
!      subroutine cal_diff_coef(numdir, ifield_d, icomp_f, n_int)
!      subroutine cal_diff_coef_fluid(numdir, ifield_d, icomp_f, n_int)
!      subroutine cal_diff_coef_conduct(numdir, ifield_d, icomp_f,      &
!     &          n_int)
!
!
      module cal_model_diff_coefs
!
      use m_precision
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
      subroutine cal_model_coefs(itype_csim, n_tensor,                  &
     &          ifield_d, icomp_f, n_int)
!
      use m_layering_ele_list
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
      integer (kind = kint), intent(in) :: itype_csim, n_tensor
      integer (kind = kint), intent(in) :: n_int, ifield_d, icomp_f
!
!
      call cal_ave_rms_sgs_dynamic(n_tensor, icomp_f, n_int)
!
      call cal_correlate_sgs_dynamic(n_tensor, icomp_f, n_int)
!
      call cal_model_coef_4_flux(layer_tbl1%e_grp, itype_csim,          &
     &    n_tensor, ifield_d, icomp_f, n_int)
!
      end subroutine cal_model_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine cal_layerd_diff_coef(numdir, ifield_d, icomp_f, n_int)
!
      use m_layering_ele_list
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: n_int, ifield_d, icomp_f
!
!
      call cal_ave_rms_diff_layerd(numdir, icomp_f, n_int)
      call cal_correlate_diff_dynamic(numdir, icomp_f, n_int)
      call cal_lsq_layerd_diff_coef                                     &
     &   (layer_tbl1%e_grp, numdir, ifield_d, icomp_f, n_int)
!
      end subroutine cal_layerd_diff_coef
!
!  ---------------------------------------------------------------------
!
      subroutine cal_whole_diff_coef(iele_fsmp_stack, numdir, ifield_d, &
     &          icomp_f, n_int, volume_d)
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
!
      call cal_ave_rms_diff_area(iele_fsmp_stack, numdir, icomp_f,      &
     &    n_int, volume_d)
!
      call cal_correlate_diff_area(iele_fsmp_stack, numdir, icomp_f,    &
     &    n_int)
!
      call cal_lsq_diff_coef(iele_fsmp_stack, numdir, ifield_d,         &
     &    icomp_f, n_int)
!
      end subroutine cal_whole_diff_coef
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_diff_coef(numdir, ifield_d, icomp_f, n_int)
!
      use m_control_parameter
      use m_geometry_data
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
!
      if (iset_DIFF_model_coefs .eq. 1) then
        call cal_layerd_diff_coef(numdir, ifield_d, icomp_f, n_int)
      else
        call cal_whole_diff_coef(ele1%istack_ele_smp, numdir, ifield_d, &
     &      icomp_f, n_int, ele1%volume)
      end if
!
      end subroutine cal_diff_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_coef_fluid(numdir, ifield_d, icomp_f, n_int)
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
!
      if (iset_DIFF_model_coefs .eq. 1) then
        call cal_layerd_diff_coef(numdir, ifield_d, icomp_f, n_int)
      else
        call cal_whole_diff_coef(iele_fl_smp_stack, numdir, ifield_d,   &
     &        icomp_f, n_int, vol_fluid)
      end if
!
      end subroutine cal_diff_coef_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_coef_conduct(numdir, ifield_d, icomp_f,       &
     &          n_int)
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
!
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
!
      if (iset_DIFF_model_coefs .eq. 1) then
        call cal_layerd_diff_coef(numdir, ifield_d, icomp_f, n_int)
      else
        call cal_whole_diff_coef(iele_cd_smp_stack, numdir, ifield_d,   &
     &      icomp_f, n_int, vol_conduct)
      end if
!
      end subroutine cal_diff_coef_conduct
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module cal_model_diff_coefs
