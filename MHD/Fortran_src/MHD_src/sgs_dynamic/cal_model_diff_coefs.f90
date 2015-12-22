!
!      module cal_model_diff_coefs
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine cal_model_coefs(layer_tbl, itype_csim, n_tensor,      &
!     &          ifield_d, icomp_f, n_int)
!
!      subroutine cal_diff_coef(layer_tbl,                              &
!     &          numdir, ifield_d, icomp_f, n_int)
!      subroutine cal_diff_coef_fluid(layer_tbl,                        &
!     &          numdir, ifield_d, icomp_f, n_int)
!      subroutine cal_diff_coef_conduct(layer_tbl,                      &
!     &          numdir, ifield_d, icomp_f, n_int)
!        type(layering_tbl), intent(in) :: layer_tbl
!
!
      module cal_model_diff_coefs
!
      use m_precision
!
      use t_layering_ele_list
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
      subroutine cal_model_coefs(layer_tbl, itype_csim, n_tensor,       &
     &          ifield_d, icomp_f, n_int)
!
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
      type(layering_tbl), intent(in) :: layer_tbl
      integer (kind = kint), intent(in) :: itype_csim, n_tensor
      integer (kind = kint), intent(in) :: n_int, ifield_d, icomp_f
!
!
      call cal_ave_rms_sgs_dynamic(layer_tbl,                           &
     &    n_tensor, icomp_f, n_int)
!
      call cal_correlate_sgs_dynamic(layer_tbl,                         &
     &    n_tensor, icomp_f, n_int)
!
      call cal_model_coef_4_flux(layer_tbl, itype_csim,                 &
     &    n_tensor, ifield_d, icomp_f, n_int)
!
      end subroutine cal_model_coefs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_diff_coef(layer_tbl,                               &
     &          numdir, ifield_d, icomp_f, n_int)
!
      use m_control_parameter
      use m_geometry_data
!
      type(layering_tbl), intent(in) :: layer_tbl
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
!
!
      if (iset_DIFF_model_coefs .eq. 1) then
        call cal_layerd_diff_coef(layer_tbl,                            &
     &      numdir, ifield_d, icomp_f, n_int)
      else
        call cal_whole_diff_coef(layer_tbl, ele1%istack_ele_smp,        &
     &      numdir, ifield_d, icomp_f, n_int, ele1%volume)
      end if
!
      end subroutine cal_diff_coef
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_coef_fluid(layer_tbl,                         &
     &          numdir, ifield_d, icomp_f, n_int)
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
!
      type(layering_tbl), intent(in) :: layer_tbl
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
!
      if (iset_DIFF_model_coefs .eq. 1) then
        call cal_layerd_diff_coef(layer_tbl,                            &
     &      numdir, ifield_d, icomp_f, n_int)
      else
        call cal_whole_diff_coef(layer_tbl, fluid1%istack_ele_fld_smp,  &
     &      numdir, ifield_d, icomp_f, n_int, fluid1%volume)
      end if
!
      end subroutine cal_diff_coef_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_diff_coef_conduct(layer_tbl,                       &
     &          numdir, ifield_d, icomp_f, n_int)
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
!
      type(layering_tbl), intent(in) :: layer_tbl
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: ifield_d, icomp_f, n_int
!
!
      if (iset_DIFF_model_coefs .eq. 1) then
        call cal_layerd_diff_coef(layer_tbl,                            &
     &      numdir, ifield_d, icomp_f, n_int)
      else
        call cal_whole_diff_coef                                        &
     &     (layer_tbl, conduct1%istack_ele_fld_smp,                     &
     &      numdir, ifield_d, icomp_f, n_int, conduct1%volume)
      end if
!
      end subroutine cal_diff_coef_conduct
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_layerd_diff_coef                                   &
     &         (layer_tbl, numdir, ifield_d, icomp_f, n_int)
!
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
      type(layering_tbl), intent(in) :: layer_tbl
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: n_int, ifield_d, icomp_f
!
!
      call cal_ave_rms_diff_layerd(layer_tbl, numdir, icomp_f, n_int)
      call cal_correlate_diff_dynamic                                   &
     &   (layer_tbl, numdir, icomp_f, n_int)
      call cal_lsq_layerd_diff_coef                                     &
     &   (layer_tbl, numdir, ifield_d, icomp_f, n_int)
!
      end subroutine cal_layerd_diff_coef
!
!  ---------------------------------------------------------------------
!
      subroutine cal_whole_diff_coef(layer_tbl, iele_fsmp_stack,        &
     &           numdir, ifield_d, icomp_f, n_int, volume_d)
!
      use cal_lsq_model_coefs
      use cal_ave_rms_4_dynamic
      use cal_correlate_4_dynamic
!
!
      type(layering_tbl), intent(in) :: layer_tbl
      integer (kind = kint), intent(in) :: numdir
      integer (kind = kint), intent(in) :: n_int, ifield_d, icomp_f
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: volume_d
!
!
      call cal_ave_rms_diff_area(iele_fsmp_stack, numdir, icomp_f,      &
     &    n_int, volume_d)
!
      call cal_correlate_diff_area(layer_tbl, iele_fsmp_stack,          &
     &    numdir, icomp_f, n_int)
!
      call cal_lsq_diff_coef(iele_fsmp_stack, numdir, ifield_d,         &
     &    icomp_f, n_int)
!
      end subroutine cal_whole_diff_coef
!
!-----------------------------------------------------------------------
!
      end module cal_model_diff_coefs
