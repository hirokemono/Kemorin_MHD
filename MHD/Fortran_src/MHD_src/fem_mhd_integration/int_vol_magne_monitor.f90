!
!     module int_vol_magne_monitor
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine int_vol_magne_monitor_pg(i_field)
!      subroutine int_vol_magne_monitor_upm(i_field)
!
      module int_vol_magne_monitor
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_parameter
      use m_machine_parameter
      use m_phys_constants
      use m_geometry_data_MHD
      use m_physical_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_node_phys_address
      use m_element_phys_address
      use m_element_phys_data
      use m_fem_gauss_int_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_magne_monitor_pg(i_field)
!
      use int_vol_vect_diff_1st
      use int_vol_vect_cst_diff_1st
      use int_vol_mag_induct_1st
      use int_vol_SGS_mag_induct_1st
!
      integer(kind=kint), intent(in) :: i_field
!
!
      if (i_field .eq. iphys%i_induction) then
        call int_vol_mag_induct_pg(iele_cd_smp_stack, intg_point_t_evo)
!
      else if (i_field .eq. iphys%i_induct_div) then
        call int_vol_div_as_tsr_1st(iele_cd_smp_stack,                  &
     &      intg_point_t_evo, iphys%i_induct_t)
!
      else if (i_field .eq. iphys%i_SGS_induction) then
        if(iflag_commute_induction .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_idct_mod_pg(iele_cd_smp_stack,           &
     &        intg_point_t_evo, ifilter_final)
        else
          call int_vol_div_as_tsr_w_const_1st(iele_cd_smp_stack,        &
     &        intg_point_t_evo, iphys%i_SGS_induct_t, coef_induct)
        end if
      end if
!
      end subroutine int_vol_magne_monitor_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_magne_monitor_upm(i_field)
!
      use int_vol_vect_diff_upw_1st
      use int_vol_vect_cst_diff_upw_1
      use int_vol_mag_induct_1st
      use int_vol_SGS_mag_induct_1st
!
      integer(kind=kint), intent(in) :: i_field
!
!
      if (i_field .eq. iphys%i_induction) then
        call int_vol_mag_induct_upm(iele_cd_smp_stack,                  &
     &      intg_point_t_evo)
!
      else if (i_field .eq. iphys%i_induct_div) then
        call int_vol_div_as_tsr_upw_1st(iele_cd_smp_stack,              &
     &    intg_point_t_evo, iphys%i_induct_t, iphys_ele%i_magne)
!
      else if (i_field .eq. iphys%i_SGS_induction) then
        if(iflag_commute_induction .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_idct_mod_upm(iele_cd_smp_stack,          &
     &        intg_point_t_evo, ifilter_final)
        else
          call int_vol_div_as_tsr_cst_upw_1(iele_cd_smp_stack,          &
     &        intg_point_t_evo, iphys%i_SGS_induct_t, coef_induct,      &
     &        iphys_ele%i_magne)
        end if
      end if
!
      end subroutine int_vol_magne_monitor_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_magne_monitor
