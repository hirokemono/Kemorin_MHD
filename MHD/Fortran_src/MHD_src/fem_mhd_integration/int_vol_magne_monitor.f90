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
      use m_machine_parameter
      use m_phys_constants
      use m_geometry_data
      use m_geometry_data_MHD
      use m_physical_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_node_phys_data
      use m_element_phys_data
      use m_fem_gauss_int_coefs
      use m_sorted_node
      use m_jacobians
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
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
      use int_vol_vect_differences
      use int_vol_vect_cst_difference
      use int_vol_mag_induction
      use int_vol_SGS_mag_induct
!
      integer(kind=kint), intent(in) :: i_field
!
!
      if (i_field .eq. iphys%i_induction) then
        call int_vol_mag_induct_pg(node1, ele1,                         &
     &      jac1_3d_q, rhs_tbl1, nod_fld1, iphys, iphys_ele,            &
     &      conduct1%istack_ele_fld_smp, intg_point_t_evo,              &
     &      fld_ele1%ntot_phys, fld_ele1%d_fld,                         &
     &      fem1_wk, mhd_fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_induct_div) then
        call int_vol_div_asym_tsr                                       &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      conduct1%istack_ele_fld_smp, intg_point_t_evo,              &
     &      iphys%i_induct_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_SGS_induction) then
        if(iflag_commute_induction .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_idct_mod_pg(node1, ele1,                 &
     &        jac1_3d_q, rhs_tbl1, nod_fld1, iphys, FEM1_elen,          &
     &        conduct1%istack_ele_fld_smp, intg_point_t_evo,            &
     &        ifilter_final, ak_diff(1,iak_diff_uxb), coef_induct,      &
     &        fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_div_as_tsr_w_const                               &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        conduct1%istack_ele_fld_smp, intg_point_t_evo,            &
     &        iphys%i_SGS_induct_t, coef_induct, fem1_wk, f1_nl)
        end if
      end if
!
      end subroutine int_vol_magne_monitor_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_magne_monitor_upm(i_field)
!
      use m_element_phys_data
      use int_vol_vect_diff_upw
      use int_vol_vect_cst_diff_upw
      use int_vol_mag_induction
      use int_vol_SGS_mag_induct
!
      integer(kind=kint), intent(in) :: i_field
!
!
      if (i_field .eq. iphys%i_induction) then
        call int_vol_mag_induct_upm(node1, ele1,                        &
     &      jac1_3d_q, rhs_tbl1, nod_fld1, iphys, iphys_ele,            &
     &      conduct1%istack_ele_fld_smp, intg_point_t_evo,              &
     &      fld_ele1%ntot_phys, fld_ele1%d_fld,                         &
     &      fem1_wk, mhd_fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_induct_div) then
        call int_vol_div_as_tsr_upw                                     &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      conduct1%istack_ele_fld_smp, intg_point_t_evo,              &
     &      iphys%i_induct_t, fld_ele1%ntot_phys, iphys_ele%i_magne,    &
     &      fld_ele1%d_fld, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_SGS_induction) then
        if(iflag_commute_induction .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_idct_mod_upm(node1, ele1,                &
     &        jac1_3d_q, rhs_tbl1, nod_fld1, iphys, FEM1_elen,          &
     &        conduct1%istack_ele_fld_smp, intg_point_t_evo,            &
     &        ifilter_final, ak_diff(1,iak_diff_uxb), coef_induct,      &
     &        fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld,    &
     &        fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_div_as_tsr_cst_upw                               &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        conduct1%istack_ele_fld_smp, intg_point_t_evo,            &
     &        iphys%i_SGS_induct_t, fld_ele1%ntot_phys,                 &
     &        iphys_ele%i_magne, fld_ele1%d_fld, coef_induct,           &
     &        fem1_wk, f1_nl)
        end if
      end if
!
      end subroutine int_vol_magne_monitor_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_magne_monitor
