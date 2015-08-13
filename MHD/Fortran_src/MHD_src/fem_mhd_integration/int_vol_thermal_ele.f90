!
!     module int_vol_thermal_ele
!
!     Written by H. Matsui on Aug., 2005
!
!      subroutine int_vol_thermal_ele
!
      module int_vol_thermal_ele
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
      use m_node_phys_address
      use m_element_phys_data
      use m_element_phys_address
      use m_fem_gauss_int_coefs
      use m_physical_property
      use m_SGS_model_coefs
      use m_SGS_address
!
!      use check_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_temp_ele
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use nodal_fld_cst_to_ele_1st
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nonlinear_1st
      use fem_skv_div_sgs_flux_1st
!
      integer(kind=kint) :: k2, num_int
!
!
      if (coef_nega_t .eq. 0.0d0 ) return
!
      num_int = intg_point_t_evo
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_cst_phys_2_each_ele(k2, iphys%i_temp,               &
     &      coef_nega_t, temp_e)
!
        if(iflag_SGS_heat .ne. id_SGS_none                              &
     &    .and. iflag_commute_heat .eq. id_SGS_commute_ON) then
          call SGS_vector_cst_each_ele_1st(k2, iphys%i_velo,            &
     &        iphys%i_temp, iphys%i_SGS_h_flux, coef_nega_t,            &
     &        sgs_e, vect_e)
          call fem_skv_scl_inertia_modsgs_1st(iele_fl_smp_stack,        &
     &        num_int, k2, ifilter_final, ak_diff(1,iak_diff_hf),       &
     &        temp_e, sgs_e, vect_e, d_ele(1,iphys_ele%i_velo), sk6)
        else if(iflag_SGS_heat .ne. id_SGS_none) then
          call vector_cst_phys_2_each_ele(k2, iphys%i_SGS_h_flux,       &
     &        coef_nega_t, sgs_e)
          call fem_skv_scl_inertia_sgs_1st(iele_fl_smp_stack,           &
     &        num_int, k2, temp_e, sgs_e, d_ele(1,iphys_ele%i_velo),    &
     &        sk6)
        else
          call fem_skv_scalar_inertia_1st(iele_fl_smp_stack,            &
     &        num_int, k2, temp_e, d_ele(1,iphys_ele%i_velo), sk6)
        end if
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_temp_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_temp_ele_upw
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use nodal_fld_cst_to_ele_1st
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nonlinear_upw_1st
      use fem_skv_div_sgs_flux_upw_1
!
      integer(kind=kint) :: k2, num_int
!
!
      if (coef_nega_t .eq. 0.0d0 ) return
!
      num_int = intg_point_t_evo
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_cst_phys_2_each_ele(k2, iphys%i_temp,               &
     &      coef_nega_t, temp_e)
!
        if(iflag_SGS_heat .ne. id_SGS_none                              &
     &    .and. iflag_commute_heat .eq. id_SGS_commute_ON) then
          call SGS_vector_cst_each_ele_1st(k2, iphys%i_velo,            &
     &        iphys%i_temp, iphys%i_SGS_h_flux, coef_nega_t,            &
     &        sgs_e, vect_e)
          call fem_skv_scl_inertia_msgs_upw_1(iele_fl_smp_stack,        &
     &        num_int, k2, ifilter_final, ak_diff(1,iak_diff_hf),       &
     &        temp_e, sgs_e, vect_e, d_ele(1,iphys_ele%i_velo),         &
     &        d_ele(1,iphys_ele%i_velo), sk6)
        else if(iflag_SGS_heat .ne. id_SGS_none) then
          call vector_cst_phys_2_each_ele(k2, iphys%i_SGS_h_flux,       &
     &        coef_nega_t, sgs_e)
          call fem_skv_scl_inertia_sgs_upw_1(iele_fl_smp_stack,         &
     &       num_int, k2, temp_e, sgs_e, d_ele(1,iphys_ele%i_velo),     &
     &       d_ele(1,iphys_ele%i_velo), sk6)
        else
          call fem_skv_scalar_inertia_upw_1st(iele_fl_smp_stack,        &
     &       num_int, k2, temp_e, d_ele(1,iphys_ele%i_velo),            &
     &       d_ele(1,iphys_ele%i_velo), sk6)
        end if
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_temp_ele_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_thermal_ele
