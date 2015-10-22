!
!     module int_vol_thermal_ele
!
!     Written by H. Matsui on Aug., 2005
!
!      subroutine int_vol_temp_ele(ncomp_ele, iele_velo, d_ele)
!      subroutine int_vol_temp_ele_upw(ncomp_ele, iele_velo, d_ele)
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
      use m_node_phys_data
      use m_fem_gauss_int_coefs
      use m_sorted_node
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
      use m_filter_elength
!
      use m_physical_property
      use m_SGS_model_coefs
      use m_SGS_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_temp_ele(ncomp_ele, iele_velo, d_ele)
!
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nonlinear_type
      use fem_skv_div_sgs_flux_type
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2, num_int
!
!
      if (coef_nega_t .eq. 0.0d0 ) return
!
      num_int = intg_point_t_evo
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, iphys%i_temp, coef_nega_t, fem1_wk%scalar_1)
!
        if(iflag_SGS_heat .ne. id_SGS_none                              &
     &    .and. iflag_commute_heat .eq. id_SGS_commute_ON) then
          call SGS_const_vector_each_ele(node1, ele1, nod_fld1,         &
     &        k2, iphys%i_velo, iphys%i_temp, iphys%i_SGS_h_flux,       &
     &        coef_nega_t, mhd_fem1_wk%sgs_v1, fem1_wk%vector_1)
          call fem_skv_scl_inertia_modsgs_pg(iele_fl_smp_stack,         &
     &        num_int, k2, ifilter_final, ak_diff(1,iak_diff_hf),       &
     &        ele1, jac1_3d_q, FEM1_elen, fem1_wk%scalar_1,             &
     &        mhd_fem1_wk%sgs_v1, fem1_wk%vector_1, d_ele(1,iele_velo), &
     &        fem1_wk%sk6)
        else if(iflag_SGS_heat .ne. id_SGS_none) then
          call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1, k2,    &
     &        iphys%i_SGS_h_flux, coef_nega_t, mhd_fem1_wk%sgs_v1)
          call fem_skv_scl_inertia_sgs_pg(iele_fl_smp_stack,            &
     &        num_int, k2, ele1, jac1_3d_q,                             &
     &        fem1_wk%scalar_1, mhd_fem1_wk%sgs_v1, d_ele(1,iele_velo), &
     &        fem1_wk%sk6)
        else
          call fem_skv_scalar_inertia_type(iele_fl_smp_stack,           &
     &        num_int, k2, fem1_wk%scalar_1, d_ele(1,iele_velo),        &
     &        ele1, jac1_3d_q, fem1_wk%sk6)
        end if
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_temp_ele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_temp_ele_upw(ncomp_ele, iele_velo, d_ele)
!
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_nonlinear_upw_type
      use fem_skv_div_sgs_flux_upw
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2, num_int
!
!
      if (coef_nega_t .eq. 0.0d0 ) return
!
      num_int = intg_point_t_evo
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node1, ele1, nod_fld1,          &
     &      k2, iphys%i_temp, coef_nega_t, fem1_wk%scalar_1)
!
        if(iflag_SGS_heat .ne. id_SGS_none                              &
     &    .and. iflag_commute_heat .eq. id_SGS_commute_ON) then
          call SGS_const_vector_each_ele(node1, ele1, nod_fld1,         &
     &        k2, iphys%i_velo, iphys%i_temp, iphys%i_SGS_h_flux,       &
     &        coef_nega_t, mhd_fem1_wk%sgs_v1, fem1_wk%vector_1)
          call fem_skv_scl_inertia_msgs_upw(iele_fl_smp_stack,          &
     &        num_int, k2, ifilter_final, ak_diff(1,iak_diff_hf),       &
     &        ele1, jac1_3d_q, FEM1_elen, fem1_wk%scalar_1,             &
     &        mhd_fem1_wk%sgs_v1, fem1_wk%vector_1, d_ele(1,iele_velo), &
     &        d_ele(1,iele_velo), fem1_wk%sk6)
        else if(iflag_SGS_heat .ne. id_SGS_none) then
          call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1, k2,    &
     &        iphys%i_SGS_h_flux, coef_nega_t, mhd_fem1_wk%sgs_v1)
          call fem_skv_scl_inertia_sgs_upwind(iele_fl_smp_stack,        &
     &       num_int, k2, ele1, jac1_3d_q, fem1_wk%scalar_1,            &
     &       mhd_fem1_wk%sgs_v1, d_ele(1,iele_velo),                    &
     &       d_ele(1,iele_velo), fem1_wk%sk6)
        else
          call fem_skv_scalar_inertia_upwind(iele_fl_smp_stack,         &
     &       num_int, k2, fem1_wk%scalar_1, d_ele(1,iele_velo),         &
     &       d_ele(1,iele_velo), ele1, jac1_3d_q, fem1_wk%sk6)
        end if
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_temp_ele_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_thermal_ele
