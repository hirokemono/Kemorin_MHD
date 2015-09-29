!
!     module int_vol_light_comp_ele
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2002
!     modified by H. Matsui on Aug., 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine int_vol_composition_ele(ncomp_ele, iele_velo, d_ele)
!!      subroutine int_vol_composition_ele_upw                          &
!!     &          (ncomp_ele, iele_velo, d_ele)
!
      module int_vol_light_comp_ele
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
      use m_node_phys_address
      use m_physical_property
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
!      use m_SGS_model_coefs
!      use m_SGS_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_composition_ele(ncomp_ele, iele_velo, d_ele)
!
      use m_filter_elength
!
      use nodal_fld_cst_to_ele_1st
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nonlinear_type
      use fem_skv_div_sgs_flux_type
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2, num_int
!
!
      if (coef_nega_c .eq. 0.0d0 ) return
!
      num_int = intg_point_t_evo
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_cst_phys_2_each_ele(k2, iphys%i_light,              &
     &      coef_nega_c, phi_e)
!
!        if(iflag_SGS_comp_flux .ne. id_SGS_none                        &
!     &    .and. iflag_commute_c_flux .eq. id_SGS_commute_ON) then
!          call SGS_vector_cst_each_ele_1st(k2, iphys%i_velo,           &
!     &        iphys%i_light, iphys%i_SGS_c_flux, coef_nega_c,          &
!     &        sgs_e, vect_e)
!          call fem_skv_scl_inertia_modsgs_pg(iele_fl_smp_stack,        &
!     &        num_int, k2, ifilter_final, ak_diff(1,iak_diff_cf),      &
!     &        ele1, jac1_3d_q, FEM1_elen, phi_e, sgs_e, vect_e,        &
!     &        d_ele(1,iele_velo), sk6)
!        else if(iflag_SGS_comp_flux .ne. id_SGS_none) then
!          call vector_cst_phys_2_each_ele(k2, iphys%i_SGS_c_flux,      &
!     &        coef_nega_c, sgs_e)
!          call fem_skv_scl_inertia_sgs_pg(iele_fl_smp_stack,           &
!     &        num_int, k2, ele1, jac1_3d_q,                            &
!     &        phi_e, sgs_e, d_ele(1,iele_velo), sk6)
!        else
          call fem_skv_scalar_inertia_type(iele_fl_smp_stack,           &
     &        num_int, k2, phi_e, d_ele(1,iele_velo),                   &
     &        ele1, jac1_3d_q, sk6)
!        end if
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_composition_ele
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_composition_ele_upw                            &
     &          (ncomp_ele, iele_velo, d_ele)
!
      use nodal_fld_cst_to_ele_1st
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nonlinear_upw_type
      use fem_skv_div_sgs_flux_upw_1
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2, num_int
!
!
      if (coef_nega_c .eq. 0.0d0 ) return
!
      num_int = intg_point_t_evo
      call reset_sk6(n_scalar)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele1%nnod_4_ele
        call scalar_cst_phys_2_each_ele(k2, iphys%i_light,              &
     &      coef_nega_c, phi_e)
!
!        if(iflag_SGS_comp_flux .ne. id_SGS_none                        &
!     &    .and. iflag_commute_c_flux .eq. id_SGS_commute_ON) then
!          call SGS_vector_cst_each_ele_1st(k2, iphys%i_velo,           &
!     &        iphys%i_light, iphys%i_SGS_c_flux, coef_nega_c,          &
!     &        sgs_e, vect_e)
!          call fem_skv_scl_inertia_msgs_upw_1(iele_fl_smp_stack,       &
!     &        num_int, k2, ifilter_final, ak_diff(1,iak_diff_cf),      &
!     &        phi_e, sgs_e, vect_e, d_ele(1,iele_velo),                &
!     &        d_ele(1,iele_velo), sk6)
!        else if(iflag_SGS_comp_flux .ne. id_SGS_none) then
!          call vector_cst_phys_2_each_ele(k2, iphys%i_SGS_c_flux,      &
!     &        coef_nega_c, sgs_e)
!          call fem_skv_scl_inertia_sgs_upw_1(iele_fl_smp_stack,        &
!     &       num_int, k2, phi_e, sgs_e, d_ele(1,iele_velo),            &
!     &       d_ele(1,iele_velo), sk6)
!        else
          call fem_skv_scalar_inertia_upwind(iele_fl_smp_stack,         &
     &       num_int, k2, phi_e, d_ele(1,iele_velo),                    &
     &       d_ele(1,iele_velo), ele1, jac1_3d_q, sk6)
!        end if
      end do
!
      call add1_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_composition_ele_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_light_comp_ele
