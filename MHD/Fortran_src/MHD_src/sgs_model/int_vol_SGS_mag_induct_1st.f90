 !int_vol_SGS_mag_induct_1st.f90
!     module int_vol_SGS_mag_induct_1st
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine int_vol_div_SGS_idct_mod_pg(iele_fsmp_stack, n_int)
!      subroutine int_vol_div_SGS_idct_mod_upm(iele_fsmp_stack, n_int,  &
!     &          i_filter, ncomp_ele, i_magne, d_ele)
!
      module int_vol_SGS_mag_induct_1st
!
      use m_precision
!
      use m_geometry_data
      use m_machine_parameter
      use m_phys_constants
      use m_physical_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_node_phys_address
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
      subroutine int_vol_div_SGS_idct_mod_pg(iele_fsmp_stack, n_int,    &
     &          i_filter)
!
      use m_finite_element_matrix
      use m_int_vol_data
      use m_jacobians
      use m_filter_elength
!
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_div_sgs_flux_type
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int, i_filter
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
         call SGS_induct_cst_each_ele_1st(k2,                           &
     &       iphys%i_magne, iphys%i_velo, iphys%i_SGS_induct_t,         &
     &       coef_induct, sgs_e, vect_e)
         call fem_skv_div_sgs_asym_tsr(iele_fsmp_stack, n_int, k2,      &
     &       i_filter, ak_diff(1,iak_diff_uxb),                         &
     &       ele1, jac1_3d_q, FEM1_elen, sgs_e, vect_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_div_SGS_idct_mod_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_idct_mod_upm(iele_fsmp_stack, n_int,   &
     &          i_filter, ncomp_ele, i_magne, d_ele)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use sgs_terms_to_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_div_sgs_flux_upw_1
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int, i_filter
!
      integer(kind = kint), intent(in) :: ncomp_ele, i_magne
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call SGS_induct_cst_each_ele_1st(k2,                            &
     &      iphys%i_magne, iphys%i_velo, iphys%i_SGS_induct_t,          &
     &      coef_induct, sgs_e, vect_e)
        call fem_skv_div_sgs_asym_t_1st_upw(iele_fsmp_stack, n_int, k2, &
     &      i_filter, ak_diff(1,iak_diff_uxb), d_ele(1,i_magne), sgs_e, &
     &      vect_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_div_SGS_idct_mod_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_SGS_mag_induct_1st
