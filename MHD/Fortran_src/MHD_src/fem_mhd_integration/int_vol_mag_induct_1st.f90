!
!     module int_vol_mag_induct_1st
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
      module int_vol_mag_induct_1st
!
      use m_precision
!
      use m_geometry_data
      use m_machine_parameter
      use m_phys_constants
      use m_physical_property
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
      subroutine int_vol_mag_induct_pg(iele_fsmp_stack, n_int)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_add_smp
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_lorentz_full_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind=kint) :: k2
!
!  ---------  set number of integral points
!
      call reset_sk6(n_vector)
!
!$omp parallel
      call add_const_to_vector_smp(np_smp, ele1%numele, iele_smp_stack, &
     &    d_ele(1,iphys_ele%i_magne), ex_magne, vect_e)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, nnod_4_ele
        call vector_phys_2_each_element(k2, iphys%i_velo, velo_1)
        call vector_phys_2_each_element(k2, iphys%i_magne, magne_1)
!
        call fem_skv_induction_1st(iele_fsmp_stack, n_int, k2,          &
     &      coef_induct, velo_1, magne_1, d_ele(1,iphys_ele%i_velo),    &
     &      vect_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_mag_induct_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_mag_induct_upm(iele_fsmp_stack, n_int)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_add_smp
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_lorentz_full_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind=kint) :: k2
!
!  ---------  set number of integral points
!
      call reset_sk6(n_vector)
!
!$omp parallel
      call add_const_to_vector_smp(np_smp, ele1%numele, iele_smp_stack, &
     &    d_ele(1,iphys_ele%i_magne), ex_magne, vect_e)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, nnod_4_ele
        call vector_phys_2_each_element(k2, iphys%i_velo, velo_1)
        call vector_phys_2_each_element(k2, iphys%i_magne, magne_1)
!
        call fem_skv_induction_upm_1st(iele_fsmp_stack, n_int, k2,      &
     &      coef_induct, velo_1, magne_1, d_ele(1,iphys_ele%i_velo),    &
     &      vect_e, d_ele(1,iphys_ele%i_magne), sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_mag_induct_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_mag_induct_1st
