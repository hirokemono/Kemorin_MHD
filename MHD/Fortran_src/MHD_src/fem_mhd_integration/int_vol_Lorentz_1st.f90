!
!     module int_vol_Lorentz_1st
!
!     numerical integration for finite elememt equations of momentum
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine int_vol_Lorentz_pg(iele_fsmp_stack, n_int)
!      subroutine int_vol_full_Lorentz_pg(iele_fsmp_stack, n_int)
!      subroutine int_vol_full_rot_Lorentz_pg(iele_fsmp_stack, n_int)
!
!      subroutine int_vol_Lorentz_upw(iele_fsmp_stack, n_int, ie_upw)
!      subroutine int_vol_full_Lorentz_upw(iele_fsmp_stack, n_int,      &
!     &           ie_upw)
!
      module int_vol_Lorentz_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_phys_constants
      use m_physical_property
      use m_node_phys_address
      use m_element_phys_data
      use m_element_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_Lorentz_pg(iele_fsmp_stack, n_int)
!
      use cal_add_smp
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_add_smp
      use int_vol_inertia_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
!
      call add_const_to_vector_smp(np_smp, numele, iele_fsmp_stack,     &
     &    d_ele(1,iphys_ele%i_magne), ex_magne, vect_e)
      call int_vol_vector_inertia_1st(iele_fsmp_stack,                  &
     &    n_int, iphys%i_magne, vect_e, coef_lor)
!
      end subroutine int_vol_Lorentz_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_full_Lorentz_pg(iele_fsmp_stack, n_int)
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
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2=1, nnod_4_ele
        call vector_phys_2_each_element(k2, iphys%i_magne, magne_1)
        call fem_skv_lorentz_full_pg_1st(iele_fsmp_stack, n_int, k2,    &
     &      coef_lor, magne_1, d_ele(1,iphys_ele%i_magne),              &
     &      ex_magne, sk6)
!
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_full_Lorentz_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_full_rot_Lorentz_pg(iele_fsmp_stack, n_int)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_add_smp
      use nodal_fld_cst_to_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_lorentz_full_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2=1, nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, iphys%i_vecp,               &
     &      coef_lor, vect_1)
        call add_const_to_vector_smp(np_smp, numele,                    &
     &      iele_fsmp_stack, d_ele(1,iphys_ele%i_magne),                &
     &      ex_magne, vect_e)
!
        call fem_skv_lorentz_rot_1st(iele_fsmp_stack,                   &
     &        n_int, k2, vect_1, vect_e, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_full_rot_Lorentz_pg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_Lorentz_upw(iele_fsmp_stack, n_int, ie_upw)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_add_smp
      use int_vol_inertia_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: ie_upw
!
!
      call add_const_to_vector_smp(np_smp, numele, iele_fsmp_stack,     &
     &    d_ele(1,iphys_ele%i_magne), ex_magne, vect_e)
      call int_vol_vector_inertia_upw_1st(iele_fsmp_stack,              &
     &    n_int, iphys%i_magne, vect_e, d_ele(1,ie_upw), coef_lor)
!
      end subroutine int_vol_Lorentz_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_full_Lorentz_upw(iele_fsmp_stack, n_int,       &
     &           ie_upw)
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
      integer(kind = kint), intent(in) :: ie_upw
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, nnod_4_ele
        call vector_phys_2_each_element(k2, iphys%i_magne, magne_1)
        call fem_skv_lorentz_full_upw_1st(iele_fsmp_stack,              &
     &        n_int, k2, coef_lor, magne_1, d_ele(1,ie_upw),            &
     &        d_ele(1,iphys_ele%i_magne), ex_magne, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_full_Lorentz_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_Lorentz_1st
