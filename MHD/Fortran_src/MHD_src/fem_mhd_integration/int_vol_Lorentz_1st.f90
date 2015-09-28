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
!      subroutine int_vol_Lorentz_pg(iele_fsmp_stack, n_int,            &
!     &          ncomp_ele, iele_magne, d_ele)
!      subroutine int_vol_full_Lorentz_pg(iele_fsmp_stack, n_int,       &
!     &          ncomp_ele, iele_magne, d_ele)
!      subroutine int_vol_full_rot_Lorentz_pg(iele_fsmp_stack, n_int,   &
!     &          ncomp_ele, d_ele)
!
!      subroutine int_vol_Lorentz_upw(iele_fsmp_stack, n_int,           &
!     &          ncomp_ele, iele_magne, ie_upw, d_ele)
!      subroutine int_vol_full_Lorentz_upw(iele_fsmp_stack, n_int,      &
!     &          ncomp_ele, iele_magne, ie_upw, d_ele)
!
      module int_vol_Lorentz_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_phys_constants
      use m_physical_property
      use m_node_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_Lorentz_pg(iele_fsmp_stack, n_int,             &
     &          ncomp_ele, iele_magne, d_ele)
!
      use cal_add_smp
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_add_smp
      use subtract_const_smp
      use int_vol_inertia_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,ncomp_ele)
!
!
!$omp parallel
      call add_const_to_vector_smp_ow                                   &
     &   (np_smp, ele1%numele, iele_fsmp_stack,                         &
     &    d_ele(1,iele_magne), ex_magne)
!$omp end parallel
!
      call int_vol_vector_inertia_1st                                   &
     &   (iele_fsmp_stack, n_int, iphys%i_magne, ncomp_ele,             &
     &    iele_magne, d_ele, coef_lor)
!
!$omp parallel
      call subt_const_to_vector_smp_ow                                  &
     &   (np_smp, ele1%numele, iele_fsmp_stack,                         &
     &    d_ele(1,iele_magne), ex_magne)
!$omp end parallel
!
      end subroutine int_vol_Lorentz_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_full_Lorentz_pg(iele_fsmp_stack, n_int,        &
     &          ncomp_ele, iele_magne, d_ele)
!
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
      use cal_add_smp
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_lorentz_full_type
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2=1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, iphys%i_magne, magne_1)
        call fem_skv_lorentz_full_galerkin(iele_fsmp_stack, n_int, k2,  &
     &      coef_lor, magne_1, d_ele(1,iele_magne), ex_magne,           &
     &      ele1, jac1_3d_q, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_full_Lorentz_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_full_rot_Lorentz_pg(iele_fsmp_stack, n_int,    &
     &          ncomp_ele, iele_magne, d_ele)
!
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
      use cal_add_smp
      use nodal_fld_cst_to_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_lorentz_full_type
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2=1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, iphys%i_vecp,               &
     &      coef_lor, vect_1)
!
!$omp parallel
        call add_const_to_vector_smp(np_smp, ele1%numele,               &
     &      iele_fsmp_stack, d_ele(1,iele_magne), ex_magne, vect_e)
!$omp end parallel
!
        call fem_skv_lorentz_rot_galerkin(iele_fsmp_stack,              &
     &        n_int, k2, vect_1, vect_e, ele1, jac1_3d_q, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_full_rot_Lorentz_pg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_Lorentz_upw(iele_fsmp_stack, n_int,            &
     &          ncomp_ele, iele_magne, ie_upw, d_ele)
!
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_add_smp
      use subtract_const_smp
      use int_vol_inertia_1st
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne, ie_upw
      real(kind = kreal), intent(inout) :: d_ele(ele1%numele,ncomp_ele)
!
!
!$omp parallel
      call add_const_to_vector_smp_ow                                   &
     &   (np_smp, ele1%numele, iele_fsmp_stack,                         &
     &    d_ele(1,iele_magne), ex_magne)
!$omp end parallel
!
      call int_vol_vector_inertia_upw_1st(iele_fsmp_stack,              &
     &    n_int, iphys%i_magne, ncomp_ele, iele_magne,                  &
     &    ie_upw, d_ele, coef_lor)
!
!$omp parallel
      call subt_const_to_vector_smp_ow                                  &
     &   (np_smp, ele1%numele, iele_fsmp_stack,                         &
     &    d_ele(1,iele_magne), ex_magne)
!$omp end parallel
!
      end subroutine int_vol_Lorentz_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_full_Lorentz_upw(iele_fsmp_stack, n_int,       &
     &          ncomp_ele, iele_magne, ie_upw, d_ele)
!
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
      use cal_add_smp
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_lorentz_full_type
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element(k2, iphys%i_magne, magne_1)
        call fem_skv_lorentz_full_upwind(iele_fsmp_stack,               &
     &        n_int, k2, coef_lor, magne_1, d_ele(1,ie_upw),            &
     &        d_ele(1,iele_magne), ex_magne, ele1, jac1_3d_q, sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_vol_full_Lorentz_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_Lorentz_1st
