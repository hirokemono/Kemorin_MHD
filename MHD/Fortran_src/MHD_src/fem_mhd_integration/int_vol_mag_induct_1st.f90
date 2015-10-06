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
!!      subroutine int_vol_mag_induct_pg                                &
!!     &         (iele_fsmp_stack, n_int, ncomp_ele, d_ele, iphys_ele)
!!      subroutine int_vol_mag_induct_upm                               &
!!     &         (iele_fsmp_stack, n_int, ncomp_ele, d_ele, iphys_ele)
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
      use m_fem_gauss_int_coefs
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
!
!
      use t_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_mag_induct_pg                                  &
     &         (iele_fsmp_stack, n_int, ncomp_ele, d_ele, iphys_ele)
!
      use cal_add_smp
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_lorentz_full_type
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      type(phys_address), intent(in) :: iphys_ele
!
      integer(kind=kint) :: k2
!
!  ---------  set number of integral points
!
      call reset_sk6(n_vector, fem1_wk%sk6)
!
!$omp parallel
      call add_const_to_vector_smp                                      &
     &   (np_smp, ele1%numele, ele1%istack_ele_smp,                     &
     &    d_ele(1,iphys_ele%i_magne), ex_magne, fem1_wk%vector_1)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element                                 &
     &     (k2, iphys%i_velo, mhd_fem1_wk%velo_1)
        call vector_phys_2_each_element(k2, iphys%i_magne, magne_1)
!
        call fem_skv_induction_galerkin(iele_fsmp_stack, n_int, k2,     &
     &      coef_induct, mhd_fem1_wk%velo_1, magne_1,                   &
     &      d_ele(1,iphys_ele%i_velo), fem1_wk%vector_1,                &
     &      ele1, jac1_3d_q, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_mag_induct_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_mag_induct_upm                                 &
     &         (iele_fsmp_stack, n_int, ncomp_ele, d_ele, iphys_ele)
!
      use cal_add_smp
      use nodal_fld_2_each_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_lorentz_full_type
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      type(phys_address), intent(in) :: iphys_ele
!
      integer(kind=kint) :: k2
!
!  ---------  set number of integral points
!
      call reset_sk6(n_vector, fem1_wk%sk6)
!
!$omp parallel
      call add_const_to_vector_smp                                      &
     &   (np_smp, ele1%numele, ele1%istack_ele_smp,                     &
     &    d_ele(1,iphys_ele%i_magne), ex_magne, fem1_wk%vector_1)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_phys_2_each_element                                 &
     &     (k2, iphys%i_velo, mhd_fem1_wk%velo_1)
        call vector_phys_2_each_element(k2, iphys%i_magne, magne_1)
!
        call fem_skv_induction_upmagne(iele_fsmp_stack, n_int, k2,      &
     &      coef_induct, mhd_fem1_wk%velo_1, magne_1,                   &
     &      d_ele(1,iphys_ele%i_velo), fem1_wk%vector_1,                &
     &      d_ele(1,iphys_ele%i_magne), ele1, jac1_3d_q, fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_mag_induct_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_mag_induct_1st
