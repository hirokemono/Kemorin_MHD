!
!     module int_vol_vect_p_pre
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!!      subroutine int_vol_vect_p_pre_ele(ncomp_ele, iele_magne, d_ele)
!!      subroutine int_vol_vect_p_pre_ele_upm                           &
!!     &         (ncomp_ele, iele_magne, d_ele)
!
      module int_vol_vect_p_pre
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_geometry_data
      use m_phys_constants
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
      use m_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_vect_p_pre_ele(ncomp_ele, iele_magne, d_ele)
!
      use cal_add_smp
      use nodal_fld_cst_to_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nonlinear_type
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
!
!   include external magnetic field
!$omp parallel
      call add_const_to_vector_smp                                      &
     &   (np_smp, ele1%numele, ele1%istack_ele_smp,                     &
     &    d_ele(1,iele_magne), ex_magne, fem1_wk%vector_1)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, iphys%i_velo,               &
     &      coef_induct, mhd_fem1_wk%velo_1)
!
        call fem_skv_rot_inertia_type(iele_cd_smp_stack,                &
     &      intg_point_t_evo, k2, mhd_fem1_wk%velo_1, fem1_wk%vector_1, &
     &      ele1, jac1_3d_q, fem1_wk%sk6)
      end do
!
      call sub3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_vect_p_pre_ele
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_vect_p_pre_ele_upm                             &
     &         (ncomp_ele, iele_magne, d_ele)
!
      use cal_add_smp
      use nodal_fld_cst_to_ele_1st
      use cal_skv_to_ff_smp_1st
      use fem_skv_nonlinear_upw_type
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, fem1_wk%sk6)
!
!$omp parallel
      call add_const_to_vector_smp                                      &
     &   (np_smp, ele1%numele, ele1%istack_ele_smp, &
     &    d_ele(1,iele_magne), ex_magne, fem1_wk%vector_1)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele1%nnod_4_ele
        call vector_cst_phys_2_each_ele(k2, iphys%i_velo,               &
     &      coef_induct, mhd_fem1_wk%velo_1)
!
        call fem_skv_rot_inertia_upwind(iele_cd_smp_stack,              &
     &      intg_point_t_evo, k2, mhd_fem1_wk%velo_1, fem1_wk%vector_1, &
     &      d_ele(1,iele_magne), ele1, jac1_3d_q, fem1_wk%sk6)
      end do
!
      call sub3_skv_to_ff_v_smp_1st(ff_nl_smp, fem1_wk%sk6)
!
      end subroutine int_vol_vect_p_pre_ele_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_vect_p_pre
