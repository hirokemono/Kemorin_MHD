!
!     module int_vol_magne_pre
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!!      subroutine int_vol_magne_pre_ele(ncomp_ele, d_ele, iphys_ele)
!!      subroutine int_vol_magne_pre_ele_upm                            &
!!     &         (ncomp_ele, d_ele, iphys_ele)
!
      module int_vol_magne_pre
!
      use m_precision
!
      use m_control_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
      use m_node_phys_data
      use m_fem_gauss_int_coefs
      use m_sorted_node
      use m_finite_element_matrix
      use m_jacobians
      use m_int_vol_data
      use m_physical_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_filter_elength
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
      subroutine int_vol_magne_pre_ele(ncomp_ele, d_ele, iphys_ele)
!
      use cal_add_smp
      use nodal_fld_2_each_element
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_vector_diff_type
      use fem_skv_lorentz_full_type
      use fem_skv_div_sgs_flux_type
!
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      type(phys_address), intent(in) :: iphys_ele
!
      integer(kind=kint) :: k2, num_int
!
!
!  ---------  set number of integral points
!
      num_int = intg_point_t_evo
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
!
! -------- loop for shape function for the phsical values
      do k2=1, ele1%nnod_4_ele
        call vector_phys_2_each_element(node1, ele1, nod_fld1,          &
     &      k2, iphys%i_velo, mhd_fem1_wk%velo_1)
        call vector_phys_2_each_element(node1, ele1, nod_fld1,          &
     &      k2, iphys%i_magne, mhd_fem1_wk%magne_1)
!
!$omp parallel
        call add_const_to_vector_smp                                    &
     &     (np_smp, ele1%numele, ele1%istack_ele_smp,                   &
     &      d_ele(1,iphys_ele%i_magne), ex_magne, fem1_wk%vector_1)
!$omp end parallel
!
        call fem_skv_induction_galerkin(iele_cd_smp_stack, num_int, k2, &
     &      coef_induct, mhd_fem1_wk%velo_1, mhd_fem1_wk%magne_1,       &
     &      d_ele(1,iphys_ele%i_velo),  fem1_wk%vector_1,               &
     &      ele1, jac1_3d_q, fem1_wk%sk6)
!
        if (iflag_SGS_induction .ne. id_SGS_none                        &
     &    .and. iflag_commute_induction .eq. id_SGS_commute_ON) then
           call SGS_const_induct_each_ele(node1, ele1, nod_fld1,        &
     &         k2, iphys%i_magne, iphys%i_velo, iphys%i_SGS_induct_t,   &
     &         coef_induct, mhd_fem1_wk%sgs_v1, fem1_wk%vector_1)
           call fem_skv_div_sgs_asym_tsr(iele_cd_smp_stack,             &
     &         num_int, k2, ifilter_final, ak_diff(1,iak_diff_uxb),     &
     &         ele1, jac1_3d_q, FEM1_elen, mhd_fem1_wk%sgs_v1,          &
     &         fem1_wk%vector_1, fem1_wk%sk6)
        else if (iflag_SGS_induction .ne. id_SGS_none) then
          call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1, k2,    &
     &        iphys%i_SGS_induct_t, coef_induct, mhd_fem1_wk%sgs_v1)
          call fem_skv_div_asym_tsr(iele_cd_smp_stack, num_int, k2,     &
     &        ele1, jac1_3d_q, mhd_fem1_wk%sgs_v1, fem1_wk%sk6)
        end if
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_magne_pre_ele
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_magne_pre_ele_upm                              &
     &          (ncomp_ele, d_ele, iphys_ele)
!
      use cal_add_smp
      use nodal_fld_2_each_element
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_lorentz_full_type
      use fem_skv_div_sgs_flux_upw
      use fem_skv_vect_diff_upw_type
!
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
      type(phys_address), intent(in) :: iphys_ele
!
      integer(kind=kint) :: k2, num_int
!
!  ---------  set number of integral points
!
      num_int = intg_point_t_evo
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
!
! -------- loop for shape function for the phsical values
      do k2=1, ele1%nnod_4_ele
        call vector_phys_2_each_element(node1, ele1, nod_fld1,          &
     &      k2, iphys%i_velo, mhd_fem1_wk%velo_1)
        call vector_phys_2_each_element(node1, ele1, nod_fld1,          &
     &      k2, iphys%i_magne, mhd_fem1_wk%magne_1)
!
!$omp parallel
        call add_const_to_vector_smp                                    &
     &     (np_smp, ele1%numele, ele1%istack_ele_smp,                   &
     &      d_ele(1,iphys_ele%i_magne), ex_magne, fem1_wk%vector_1)
!$omp end parallel
!
        call fem_skv_induction_upmagne(iele_cd_smp_stack, num_int, k2,  &
     &      coef_induct, mhd_fem1_wk%velo_1, mhd_fem1_wk%magne_1,       &
     &      d_ele(1,iphys_ele%i_velo), fem1_wk%vector_1,                &
     &      d_ele(1,iphys_ele%i_magne), ele1, jac1_3d_q, fem1_wk%sk6)
!
        if (iflag_SGS_induction .ne. id_SGS_none                        &
     &    .and. iflag_commute_induction .eq. id_SGS_commute_ON) then
          call SGS_const_induct_each_ele(node1, ele1, nod_fld1,         &
     &        k2, iphys%i_magne, iphys%i_velo, iphys%i_SGS_induct_t,    &
     &        coef_induct, mhd_fem1_wk%sgs_v1, fem1_wk%vector_1)
          call fem_skv_div_sgs_asym_t_upwind(iele_cd_smp_stack,         &
     &        num_int, k2, ifilter_final, ak_diff(1,iak_diff_uxb),      &
     &        ele1, jac1_3d_q, FEM1_elen, d_ele(1,iphys_ele%i_magne),   &
     &        mhd_fem1_wk%sgs_v1, fem1_wk%vector_1, fem1_wk%sk6)
        else if (iflag_SGS_induction .ne. id_SGS_none) then
          call vector_cst_phys_2_each_ele(node1, ele1, nod_fld1, k2,    &
     &        iphys%i_SGS_induct_t, coef_induct, mhd_fem1_wk%sgs_v1)
          call fem_skv_div_as_tsr_upw(iele_cd_smp_stack, num_int, k2,   &
     &        d_ele(1,iphys_ele%i_magne), ele1, jac1_3d_q,              &
     &        mhd_fem1_wk%sgs_v1, fem1_wk%sk6)
        end if
!
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_magne_pre_ele_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_magne_pre
