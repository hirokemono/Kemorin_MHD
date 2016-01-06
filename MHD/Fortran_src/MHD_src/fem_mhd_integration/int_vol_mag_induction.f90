!
!     module int_vol_mag_induction
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!!      subroutine int_vol_mag_induct_pg                                &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iphys_nod,       &
!!     &          iele_fsmp_stack, n_int, ncomp_ele, d_ele, iphys_ele,  &
!!     &          fem_wk, mhd_fem_wk, f_nl)
!!      subroutine int_vol_mag_induct_upm_t(mesh, jac_3d, rhs_tbl,      &
!!     &          nod_fld, iele_fsmp_stack, n_int, ncomp_ele, d_ele,    &
!!     &          fem_wk, mhd_fem_wk, iphys_nod, iphys_ele, f_nl)
!!      type(node_data), intent(in) :: node
!!      type(element_data), intent(in) :: ele
!!      type(jacobians_3d), intent(in) :: jac_3d
!!      type(phys_data),    intent(in) :: nod_fld
!!      type(phys_address), intent(in) :: iphys_nod
!!      type(phys_address), intent(in) :: iphys_ele
!!      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!      type(work_finite_element_mat), intent(inout) :: fem_wk
!!      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_vol_mag_induction
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
      use m_physical_property
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_mag_induct_pg(node, ele, jac_3d,               &
     &          rhs_tbl, nod_fld, iphys_nod, iphys_ele,                 &
     &          iele_fsmp_stack, n_int, ncomp_ele, d_ele,               &
     &          fem_wk, mhd_fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_lorentz_full_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_nod
      type(phys_address), intent(in) :: iphys_ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
!$omp parallel
      call add_const_to_vector_smp(np_smp, ele%numele,                  &
     &    ele%istack_ele_smp, d_ele(1,iphys_ele%i_magne),               &
     &    ex_magne, mhd_fem_wk%magne_1)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_nod%i_velo,  mhd_fem_wk%velo_1)
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_nod%i_magne, fem_wk%vector_1)
!
        call fem_skv_induction_galerkin(iele_fsmp_stack, n_int, k2,     &
     &      coef_induct, mhd_fem_wk%velo_1, fem_wk%vector_1,            &
     &      d_ele(1,iphys_ele%i_velo), mhd_fem_wk%magne_1,              &
     &      ele, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_mag_induct_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_mag_induct_upm(node, ele, jac_3d,              &
     &          rhs_tbl, nod_fld, iphys_nod, iphys_ele,                 &
     &          iele_fsmp_stack, n_int, ncomp_ele, d_ele,               &
     &          fem_wk, mhd_fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_lorentz_full_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_nod
      type(phys_address), intent(in) :: iphys_ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
!$omp parallel
      call add_const_to_vector_smp(np_smp, ele%numele,                  &
     &    ele%istack_ele_smp, d_ele(1,iphys_ele%i_magne),               &
     &    ex_magne, mhd_fem_wk%magne_1)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_nod%i_velo,  mhd_fem_wk%velo_1)
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_nod%i_magne, fem_wk%vector_1)
!
        call fem_skv_induction_upmagne(iele_fsmp_stack, n_int, k2,      &
     &      coef_induct, mhd_fem_wk%velo_1, fem_wk%vector_1,            &
     &      d_ele(1,iphys_ele%i_velo), mhd_fem_wk%magne_1,              &
     &      d_ele(1,iphys_ele%i_magne), ele, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_mag_induct_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_mag_induction
