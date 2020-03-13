!>@file   int_vol_mag_induction.f90
!!@brief  module int_vol_mag_induction
!!
!!@author H. Matsui 
!!@date Programmed by H. Matsui in 2002
!!        modified by H. Matsui in Oct., 2005
!!        modified by H. Matsui in Aug., 2007
!!
!>@brief  Finite elememt integration for magnetic induction equation
!!
!!@verbatim
!!      subroutine int_vol_mag_induct_pg(node, ele, cd_prop,            &
!!     &          g_FEM, jac_3d,rhs_tbl, nod_fld, iphys_nod, iphys_ele, &
!!     &          iele_fsmp_stack, num_int, ncomp_ele, d_ele,           &
!!     &          fem_wk, mhd_fem_wk, f_nl)
!!      subroutine int_vol_mag_induct_upm(node, ele, cd_prop,           &
!!     &          g_FEM, jac_3d, rhs_tbl, nod_fld, iphys_nod, iphys_ele,&
!!     &          iele_fsmp_stack, num_int, dt, ncomp_ele, d_ele,       &
!!     &          fem_wk, mhd_fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_nod
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!@endverbatim
!
      module int_vol_mag_induction
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_physical_property
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_fem_gauss_int_coefs
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
      subroutine int_vol_mag_induct_pg(node, ele, cd_prop,              &
     &          g_FEM, jac_3d,rhs_tbl, nod_fld, iphys_nod, iphys_ele,   &
     &          iele_fsmp_stack, num_int, ncomp_ele, d_ele,             &
     &          fem_wk, mhd_fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_induction
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(conductive_property), intent(in) :: cd_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_nod
      type(phys_address), intent(in) :: iphys_ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: num_int
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
      call add_const_to_vector_smp                                      &
     &   (ele%numele, d_ele(1,iphys_ele%base%i_magne),                  &
     &    cd_prop%ex_magne, mhd_fem_wk%magne_1)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_nod%base%i_velo, mhd_fem_wk%velo_1)
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_nod%base%i_magne, fem_wk%vector_1)
!
        call fem_skv_induction_pg                                       &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      jac_3d%ntot_int, num_int, k2, jac_3d%xjac,                  &
     &      jac_3d%an, jac_3d%dnx, mhd_fem_wk%velo_1, fem_wk%vector_1,  &
     &      d_ele(1,iphys_ele%base%i_velo), mhd_fem_wk%magne_1,         &
     &      cd_prop%coef_induct, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_mag_induct_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_mag_induct_upm(node, ele, cd_prop,             &
     &          g_FEM, jac_3d, rhs_tbl, nod_fld, iphys_nod, iphys_ele,  &
     &          iele_fsmp_stack, num_int, dt, ncomp_ele, d_ele,         &
     &          fem_wk, mhd_fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_induction
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(conductive_property), intent(in) :: cd_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_nod
      type(phys_address), intent(in) :: iphys_ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
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
      call add_const_to_vector_smp                                      &
     &   (ele%numele, d_ele(1,iphys_ele%base%i_magne),                  &
     &    cd_prop%ex_magne, mhd_fem_wk%magne_1)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_nod%base%i_velo,  mhd_fem_wk%velo_1)
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_nod%base%i_magne, fem_wk%vector_1)
!
        call fem_skv_induction_upm                                      &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      jac_3d%ntot_int, num_int, k2, dt, jac_3d%xjac,              &
     &      jac_3d%an, jac_3d%dnx, jac_3d%dnx,                          &
     &      mhd_fem_wk%velo_1, fem_wk%vector_1,                         &
     &      d_ele(1,iphys_ele%base%i_velo), mhd_fem_wk%magne_1,         &
     &      d_ele(1,iphys_ele%base%i_magne), cd_prop%coef_induct,       &
     &      fem_wk%sk6)
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
