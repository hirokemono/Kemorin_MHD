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
!!      subroutine int_vol_vect_p_pre_ele                               &
!!     &         (num_int, node, ele, conduct, cd_prop, iphys, nod_fld, &
!!     &          ncomp_ele, iele_magne, d_ele,                         &
!!     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_nl)
!!      subroutine int_vol_vect_p_pre_ele_upm                           &
!!     &         (num_int, dt, node, ele, conduct, cd_prop,             &
!!     &          iphys, nod_fld, ncomp_ele, iele_magne, d_ele,         &
!!     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_vect_p_pre
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_physical_property
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_phys_address
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
      subroutine int_vol_vect_p_pre_ele                                 &
     &         (num_int, node, ele, conduct, cd_prop, iphys, nod_fld,   &
     &          ncomp_ele, iele_magne, d_ele,                           &
     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_cst_to_element
      use cal_skv_to_ff_smp
      use fem_skv_nonlinear_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
!   include external magnetic field
!$omp parallel
      call add_const_to_vector_smp(ele%numele, d_ele(1,iele_magne),     &
     &    cd_prop%ex_magne, fem_wk%vector_1)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, iphys%i_velo, cd_prop%coef_induct, mhd_fem_wk%velo_1)
!
        call fem_skv_rot_inertia_type(conduct%istack_ele_fld_smp,       &
     &      num_int, k2, mhd_fem_wk%velo_1, fem_wk%vector_1,            &
     &      ele, jac_3d, fem_wk%sk6)
      end do
!
      call sub3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_vect_p_pre_ele
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_vect_p_pre_ele_upm                             &
     &         (num_int, dt, node, ele, conduct, cd_prop,               &
     &          iphys, nod_fld, ncomp_ele, iele_magne, d_ele,           &
     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_cst_to_element
      use cal_skv_to_ff_smp
      use fem_skv_nonlinear_upwind
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
!$omp parallel
      call add_const_to_vector_smp(ele%numele, d_ele(1,iele_magne),     &
     &    cd_prop%ex_magne, fem_wk%vector_1)
!$omp end parallel
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, iphys%i_velo, cd_prop%coef_induct, mhd_fem_wk%velo_1)
!
        call fem_skv_rot_inertia_upwind(conduct%istack_ele_fld_smp,     &
     &      num_int, k2, dt, mhd_fem_wk%velo_1, fem_wk%vector_1,        &
     &      d_ele(1,iele_magne), ele, jac_3d, fem_wk%sk6)
      end do
!
      call sub3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_vect_p_pre_ele_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_vect_p_pre
