!
!     module cal_stratification_by_temp
!
!        modified by H. Matsui in 2003
!        modified by H. Matsui on Aug., 2007
!
!!      subroutine cal_stratified_layer                                 &
!!     &         (node, ele, fluid, iphys, nod_fld, &
!!     &          ncomp_ele, iele_velo, d_ele, jac_3d, rhs_tbl,         &
!!     &          mhd_fem_wk, fem_wk, f_nl)
!!      subroutine cal_stratified_layer_upw                             &
!!     &         (node, ele, fluid, iphys, nod_fld,                     &
!!     &          ncomp_ele, iele_velo, d_ele, jac_3d, rhs_tbl,         &
!!     &          mhd_fem_wk, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
!*****************************************************
!
!      subroutine for set strtified layer
!       define... see Takehiro & Rister (2001)
!
!       Written by H. Matsui
!
!*****************************************************
!
!*****************************************************
!
!   Paramters
!      stratified_sigma:   intensity of stratification
!      stratified_width:   width of stratified layer
!      stratified_outer_r: radius of outer boundary of stratified layer
!
!*****************************************************
!
      module cal_stratification_by_temp
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
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
      subroutine cal_stratified_layer                                   &
     &         (node, ele, fluid, iphys, nod_fld, &
     &          ncomp_ele, iele_velo, d_ele, jac_3d, rhs_tbl,           &
     &          mhd_fem_wk, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_lorentz_full_type
      use nodal_fld_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer (kind = kint) :: k2
!
! -------- loop for shape function for the phsical values
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      do k2 = 1, ele%nnod_4_ele
!
! --------- set position at each node in an element
       call position_2_each_element(node, ele,                          &
     &     k2, mhd_fem_wk%xx_e, mhd_fem_wk%rr_e)
       call scalar_phys_2_each_element(node, ele, nod_fld,              &
     &     k2, iphys%i_gref_t, fem_wk%scalar_1)
!
        call fem_skv_stratified_galerkin(fluid%istack_ele_fld_smp,      &
     &      intg_point_t_evo, k2, fem_wk%scalar_1, d_ele(1,iele_velo),  &
     &      mhd_fem_wk%xx_e, ele, jac_3d, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine cal_stratified_layer
!
!-----------------------------------------------------------------------
!
      subroutine cal_stratified_layer_upw                               &
     &         (node, ele, fluid, iphys, nod_fld,                       &
     &          ncomp_ele, iele_velo, d_ele, jac_3d, rhs_tbl,           &
     &          mhd_fem_wk, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_lorentz_full_type
      use nodal_fld_2_each_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_velo
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
       integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
!
! --------- set position at each node in an element
        call position_2_each_element(node, ele,                         &
     &      k2, mhd_fem_wk%xx_e, mhd_fem_wk%rr_e)
        call scalar_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys%i_gref_t, fem_wk%scalar_1)
!
        call fem_skv_stratified_upwind(fluid%istack_ele_fld_smp,        &
     &      intg_point_t_evo, k2, fem_wk%scalar_1, d_ele(1,iele_velo),  &
     &      mhd_fem_wk%xx_e, ele, jac_3d, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine cal_stratified_layer_upw
!
!-----------------------------------------------------------------------
!
      end module cal_stratification_by_temp
