!>@file   int_vol_coriolis.f90
!!@brief  module int_vol_coriolis
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!        modified by H. Matsui in Oct., 2005
!!        modified by H. Matsui in Aug., 2007
!!
!>@brief  Finite elememt integration for Coriolis force
!!
!!@verbatim
!!      subroutine int_vol_coriolis_pg(node, ele, fluid, fl_prop,       &
!!     &          g_FEM, jac_3d, rhs_tbl, nod_fld,                      &
!!     &          num_int, i_velo, fem_wk, f_nl)
!!      subroutine int_vol_coriolis_upw(node, ele, fluid, fl_prop,      &
!!     &          g_FEM, jac_3d, rhs_tbl, nod_fld, num_int, dt,         &
!!     &          i_velo, ncomp_ele, ie_upw, d_ele, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!@endverbatim
!
!
      module int_vol_coriolis
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_physical_property
      use t_geometry_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_table_FEM_const
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_coriolis_pg(node, ele, fluid, fl_prop,         &
     &          g_FEM, jac_3d, rhs_tbl, nod_fld,                        &
     &          num_int, i_velo, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp
      use int_vol_coriolis_term
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: num_int, i_velo
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      call int_vol_coriolis_ele                                         &
     &   (num_int, node, ele, fluid, fl_prop, g_FEM, jac_3d,            &
     &    rhs_tbl, i_velo, nod_fld, fem_wk, f_nl)
!
      end subroutine int_vol_coriolis_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_coriolis_upw(node, ele, fluid, fl_prop,        &
     &          g_FEM, jac_3d, rhs_tbl, nod_fld, num_int, dt,           &
     &          i_velo, ncomp_ele, ie_upw, d_ele, fem_wk, f_nl)
!
      use nodal_fld_cst_to_element
      use cal_skv_to_ff_smp
      use fem_skv_nonlinear_upwind
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: num_int, i_velo
!
      integer(kind = kint), intent(in) :: ncomp_ele, ie_upw
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_velo, fl_prop%coef_cor, fem_wk%vector_1)
        call fem_skv_coriolis_upwind                                    &
     &     (fluid%istack_ele_fld_smp, num_int, k2, dt,                  &
     &      fem_wk%vector_1, fl_prop%sys_rot, d_ele(1,ie_upw),          &
     &      ele, g_FEM, jac_3d, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_coriolis_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_coriolis
