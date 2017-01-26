!evolve_by_1st_euler.f90
!      module evolve_by_1st_euler
!
!!        programmed by H.Matsui and H.Okuda
!!                                    on July 2000 (ver 1.1)
!!        modieied by H. Matsui on Sep., 2005
!!
!!      subroutine cal_velo_pre_euler(iflag_supg, nod_comm, node, ele,  &
!!     &          fluid, fl_prop, iphys, iphys_ele, ele_fld,            &
!!     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,                  &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_magne_pre_euler(iflag_supg, i_field,             &
!!     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,     &
!!     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,                  &
!!     &          f_l, f_nl, nod_fld)
!!      subroutine cal_scalar_pre_euler(iflag_supg, i_field,            &
!!     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,       &
!!     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,                  &
!!     &          f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module evolve_by_1st_euler
!
      use m_precision
!
      use m_machine_parameter
      use m_t_int_parameter
      use m_phys_constants
!
      use t_physical_property
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_velo_pre_euler(iflag_supg, nod_comm, node, ele,    &
     &          fluid, fl_prop, iphys, iphys_ele, ele_fld,              &
     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,                    &
     &          f_l, f_nl, nod_fld)
!
      use cal_multi_pass
      use cal_sol_field_explicit
      use int_vol_coriolis_term
!
      integer(kind = kint), intent(in) :: iflag_supg
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_t_evo_4_vector(iflag_supg,                               &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_coriolis_nod_exp'
      call int_coriolis_nod_exp(node, fl_prop, mhd_fem_wk,              &
     &    iphys%i_velo, nod_fld, f_l, f_nl)
      if (iflag_debug.eq.1)  write(*,*) 'int_buoyancy_nod_exp'
      call int_buoyancy_nod_exp                                         &
     &    (node, fl_prop, mhd_fem_wk, iphys, nod_fld, f_nl)
!
      call cal_sol_vect_pre_fluid_euler                                 &
     &   (node%numnod, node%istack_internal_smp,                        &
     &    mhd_fem_wk%mlump_fl%ml, f_l%ff, f_nl%ff,                      &
     &    nod_fld%ntot_phys, n_vector, iphys%i_velo, nod_fld%d_fld)
!
      end subroutine cal_velo_pre_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_magne_pre_euler(iflag_supg, i_field,               &
     &          nod_comm, node, ele, conduct, iphys_ele, ele_fld,       &
     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,                    &
     &          f_l, f_nl, nod_fld)
!
      use cal_sol_field_explicit
      use cal_multi_pass
!
      integer(kind = kint), intent(in) :: iflag_supg, i_field
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_t_evo_4_vector_cd(iflag_supg,                            &
     &    conduct%istack_ele_fld_smp, mhd_fem_wk%mlump_cd,              &
     &    nod_comm, node, ele, iphys_ele, ele_fld, jac_3d,              &
     &    rhs_tbl, mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
      call cal_sol_vect_pre_conduct_euler                               &
     &   (node%numnod, conduct%istack_inter_fld_smp,                    &
     &    conduct%numnod_fld, conduct%inod_fld,                         &
     &    mhd_fem_wk%mlump_cd%ml, f_l%ff, f_nl%ff,                      &
     &    nod_fld%ntot_phys, n_vector, i_field, nod_fld%d_fld)
!
      end subroutine cal_magne_pre_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_scalar_pre_euler(iflag_supg, i_field,              &
     &          nod_comm, node, ele, fluid, iphys_ele, ele_fld,         &
     &          jac_3d, rhs_tbl, mhd_fem_wk, fem_wk,                    &
     &          f_l, f_nl, nod_fld)
!
      use cal_multi_pass
      use cal_sol_field_explicit
!
      integer(kind = kint), intent(in) :: iflag_supg, i_field
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_t_evo_4_scalar(iflag_supg, &
     &    fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl, nod_comm,      &
     &    node, ele, iphys_ele, ele_fld, jac_3d, rhs_tbl,               &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
      call cal_sol_vect_pre_fluid_euler                                 &
     &   (node%numnod, node%istack_internal_smp,                        &
     &    mhd_fem_wk%mlump_fl%ml, f_l%ff, f_nl%ff,                      &
     &    nod_fld%ntot_phys, n_scalar, i_field, nod_fld%d_fld)
!
      end subroutine cal_scalar_pre_euler
!
! ----------------------------------------------------------------------
!
      end module evolve_by_1st_euler
