!>@file   MHD_field_by_rotation.f90
!!@brief  module MHD_field_by_rotation
!!
!!@author H. Matsui
!!@date Programmed...when??
!
!>@brief Evaluate vorticity and current density
!!
!!@verbatim
!!      subroutine cal_field_by_rotation                                &
!!     &         (nod_comm, node, ele, surf, sf_grp, fluid, conduct,    &
!!     &          iphys, iphys_ele, ele_fld, jac_3d, jac_sf_grp,        &
!!     &          rhs_tbl, FEM_elens, m_lump, mhd_fem_wk, fem_wk,       &
!!     &          f_l, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module MHD_field_by_rotation
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_field_by_rotation                                  &
     &         (nod_comm, node, ele, surf, sf_grp, fluid, conduct,      &
     &          iphys, iphys_ele, ele_fld, jac_3d, jac_sf_grp,          &
     &          rhs_tbl, FEM_elens, m_lump, mhd_fem_wk, fem_wk,         &
     &          f_l, f_nl, nod_fld)
!
      use m_machine_parameter
      use m_control_parameter
      use m_SGS_address
      use m_bc_data_velo
      use m_bc_data_magne
      use m_surf_data_torque
      use m_surf_data_magne
!
      use cal_rotation_sgs
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(iphys%i_vort .gt. izero)then
        if(nod_fld%iflag_update(iphys%i_vort) .eq. izero) then
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &        write(*,*) 'cal_vorticity'
          call choose_cal_rotation_sgs                                  &
     &       (iflag_commute_velo, iflag_velo_supg,                      &
     &        iak_diff_v, iphys%i_velo, iphys%i_vort,                   &
     &        fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,            &
     &        nod_comm, node, ele, surf, sf_grp, iphys_ele, ele_fld,    &
     &        jac_3d, jac_sf_grp, FEM_elens, Vnod1_bcs%nod_bc_w,        &
     &        Vsf1_bcs%sgs, rhs_tbl, fem_wk, f_nl, nod_fld)
        end if
      end if
!
      if(iphys%i_current .gt. izero)then
        if(nod_fld%iflag_update(iphys%i_current) .eq.0 ) then
          if(iflag_t_evo_4_vect_p .gt. id_no_evolution) then
            if (iflag_debug .ge. iflag_routine_msg)                     &
     &        write(*,*) 'cal_current_density'
              call choose_cal_rotation_sgs                              &
     &           (iflag_commute_magne, iflag_mag_supg,                  &
     &            iak_diff_b, iphys%i_magne, iphys%i_current,           &
     &            ele%istack_ele_smp, m_lump, nod_comm, node, ele,      &
     &            surf, sf_grp, iphys_ele, ele_fld, jac_3d,             &
     &            jac_sf_grp, FEM_elens, Bnod1_bcs%nod_bc_j,            &
     &            Bsf1_bcs%sgs, rhs_tbl, fem_wk, f_nl, nod_fld)
!
!             call choose_cal_rotation_sgs                              &
!     &          (iflag_commute_magne, iflag_mag_supg,                  &
!     &           iak_diff_b, iphys%i_magne, iphys%i_current,           &
!     &           conduct%istack_ele_fld_smp, mhd_fem_wk%mlump_cd,      &
!     &           nod_comm, node, ele, surf, sf_grp, iphys_ele, ele_fld,&
!     &           jac_3d,  jac_sf_grp, FEM_elens, Bnod1_bcs%nod_bc_j,   &
!     &           Bsf1_bcs%sgs, rhs_tbl, fem_wk, f_nl, nod_fld)
!             call int_current_diffuse                                  &
!     &         (nod_comm, node, ele, surf, sf_grp, Asf1_bcs,           &
!     &          iphys, jac_3d, jac_sf_grp, rhs_tbl, m_lump,            &
!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
          else
            if (iflag_debug .ge. iflag_routine_msg)                     &
     &        write(*,*) 'cal_current_density'
            call choose_cal_rotation_sgs                                &
               (iflag_commute_magne, iflag_mag_supg,                    &
     &          iak_diff_b, iphys%i_magne, iphys%i_current,             &
     &          ele%istack_ele_smp, m_lump, nod_comm, node, ele,        &
     &          surf, sf_grp, iphys_ele, ele_fld, jac_3d, jac_sf_grp,   &
     &          FEM_elens, Bnod1_bcs%nod_bc_j, Bsf1_bcs%sgs,            &
     &          rhs_tbl, fem_wk, f_nl, nod_fld)
!           call choose_cal_rotation_sgs                                &
!     &        (iflag_commute_magne, iflag_mag_supg,                    &
!     &         iak_diff_b, iphys%i_magne, iphys%i_current,             &
!     &         conduct%istack_ele_fld_smp, mhd_fem_wk%mlump_cd,        &
!     &         nod_comm, node, ele, surf, sf_grp, iphys_ele, ele_fld,  &
!     &         jac_3d, jac_sf_grp, FEM_elens, Bnod1_bcs%nod_bc_j,      &
!     &         Bsf1_bcs%sgs, rhs_tbl, fem_wk, f_nl, nod_fld)
          end if
        end if
      end if
!
      end subroutine cal_field_by_rotation
!
! ----------------------------------------------------------------------
!
      end module MHD_field_by_rotation
