!>@file   MHD_field_by_rotation.f90
!!@brief  module MHD_field_by_rotation
!!
!!@author H. Matsui
!!@date Programmed...when??
!
!>@brief Evaluate vorticity and current density
!!
!!@verbatim
!!      subroutine cal_field_by_rotation(FEM_prm, SGS_param, cmt_param, &
!!     &          nod_comm, node, ele, surf, fluid, conduct,            &
!!     &          sf_grp, nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld, &
!!     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,               &
!!     &          ifld_diff, diff_coefs, m_lump, mhd_fem_wk, fem_wk,    &
!!     &          surf_wk, f_l, f_nl, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module MHD_field_by_rotation
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_control_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
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
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_bc_data_MHD
      use t_MHD_boundary_data
      use t_material_property
      use t_SGS_model_coefs
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_field_by_rotation(FEM_prm, SGS_param, cmt_param,   &
     &          nod_comm, node, ele, surf, fluid, conduct,              &
     &          sf_grp, nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld,   &
     &          jac_3d, jac_sf_grp, rhs_tbl, FEM_elens,                 &
     &          ifld_diff, diff_coefs, m_lump, mhd_fem_wk, fem_wk,      &
     &          surf_wk, f_l, f_nl, nod_fld)
!
      use cal_rotation_sgs
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(surface_group_data), intent(in) :: sf_grp
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(iphys%i_vort .gt. izero)then
        if(nod_fld%iflag_update(iphys%i_vort) .eq. izero) then
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &        write(*,*) 'cal_vorticity'
          call choose_cal_rotation_sgs(cmt_param%iflag_c_velo,          &
     &       FEM_prm%iflag_velo_supg, intg_point_t_evo,                 &
     &       ifld_diff%i_velo, iphys%i_velo, iphys%i_vort,              &
     &       fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,             &
     &       SGS_param, nod_comm, node, ele, surf, sf_grp,              &
     &       iphys_ele, ele_fld, jac_3d, jac_sf_grp, FEM_elens,         &
     &       diff_coefs, nod_bcs%Vnod_bcs%nod_bc_w,                     &
     &       surf_bcs%Vsf_bcs%sgs, rhs_tbl, fem_wk, surf_wk,            &
     &       f_nl, nod_fld)
        end if
      end if
!
      if(iphys%i_current .gt. izero)then
        if(nod_fld%iflag_update(iphys%i_current) .eq.0 ) then
          if(evo_vect_p%iflag_scheme .gt. id_no_evolution) then
            if (iflag_debug .ge. iflag_routine_msg)                     &
     &        write(*,*) 'cal_current_density'
              call choose_cal_rotation_sgs(cmt_param%iflag_c_magne,     &
     &            FEM_prm%iflag_magne_supg, intg_point_t_evo,           &
     &            ifld_diff%i_magne, iphys%i_magne, iphys%i_current,    &
     &            ele%istack_ele_smp, m_lump, SGS_param,                &
     &            nod_comm, node, ele, surf, sf_grp, iphys_ele,         &
     &            ele_fld, jac_3d, jac_sf_grp, FEM_elens, diff_coefs,   &
     &            nod_bcs%Bnod_bcs%nod_bc_j, surf_bcs%Bsf_bcs%sgs,      &
     &            rhs_tbl, fem_wk, surf_wk, f_nl, nod_fld)
!
!             call choose_cal_rotation_sgs(cmt_param%iflag_c_magne,     &
!     &           FEM_prm%iflag_magne_supg, intg_point_t_evo,           &
!     &           ifld_diff%i_magne, iphys%i_magne, iphys%i_current,    &
!     &           conduct%istack_ele_fld_smp, mhd_fem_wk%mlump_cd,      &
!     &           SGS_param, nod_comm, node, ele, surf, sf_grp,         &
!     &           iphys_ele, ele_fld, jac_3d, jac_sf_grp, FEM_elens,    &
!     &           diff_coefs, nod_bcs%Bnod_bcs%nod_bc_j,                &
!     &           surf_bcs%Bsf_bcs%sgs, rhs_tbl, fem_wk, surf_wk,       &
!     &           f_nl, nod_fld)
!             call int_current_diffuse                                  &
!     &         (FEM_prm, nod_comm, node, ele, surf, sf_grp,            &
!     &          surf_bcs%Asf_bcs, iphys, jac_3d, jac_sf_grp, rhs_tbl,  &
!     &          m_lump, mhd_fem_wk, fem_wk, surf_wk,                   &
!     &          f_l, f_nl, nod_fld)
          else
            if (iflag_debug .ge. iflag_routine_msg)                     &
     &        write(*,*) 'cal_current_density'
            call choose_cal_rotation_sgs                                &
               (cmt_param%iflag_c_magne, FEM_prm%iflag_magne_supg,      &
     &          intg_point_t_evo, ifld_diff%i_magne,                    &
     &          iphys%i_magne, iphys%i_current, ele%istack_ele_smp,     &
     &          m_lump, SGS_param, nod_comm, node, ele, surf,           &
     &          sf_grp, iphys_ele, ele_fld, jac_3d, jac_sf_grp,         &
     &          FEM_elens, diff_coefs, nod_bcs%Bnod_bcs%nod_bc_j,       &
     &          surf_bcs%Bsf_bcs%sgs, rhs_tbl, fem_wk, surf_wk,         &
     &          f_nl, nod_fld)
!           call choose_cal_rotation_sgs(cmt_param%iflag_c_magne,       &
!     &         FEM_prm%iflag_magne_supg, intg_point_t_evo,             &
!     &         ifld_diff%i_magne, iphys%i_magne, iphys%i_current,      &
!     &         conduct%istack_ele_fld_smp, mhd_fem_wk%mlump_cd,        &
!     &         SGS_param, nod_comm, node, ele, surf, sf_grp,           &
!     &         iphys_ele, ele_fld, jac_3d, jac_sf_grp, FEM_elens,      &
!     &         diff_coefs, nod_bcs%Bnod_bcs%nod_bc_j,                  &
!     &         surf_bcs%Bsf_bcs%sgs, rhs_tbl, fem_wk, surf_wk,         &
!     &         f_nl, nod_fld)
          end if
        end if
      end if
!
      end subroutine cal_field_by_rotation
!
! ----------------------------------------------------------------------
!
      end module MHD_field_by_rotation
