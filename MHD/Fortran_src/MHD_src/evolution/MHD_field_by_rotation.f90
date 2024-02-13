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
!!     &         (dt, FEM_prm, SGS_param, cmt_param, mesh, group,       &
!!     &          fluid, conduct, cd_prop, nod_bcs, surf_bcs,           &
!!     &          iphys_base, iphys_ele_base, ele_fld, fem_int,         &
!!     &          FEM_elens, iak_diff_base, diff_coefs, mk_MHD,         &
!!     &          mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(base_field_address), intent(in) :: iak_diff_base
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module MHD_field_by_rotation
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_physical_property
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_data
      use t_phys_data
      use t_base_field_labels
      use t_table_FEM_const
      use t_jacobians
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_filter_elength
      use t_bc_data_MHD
      use t_surface_bc_data_MHD
      use t_material_property
      use t_SGS_model_coefs
      use t_work_FEM_integration
      use t_vector_for_solver
      use t_solver_SR
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
     &         (dt, FEM_prm, SGS_param, cmt_param, mesh, group,         &
     &          fluid, conduct, cd_prop, nod_bcs, surf_bcs,             &
     &          iphys_base, iphys_ele_base, ele_fld, fem_int,           &
     &          FEM_elens, iak_diff_base, diff_coefs, mk_MHD,           &
     &          mhd_fem_wk, rhs_mat, nod_fld, v_sol, SR_sig, SR_r)
!
      use cal_rotation_sgs
!
      real(kind=kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(base_field_address), intent(in) :: iak_diff_base
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(iphys_base%i_vort .gt. izero)then
        if(nod_fld%iflag_update(iphys_base%i_vort) .eq. izero) then
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &        write(*,*) 'cal_vorticity'
          call choose_cal_rotation_sgs                                  &
     &       (SGS_param%SGS_momentum%iflag_commute_field,               &
     &        FEM_prm%iflag_velo_supg, FEM_prm%npoint_t_evo_int,        &
     &        dt, iak_diff_base%i_velo,                                 &
     &        iphys_base%i_velo, iphys_base%i_vort,                     &
     &        fluid%istack_ele_fld_smp, mk_MHD%mlump_fl,                &
     &        SGS_param, mesh%nod_comm, mesh%node, mesh%ele, mesh%surf, &
     &        group%surf_grp, iphys_ele_base, ele_fld,                  &
     &        fem_int%jcs, FEM_elens, diff_coefs,                       &
     &        nod_bcs%Vnod_bcs%nod_bc_w, surf_bcs%Vsf_bcs%sgs,          &
     &        fem_int%rhs_tbl, rhs_mat%fem_wk, rhs_mat%surf_wk,         &
     &        rhs_mat%f_nl, nod_fld, v_sol, SR_sig, SR_r)
        end if
      end if
!
      if(iphys_base%i_current .gt. izero)then
        if(nod_fld%iflag_update(iphys_base%i_current) .eq.0 ) then
          if(cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
            if (iflag_debug .ge. iflag_routine_msg)                     &
     &        write(*,*) 'cal_current_density'
              call choose_cal_rotation_sgs                              &
     &          (cmt_param%iflag_c_magne, FEM_prm%iflag_magne_supg,     &
     &           FEM_prm%npoint_t_evo_int, dt, iak_diff_base%i_magne,   &
     &           iphys_base%i_magne, iphys_base%i_current,              &
     &           mesh%ele%istack_ele_smp, fem_int%m_lump, SGS_param,    &
     &           mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,         &
     &           group%surf_grp, iphys_ele_base, ele_fld, fem_int%jcs,  &
     &           FEM_elens, diff_coefs, nod_bcs%Bnod_bcs%nod_bc_j,      &
     &           surf_bcs%Bsf_bcs%sgs, fem_int%rhs_tbl, rhs_mat%fem_wk, &
     &           rhs_mat%surf_wk, rhs_mat%f_nl, nod_fld,                &
     &           v_sol, SR_sig, SR_r)
!
!             call choose_cal_rotation_sgs                              &
!     &         (cmt_param%iflag_c_magne, FEM_prm%iflag_magne_supg,     &
!     &          FEM_prm%npoint_t_evo_int, dt, iak_diff_base%i_magne,   &
!     &          iphys_base%i_magne, iphys_base%i_current,              &
!     &          conduct%istack_ele_fld_smp, mk_MHD%mlump_cd, SGS_param,&
!     &          mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,         &
!     &          group%surf_grp, iphys_ele_base, ele_fld, fem_int%jcs,  &
!     &          FEM_elens, diff_coefs, nod_bcs%Bnod_bcs%nod_bc_j,      &
!     &          surf_bcs%Bsf_bcs%sgs, fem_int%rhs_tbl,                 &
!     &          rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_nl,         &
!     &          nod_fld, v_sol, SR_sig, SR_r)
!             call int_current_diffuse                                  &
!     &         (FEM_prm, mesh%nod_comm, mesh%node, mesh%ele,           &
!     &          mesh%surf, group%surf_grp, surf_bcs%Asf_bcs,           &
!     &          iphys_base, fem_int%jcs, fem_int%rhs_tbl,              &
!     &          fem_int%m_lump, mhd_fem_wk, rhs_mat%fem_wk,            &
!     &          rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl,            &
!     &          nod_fld, v_sol, SR_sig, SR_r)
          else
            if (iflag_debug .ge. iflag_routine_msg)                     &
     &        write(*,*) 'cal_current_density'
            call choose_cal_rotation_sgs                                &
     &        (cmt_param%iflag_c_magne, FEM_prm%iflag_magne_supg,       &
     &         FEM_prm%npoint_t_evo_int, dt, iak_diff_base%i_magne,     &
     &         iphys_base%i_magne, iphys_base%i_current,                &
     &         mesh%ele%istack_ele_smp, fem_int%m_lump, SGS_param,      &
     &         mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,           &
     &         group%surf_grp, iphys_ele_base, ele_fld, fem_int%jcs,    &
     &         FEM_elens, diff_coefs,  nod_bcs%Bnod_bcs%nod_bc_j,       &
     &         surf_bcs%Bsf_bcs%sgs, fem_int%rhs_tbl, rhs_mat%fem_wk,   &
     &         rhs_mat%surf_wk, rhs_mat%f_nl, nod_fld,                  &
     &         v_sol, SR_sig, SR_r)
!           call choose_cal_rotation_sgs(cmt_param%iflag_c_magne,       &
!     &         FEM_prm%iflag_magne_supg, FEM_prm%npoint_t_evo_int,     &
!     &         dt, iak_diff_base%i_magne,                              &
!     &         iphys_base%i_magne, iphys_base%i_current,               &
!     &         conduct%istack_ele_fld_smp, mk_MHD%mlump_cd, SGS_param, &
!     &         mesh%nod_comm, mesh%node, mesh%ele, mesh%surf,          &
!     &         group%surf_grp, iphys_ele_base, ele_fld, fem_int%jcs,   &
!     &         FEM_elens, diff_coefs, nod_bcs%Bnod_bcs%nod_bc_j,       &
!     &         surf_bcs%Bsf_bcs%sgs, fem_int%rhs_tbl,                  &
!     &         rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_nl,          &
!     &         nod_fld, v_sol, SR_sig, SR_r)
          end if
        end if
      end if
!
      end subroutine cal_field_by_rotation
!
! ----------------------------------------------------------------------
!
      end module MHD_field_by_rotation
