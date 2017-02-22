!>@file   set_control_FEM_MHD.f90
!!@brief  module set_control_FEM_MHD
!!
!!@author H. Matsui
!!@date Programmed in 2002
!
!> @brief Set parameters for MHD dynamo simulation from control data
!!
!!@verbatim
!!      subroutine set_control_4_FEM_MHD                                &
!!     &         (plt, org_plt, model_ctl, ctl_ctl, nmtr_ctl,           &
!!     &          mesh_file, udt_org_param, FEM_prm, SGS_par, nod_fld)
!!        type(platform_data_control), intent(in) :: plt
!!        type(platform_data_control), intent(in) :: org_plt
!!        type(mhd_model_control), intent(inout) :: model_ctl
!!        type(mhd_control_control), intent(inout) :: ctl_ctl
!!        type(node_monitor_control), intent(inout) :: nmtr_ctl
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(field_IO_params), intent(inout) :: udt_org_param
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module set_control_FEM_MHD
!
      use m_precision
      use m_control_parameter
      use t_phys_data
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_MHD_model
      use t_ctl_data_MHD_control
      use t_ctl_data_node_monitor
!
      implicit  none
!
      private :: set_control_FEM_MHD_bcs
      private :: set_control_rotation_form
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_FEM_MHD                                  &
     &         (plt, org_plt, model_ctl, ctl_ctl, nmtr_ctl,             &
     &          mesh_file, udt_org_param, FEM_prm, SGS_par, nod_fld)
!
      use calypso_mpi
      use m_ucd_data
      use m_default_file_prefix
      use m_physical_property
      use t_FEM_control_parameter
      use t_SGS_control_parameter
!
      use set_control_platform_data
      use set_control_nodal_data_MHD
      use set_ctl_parallel_platform
      use set_control_4_time_steps
!
      use set_control_4_force
      use set_control_4_normalize
      use set_control_4_SGS
      use set_control_4_filtering
      use set_control_4_model
      use set_control_4_scheme
      use set_control_4_solver
      use set_control_evo_layers
!
      use fem_mhd_rst_IO_control
!
      type(platform_data_control), intent(in) :: plt
      type(platform_data_control), intent(in) :: org_plt
      type(mhd_model_control), intent(inout) :: model_ctl
      type(mhd_control_control), intent(inout) :: ctl_ctl
      type(node_monitor_control), intent(inout) :: nmtr_ctl
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: udt_org_param
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(phys_data), intent(inout) :: nod_fld
!
!
!   set parameters for data files
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call check_control_num_domains(plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_mesh_def(plt, mesh_file)
      call set_ctl_restart_4_fem_mhd(plt)
      call set_control_MHD_field_file(plt)
      call set_control_mesh_file_def                                    &
     &   (def_org_ucd_header, org_plt, udt_org_param)
!
!   set parameters for general information
!
      call s_set_control_4_model                                        &
     &   (model_ctl%reft_ctl, model_ctl%refc_ctl,                       &
     &    ctl_ctl%mevo_ctl, model_ctl%evo_ctl, nmtr_ctl,                &
     &    evo_magne, evo_vect_p, evo_temp, evo_comp, fl_prop1)
!
!   set element groups for evolution
!
      call s_set_control_evo_layers(model_ctl%earea_ctl,                &
     &    evo_magne, evo_vect_p, evo_temp, evo_comp, fl_prop1, FEM_prm)
!
!   set forces
!
      call s_set_control_4_force(model_ctl%frc_ctl, model_ctl%g_ctl,    &
     &    model_ctl%cor_ctl, model_ctl%mcv_ctl, fl_prop1, cd_prop1)
      call set_control_rotation_form                                    &
     &   (ctl_ctl%mevo_ctl, fl_prop1, FEM_prm)
!
!   set parameters for SGS model
!
      call set_control_SGS_model                                        &
     &   (model_ctl%sgs_ctl, SGS_par%model_p, SGS_par%commute_p,        &
     &    SGS_par%filter_p)
      call set_control_FEM_SGS(model_ctl%sgs_ctl%ffile_ctl,             &
     &    model_ctl%sgs_ctl, model_ctl%sgs_ctl%elayer_ctl,              &
     &    SGS_par%model_p)
!
!   set parameters for filtering operation
!
      call s_set_control_4_filtering                                    &
     &   (evo_magne, evo_vect_p, evo_temp, evo_comp, fl_prop1,          &
     &    SGS_par%model_p, model_ctl%sgs_ctl%SGS_filter_name_ctl,       &
     &    model_ctl%sgs_ctl%ffile_ctl, model_ctl%sgs_ctl%s3df_ctl,      &
     &    SGS_par%filter_p)
!
!   set fields
!
      call set_control_4_fields                                         &
     &   (FEM_prm, SGS_par, model_ctl%fld_ctl%field_ctl, nod_fld)
!
!   set control parameters
!
      call s_set_control_4_normalize                                    &
     &   (evo_magne, evo_vect_p, evo_temp, evo_comp, fl_prop1,          &
     &    model_ctl%dless_ctl, model_ctl%eqs_ctl)
!
!   set boundary conditions
!
      call set_control_FEM_MHD_bcs                                      &
     &   (model_ctl%nbc_ctl, model_ctl%sbc_ctl)
!
!   set control parameters
!
      call s_set_control_4_time_steps                                   &
     &   (SGS_par%model_p, ctl_ctl%mrst_ctl, ctl_ctl%tctl)
      call s_set_control_4_crank(ctl_ctl%mevo_ctl,                      &
     &    evo_magne, evo_vect_p, evo_temp, evo_comp, fl_prop1)
!
      call s_set_control_4_solver(ctl_ctl%mevo_ctl, ctl_ctl%CG_ctl)
      call set_control_4_FEM_params(ctl_ctl%mevo_ctl, ctl_ctl%fint_ctl, &
     &    evo_magne, evo_vect_p, fl_prop1, FEM_prm)
!
      end subroutine set_control_4_FEM_MHD
!
! -----------------------------------------------------------------------
!
      subroutine set_control_FEM_MHD_bcs(nbc_ctl, sbc_ctl)
!
      use m_physical_property
      use t_ctl_data_node_boundary
      use t_ctl_data_surf_boundary
!
      use set_control_4_velo
      use set_control_4_press
      use set_control_4_temp
      use set_control_4_vect_p
      use set_control_4_magne
      use set_control_4_mag_p
      use set_control_4_current
      use set_control_4_composition
      use set_control_4_infty
!
      type(node_bc_control), intent(inout) :: nbc_ctl
      type(surf_bc_control), intent(inout) :: sbc_ctl
!
!
!   set boundary conditions for temperature
!
      call s_set_control_4_temp                                         &
     &   (evo_temp, nbc_ctl%node_bc_T_ctl, sbc_ctl%surf_bc_HF_ctl)
!
!   set boundary conditions for velocity
!
      call s_set_control_4_velo                                         &
     &   (fl_prop1, nbc_ctl%node_bc_U_ctl, sbc_ctl%surf_bc_ST_ctl)
!
!  set boundary conditions for pressure
!
      call s_set_control_4_press                                        &
     &   (fl_prop1, nbc_ctl%node_bc_P_ctl, sbc_ctl%surf_bc_PN_ctl)
!
!   set boundary conditions for composition
!
      call s_set_control_4_composition                                  &
     &   (evo_comp, nbc_ctl%node_bc_C_ctl, sbc_ctl%surf_bc_CF_ctl)
!
!   set boundary_conditons for magnetic field
!
      call s_set_control_4_magne(evo_magne, evo_vect_p,                 &
     &    nbc_ctl%node_bc_B_ctl, sbc_ctl%surf_bc_BN_ctl)
!
!   set boundary_conditons for magnetic potential
!
      call s_set_control_4_mag_p(evo_magne, evo_vect_p,                 &
     &    nbc_ctl%node_bc_MP_ctl, sbc_ctl%surf_bc_MPN_ctl)
!
!   set boundary_conditons for vector potential
!
      call s_set_control_4_vect_p(evo_vect_p,                           &
     &    nbc_ctl%node_bc_A_ctl, sbc_ctl%surf_bc_AN_ctl)
!
!   set boundary_conditons for current density
!
      call s_set_control_4_current(evo_magne, evo_vect_p,               &
     &    nbc_ctl%node_bc_J_ctl, sbc_ctl%surf_bc_JN_ctl)
!
!   set boundary_conditons for magnetic potential
!
      call s_set_control_4_infty(sbc_ctl%surf_bc_INF_ctl)
!
      end subroutine set_control_FEM_MHD_bcs
!
! ----------------------------------------------------------------------
!
      subroutine set_control_rotation_form(mevo_ctl, fl_prop, FEM_prm)
!
      use m_control_parameter
      use t_ctl_data_mhd_evolution
      use t_FEM_control_parameter
      use t_physical_property
!
      type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
      type(fluid_property), intent(in) :: fl_prop
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!
      integer (kind = kint) :: i
!
!
      if    (iflag_scheme .eq. id_explicit_euler                        &
     &  .or. iflag_scheme .eq. id_explicit_adams2) then
        FEM_prm%iflag_imp_correct = id_turn_OFF
      else
        FEM_prm%iflag_imp_correct = id_turn_On
!
        if (mevo_ctl%diffuse_correct%iflag .eq. 0) then
          FEM_prm%iflag_imp_correct = id_turn_OFF
        else
          if (yes_flag(mevo_ctl%diffuse_correct%charavalue)) then
            FEM_prm%iflag_imp_correct = iflag_scheme
          end if
        end if
      end if
!
      FEM_prm%iflag_rotate_form = id_turn_OFF
      do i = 1, fl_prop%num_force
        if(cmp_no_case(fl_prop%name_force(i),cflag_rot_form)) then
          FEM_prm%iflag_rotate_form =  id_turn_ON
          exit
        end if
      end do
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'iflag_implicit_correct ', FEM_prm%iflag_imp_correct
      end if
!
      end subroutine set_control_rotation_form
!
! ----------------------------------------------------------------------
!
      end module set_control_FEM_MHD
