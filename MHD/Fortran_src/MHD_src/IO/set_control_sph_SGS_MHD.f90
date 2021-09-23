!>@file   set_control_sph_SGS_MHD.f90
!!@brief  module set_control_sph_SGS_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_control_SGS_SPH_MHD_field                        &
!!     &         (model_ctl, psph_ctl, smonitor_ctl, zm_ctls,           &
!!     &          SGS_par, MHD_prop, sph, rj_fld, nod_fld, monitor)
!!      subroutine set_control_4_SPH_SGS_MHD(plt, org_plt,              &
!!     &          model_ctl, smctl_ctl, nmtr_ctl, psph_ctl, MHD_files,  &
!!     &          bc_IO, SGS_par, dynamic_SPH, MHD_step, MHD_prop,      &
!!     &          MHD_BC, trans_p, WK, sph_maker)
!!        type(platform_data_control), intent(in) :: plt
!!        type(platform_data_control), intent(in) :: org_plt
!!        type(mhd_model_control), intent(in) :: model_ctl
!!        type(sph_mhd_control_control), intent(in) :: smctl_ctl
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
!!        type(node_monitor_control), intent(in) :: nmtr_ctl
!!        type(parallel_sph_shell_control), intent(in) :: psph_ctl
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!
!!      subroutine set_ctl_params_pick_circle                           &
!!     &         (field_ctl, meq_ctl, circle, d_circle)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!        type(mid_equator_control), intent(in) :: meq_ctl
!!        type(fields_on_circle), intent(inout) :: circle
!!        type(phys_data), intent(inout) :: d_circle
!!@endverbatim
!
      module set_control_sph_SGS_MHD
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_field_data_IO
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_SGS_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_control_data_dynamo_vizs
      use t_sph_grid_maker_in_sim
      use t_bc_data_list
      use t_flex_delta_t_data
!
      implicit none
!
      private :: set_control_sph_sgs_mhd_fields
      private :: set_control_SGS_SPH_MHD
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_SGS_SPH_MHD_field                          &
     &         (model_ctl,  psph_ctl, smonitor_ctl, zm_ctls,            &
     &          SGS_par, MHD_prop, sph, rj_fld, nod_fld, monitor)
!
      use t_SGS_control_parameter
      use t_phys_data
      use t_sph_mhd_monitor_data_IO
!
      use set_control_sph_data_MHD
      use set_control_sph_mhd
      use set_controls_4_sph_shell
      use set_field_data_w_SGS
      use set_nodal_field_name
      use node_monitor_IO
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(mhd_model_control), intent(inout) :: model_ctl
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(sph_dynamo_viz_controls), intent(in) :: zm_ctls
      type(parallel_sph_shell_control), intent(in) :: psph_ctl
      type(sph_grids), intent(inout) :: sph
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
      integer(kind = kint) :: ierr
!
!
!       set nodal field list
      if(iflag_debug.gt.0) write(*,*) 'set_SGS_field_ctl_by_viz'
      call set_SGS_field_ctl_by_viz                                     &
     &   (model_ctl%fld_ctl%field_ctl, nod_fld, ierr)
!
!       set spectr field list
      if(iflag_debug.gt.0) write(*,*) 'set_control_sph_sgs_mhd_fields'
      call set_control_sph_sgs_mhd_fields(SGS_par%model_p, MHD_prop,    &
     &    model_ctl%fld_ctl%field_ctl, rj_fld)
!
!   set_pickup modes
      call set_control_SPH_MHD_monitors(smonitor_ctl, rj_fld, monitor)
!
      call set_crustal_filtering_control                                &
     &   (zm_ctls%crust_filter_ctl, monitor)
!
!
      call set_FEM_mesh_mode_4_SPH(psph_ctl%spctl, sph%sph_params)
!
      call count_field_4_monitor                                        &
     &   (rj_fld, num_field_monitor, ntot_comp_monitor)
!
      end subroutine set_control_SGS_SPH_MHD_field
!
! ----------------------------------------------------------------------
!
      subroutine set_control_4_SPH_SGS_MHD(plt, org_plt,                &
     &          model_ctl, smctl_ctl, nmtr_ctl, psph_ctl, MHD_files,    &
     &          bc_IO, SGS_par, dynamic_SPH, MHD_step, MHD_prop,        &
     &          MHD_BC, trans_p, WK, sph_maker)
!
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use t_sph_filtering
      use t_sph_trans_arrays_MHD
      use t_const_spherical_grid
      use t_sph_boundary_input_data
      use t_ctl_params_gen_sph_shell
      use t_SPH_mesh_field_data
      use t_sph_trans_arrays_MHD
!
      use set_control_4_SGS
      use set_control_SGS_commute
      use set_control_sph_data_MHD
      use set_control_sph_mhd
      use set_control_sph_filter
      use set_field_data_w_SGS
!
      type(platform_data_control), intent(in) :: plt
      type(platform_data_control), intent(in) :: org_plt
!
      type(mhd_model_control), intent(in) :: model_ctl
      type(sph_mhd_control_control), intent(in) :: smctl_ctl
      type(node_monitor_control), intent(in) :: nmtr_ctl
      type(parallel_sph_shell_control), intent(in) :: psph_ctl
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
!   set parameters for SGS model
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_SGS_model'
      call set_control_SGS_model                                        &
     &   (model_ctl%sgs_ctl, SGS_par%model_p, SGS_par%filter_p,         &
     &    MHD_files%Csim_file_IO, SGS_par%i_step_sgs_coefs)
      call s_set_control_SGS_commute                                    &
     &   (SGS_par%model_p, model_ctl%sgs_ctl, SGS_par%commute_p,        &
     &    MHD_files%Cdiff_file_IO)
!
      call set_control_SPH_SGS_filters                                  &
     &   (model_ctl%sgs_ctl, SGS_par%model_p, dynamic_SPH)
!
!   set parameters for data files
!
      call set_control_SGS_SPH_MHD(plt, org_plt,                        &
     &    model_ctl, smctl_ctl, nmtr_ctl, psph_ctl, MHD_files,          &
     &    bc_IO, MHD_step, MHD_prop, MHD_BC, trans_p, WK, sph_maker)
!
      end subroutine set_control_4_SPH_SGS_MHD
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_sph_sgs_mhd_fields                         &
     &         (SGS_param, MHD_prop, field_ctl, rj_fld)
!
      use m_error_IDs
      use m_machine_parameter
!
      use t_control_parameter
      use t_SGS_control_parameter
      use t_control_array_character3
      use t_phys_data
!
      use add_nodal_fields_4_MHD
      use add_sph_MHD_fields_2_ctl
      use add_sph_SGS_MHD_fld_2_ctl
      use add_sph_filter_force_2_ctl
      use set_field_data_w_SGS
      use add_dependency_for_SGS
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ierr
!
!
!   set physical values
!
      if(field_ctl%icou .eq. 0) then
        call calypso_MPI_abort(ierr_fld, 'Set field for simulation')
      end if
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'original number of field ', field_ctl%num
!
      if ( field_ctl%num .ne. 0 ) then
!
!     add fields for simulation
        call add_field_name_4_mhd(MHD_prop, field_ctl)
        call add_field_name_4_sph_mhd                                   &
     &     (MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop, field_ctl)
        call add_filter_force_4_sph_mhd                                 &
     &     (MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop, field_ctl)
!
        call add_field_name_4_SGS(SGS_param, field_ctl)
        call add_field_name_dynamic_SGS                                 &
     &     (SGS_param, MHD_prop%fl_prop, field_ctl)
!
        call add_dependent_SGS_field(SGS_param, field_ctl)
        call add_dependent_field(MHD_prop, field_ctl)
!
        if (iflag_debug.eq.1) write(*,*)                                &
     &    'field_ctl%num after modified ', field_ctl%num
!
!    set nodal data
!
        if (iflag_debug.gt.0) write(*,*) 'set_SGS_field_ctl_by_viz'
        call set_SGS_field_ctl_by_viz(field_ctl, rj_fld, ierr)
      end if
!
      end subroutine set_control_sph_sgs_mhd_fields
!
! -----------------------------------------------------------------------
!
      subroutine set_control_SGS_SPH_MHD(plt, org_plt,                  &
     &          model_ctl, smctl_ctl, nmtr_ctl, psph_ctl, MHD_files,    &
     &          bc_IO, MHD_step, MHD_prop, MHD_BC, trans_p, WK,         &
     &          sph_maker)
!
      use t_spheric_parameter
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_sph_trans_arrays_MHD
      use t_const_spherical_grid
      use t_sph_boundary_input_data
      use t_ctl_params_gen_sph_shell
      use t_SPH_mesh_field_data
      use t_work_4_sph_trans
      use t_sph_trans_arrays_MHD
!
      use gen_sph_grids_modes
      use set_control_platform_item
      use set_control_platform_data
      use set_ctl_parallel_platform
      use set_control_4_model
      use set_control_sph_data_MHD
      use set_control_4_force
      use set_control_4_normalize
      use set_control_sph_mhd
      use set_ctl_4_shell_grids
!
      use set_control_4_pickup_sph
      use parallel_ucd_IO_select
!
      type(platform_data_control), intent(in) :: plt
      type(platform_data_control), intent(in) :: org_plt
!
      type(mhd_model_control), intent(in) :: model_ctl
      type(sph_mhd_control_control), intent(in) :: smctl_ctl
      type(node_monitor_control), intent(in) :: nmtr_ctl
      type(parallel_sph_shell_control), intent(in) :: psph_ctl
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
      integer(kind = kint) :: ierr
!
!
!   set parameters for data files
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call check_control_num_domains(plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_sph_mesh(plt, psph_ctl%Fmesh_ctl,                &
     &    MHD_files%sph_file_param, MHD_files%mesh_file_IO,             &
     &    MHD_files%sph_file_IO, MHD_files%FEM_mesh_flags)
      call set_control_restart_file_def(plt, MHD_files%fst_file_IO)
      call set_merged_ucd_file_define(plt, MHD_files%ucd_file_IO)
      call set_control_org_sph_files(org_plt, MHD_files)
!
      call s_set_control_4_model                                        &
     &    (model_ctl%reft_ctl, model_ctl%refc_ctl,                      &
     &     smctl_ctl%mevo_ctl, model_ctl%evo_ctl, nmtr_ctl, MHD_prop)
!
!   set spherical shell parameters
!
      call set_ctl_4_sph_grid_maker(nprocs, psph_ctl,                   &
     &    plt%sph_file_prefix, MHD_files%sph_file_param,                &
     &    sph_maker, ierr)
!
!   set forces
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_force'
      call s_set_control_4_force(model_ctl%frc_ctl, model_ctl%g_ctl,    &
     &    model_ctl%cor_ctl, model_ctl%mcv_ctl, MHD_prop)
!
!   set parameters for general information
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_sph_data_MHD'
      call s_set_control_sph_data_MHD(plt, smctl_ctl%mevo_ctl,          &
     &    MHD_files%org_rj_file_IO, MHD_files%org_rst_file_IO,          &
     &    MHD_files%fst_file_IO, bc_IO, trans_p, WK%WK_leg)
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_normalize'
      call s_set_control_4_normalize                                    &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop, MHD_prop%ht_prop,         &
     &    MHD_prop%cp_prop, model_ctl%dless_ctl, model_ctl%eqs_ctl,     &
     &    MHD_prop%MHD_coef_list)
!
!   set boundary conditions
!
      call set_control_SPH_MHD_bcs                                      &
     &   (MHD_prop, model_ctl%nbc_ctl, model_ctl%sbc_ctl, MHD_BC)
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_time_steps'
      call s_set_control_4_time_steps                                   &
     &   (smctl_ctl%mrst_ctl, smctl_ctl%tctl, MHD_step)
!
      call s_set_control_4_crank(smctl_ctl%mevo_ctl,                    &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop)
!
      end subroutine set_control_SGS_SPH_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_pick_circle                             &
     &         (field_ctl, meq_ctl, circle, d_circle)
!
      use t_ctl_data_sph_vol_spectr
      use t_mid_equator_control
      use t_control_array_character3
      use t_circle_transform
      use t_phys_data
      use set_field_data_w_SGS
      use skip_comment_f
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(mid_equator_control), intent(in) :: meq_ctl
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
      character(len = kchara) :: tmpchara
      integer(kind = kint) :: ierr = 0
!
!
      circle%iflag_circle_coord = iflag_circle_sph
      if (meq_ctl%pick_circle_coord_ctl%iflag .ne. 0) then
        tmpchara = meq_ctl%pick_circle_coord_ctl%charavalue
        if(    cmp_no_case(tmpchara,'spherical')                        &
     &    .or. cmp_no_case(tmpchara,'rtp')) then
          circle%iflag_circle_coord = iflag_circle_sph
        else if(cmp_no_case(tmpchara,'cyrindrical')                     &
      &    .or. cmp_no_case(tmpchara,'spz')) then
          circle%iflag_circle_coord = iflag_circle_cyl
        end if
      end if
!
      circle%mphi_circle = -1
      if(meq_ctl%nphi_mid_eq_ctl%iflag .gt. 0) then
        circle%mphi_circle = meq_ctl%nphi_mid_eq_ctl%intvalue
      end if
!
      circle%s_circle = 7.0d0/13.0d0 + 0.5d0
      if(meq_ctl%pick_s_ctl%iflag .gt. 0) then
        circle%s_circle = meq_ctl%pick_s_ctl%realvalue
      end if
!
      circle%z_circle = 0.0d0
      if(meq_ctl%pick_z_ctl%iflag .gt. 0) then
        circle%z_circle = meq_ctl%pick_z_ctl%realvalue
      end if
!
      call set_SGS_field_ctl_by_viz(field_ctl, d_circle, ierr)
!
      end subroutine set_ctl_params_pick_circle
!
! -----------------------------------------------------------------------
!
      end module set_control_sph_SGS_MHD
