!>@file   set_control_sph_SGS_MHD.f90
!!@brief  module set_control_sph_SGS_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_control_4_SPH_SGS_MHD(plt, org_plt, model_ctl,   &
!!     &         smctl_ctl, smonitor_ctl, nmtr_ctl, psph_ctl, sph_gen,  &
!!     &         rj_fld, MHD_files, bc_IO, pwr, SGS_par, sph_filters,   &
!!     &         flex_p, MHD_step, MHD_prop, MHD_BC, WK_sph, gen_sph)
!!        type(platform_data_control), intent(in) :: plt
!!        type(platform_data_control), intent(in) :: org_plt
!!        type(mhd_model_control), intent(inout) :: model_ctl
!!        type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
!!        type(node_monitor_control), intent(inout) :: nmtr_ctl
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!        type(sph_grids), intent(inout) :: sph_gen
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(sph_filters_type), intent(inout) :: sph_filters(1)
!!        type(flexible_stepping_parameter), intent(inout) :: flex_p
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
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
      use t_ctl_data_SGS_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_sph_transforms
      use t_bc_data_list
      use t_flex_delta_t_data
!
      implicit none
!
      private :: set_control_sph_data_SGS_MHD
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_4_SPH_SGS_MHD(plt, org_plt, model_ctl,     &
     &         smctl_ctl, smonitor_ctl, nmtr_ctl, psph_ctl, sph_gen,    &
     &         rj_fld, MHD_files, bc_IO, pwr, SGS_par, sph_filters,     &
     &         flex_p, MHD_step, MHD_prop, MHD_BC, WK_sph, gen_sph)
!
      use sph_mhd_rms_IO
!
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_sph_filtering_data
      use t_sph_trans_arrays_MHD
      use t_const_spherical_grid
      use t_sph_boundary_input_data
!
      use m_flexible_time_step
!
      use gen_sph_grids_modes
      use set_control_platform_data
      use set_ctl_parallel_platform
      use set_control_sph_mhd
      use set_control_4_model
      use set_control_sph_data_MHD
      use set_control_4_force
      use set_control_4_normalize
      use set_control_4_time_steps
!
      use set_control_4_pickup_sph
      use set_ctl_gen_shell_grids
      use parallel_ucd_IO_select
      use set_control_4_SGS
!
      type(platform_data_control), intent(in) :: plt
      type(platform_data_control), intent(in) :: org_plt
!
      type(mhd_model_control), intent(inout) :: model_ctl
      type(sph_mhd_control_control), intent(inout) :: smctl_ctl
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
      type(node_monitor_control), intent(inout) :: nmtr_ctl
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
      type(sph_grids), intent(inout) :: sph_gen
      type(phys_data), intent(inout) :: rj_fld
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(sph_mean_squares), intent(inout) :: pwr
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(sph_filters_type), intent(inout) :: sph_filters(1)
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
      integer(kind = kint) :: ierr
!
!   set parameters for SGS model
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_SGS_model'
      call set_control_SGS_model(model_ctl%sgs_ctl,                     &
     &    SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,         &
     &    MHD_files%Csim_file_IO, MHD_files%Cdiff_file_IO,              &
     &    SGS_par%i_step_sgs_coefs)
!
      if(SGS_par%model_p%iflag_SGS .ne. id_SGS_none) then
        call set_control_SPH_SGS                                        &
     &     (model_ctl%sgs_ctl%num_sph_filter_ctl,                       &
     &      model_ctl%sgs_ctl%sph_filter_ctl(1), sph_filters(1))
      end if
      if(model_ctl%sgs_ctl%num_sph_filter_ctl .gt. 0) then
        call dealloc_sph_filter_ctl(model_ctl%sgs_ctl)
      end if
!
!
!   set parameters for data files
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call check_control_num_domains(plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_mesh_def(plt, MHD_files%mesh_file_IO)
      call set_FEM_mesh_switch_4_SPH(plt, iflag_output_mesh)
      call set_control_sph_mesh                                         &
     &   (plt, MHD_files%mesh_file_IO, MHD_files%sph_file_IO)
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
      if(psph_ctl%iflag_sph_shell .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_control_4_shell_grids'
        call set_control_4_shell_grids                                  &
     &     (nprocs, psph_ctl%spctl, psph_ctl%sdctl, sph_gen,            &
     &      gen_sph, ierr)
      end if
!
!   set forces
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_force'
      call s_set_control_4_force(model_ctl%frc_ctl, model_ctl%g_ctl,    &
     &    model_ctl%cor_ctl, model_ctl%mcv_ctl,                         &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop)
!
!   set parameters for general information
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_sph_data_SGS_MHD'
      call set_control_sph_data_SGS_MHD                                 &
     &   (SGS_par%model_p, MHD_prop, plt,                               &
     &    model_ctl%fld_ctl%field_ctl, smctl_ctl%mevo_ctl,              &
     &    MHD_files%org_rj_file_IO, MHD_files%org_rst_file_IO,          &
     &    MHD_files%fst_file_IO, rj_fld, bc_IO, WK_sph)
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_normalize'
      call s_set_control_4_normalize                                    &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    model_ctl%dless_ctl, model_ctl%eqs_ctl)
!
!   set boundary conditions
!
      call set_control_SPH_MHD_bcs                                      &
     &   (MHD_prop, MHD_BC, model_ctl%nbc_ctl, model_ctl%sbc_ctl)
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_time_steps'
      call s_set_control_4_time_steps                                   &
     &   (flex_p, MHD_step, smctl_ctl%mrst_ctl, smctl_ctl%tctl)
!
      call s_set_control_4_crank(smctl_ctl%mevo_ctl,                    &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop)
!
!   set_pickup modes
!
      call set_ctl_params_layered_spectr(smonitor_ctl%lp_ctl, pwr)
      call set_ctl_params_sph_spectr(smonitor_ctl, pwr)
!
      call set_ctl_params_pick_sph(smonitor_ctl%pspec_ctl,              &
     &    pickup_sph_head, pick_list1, pick1)
!
      call set_ctl_params_pick_gauss(smonitor_ctl%g_pwr,                &
     &    gauss_coefs_file_head, gauss_list1, gauss1)
!
      call set_ctl_params_no_heat_Nu(smonitor_ctl%Nusselt_file_prefix,  &
     &    rj_fld, Nu_type1)
!
      end subroutine set_control_4_SPH_SGS_MHD
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_sph_data_SGS_MHD                           &
     &         (SGS_param, MHD_prop, plt, field_ctl, mevo_ctl,          &
     &          rj_org_param, rst_org_param, fst_file_IO,               &
     &          rj_fld, bc_IO, WK_sph)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_file_format_switch
!
      use m_sel_spherical_SRs
      use m_FFT_selector
      use m_legendre_transform_list
!
      use t_SGS_control_parameter
      use t_ctl_data_4_platforms
      use t_read_control_arrays
      use t_ctl_data_mhd_evo_scheme
      use t_phys_data
      use t_field_data_IO
      use t_sph_transforms
      use t_control_parameter
      use t_sph_boundary_input_data
!
      use skip_comment_f
      use set_control_sph_data
      use add_nodal_fields_4_MHD
      use add_sph_MHD_fields_2_ctl
      use sph_mhd_rst_IO_control
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(platform_data_control), intent(in) :: plt
      type(ctl_array_c3), intent(inout) :: field_ctl
      type(mhd_evo_scheme_control), intent(in) :: mevo_ctl
      type(field_IO_params), intent(in) :: rj_org_param, rst_org_param
!
      type(field_IO_params), intent(inout) :: fst_file_IO
      type(phys_data), intent(inout) :: rj_fld
      type(boundary_spectra), intent(inout) :: bc_IO
      type(spherical_trns_works), intent(inout) :: WK_sph
!
      integer(kind = kint) :: ierr
!
!   overwrite restart header for magnetic field extension
!
      if( (rj_org_param%iflag_IO*rst_org_param%iflag_IO) .gt. 0)        &
     &   fst_file_IO%file_prefix = rst_org_param%file_prefix
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
        call add_field_name_4_SGS(SGS_param, field_ctl)
        call add_field_name_dynamic_SGS                                 &
     &     (SGS_param, MHD_prop%fl_prop, field_ctl)
        if (iflag_debug.eq.1) write(*,*)                                &
     &    'field_ctl%num after modified ', field_ctl%num
!
!    set nodal data
!
        if (iflag_debug.gt.0) write(*,*) 's_set_control_sph_data'
        call s_set_control_sph_data(field_ctl, rj_fld, ierr)
      end if
!
!
      if(mevo_ctl%leg_vector_len%iflag .gt. 0) then
        nvector_legendre = mevo_ctl%leg_vector_len%intvalue
      else
        nvector_legendre = 0
      end if
!      
      if(mevo_ctl%Legendre_trans_type%iflag .gt. 0) then
        WK_sph%WK_leg%id_legendre = set_legendre_trans_mode_ctl         &
     &                       (mevo_ctl%Legendre_trans_type%charavalue)
      end if
!
      if(mevo_ctl%FFT_library%iflag .gt. 0) then
        call set_fft_library_ctl(mevo_ctl%FFT_library%charavalue)
      end if
      if(mevo_ctl%import_mode%iflag .gt. 0) then
        call set_import_table_ctl(mevo_ctl%import_mode%charavalue)
      end if
      if(mevo_ctl%SR_routine%iflag .gt. 0) then
        call set_sph_comm_routine_ctl(mevo_ctl%SR_routine%charavalue)
      end if
!
      if (plt%bc_data_file_name_ctl%iflag .gt. 0) then
        bc_IO%file_name = plt%bc_data_file_name_ctl%charavalue
      end if
!
      end subroutine set_control_sph_data_SGS_MHD
!
! -----------------------------------------------------------------------
!
      end module set_control_sph_SGS_MHD
