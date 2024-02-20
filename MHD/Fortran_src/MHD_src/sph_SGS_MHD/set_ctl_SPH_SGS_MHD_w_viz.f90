!>@file   set_ctl_SPH_SGS_MHD_w_viz.f90
!!@brief  module set_ctl_SPH_SGS_MHD_w_viz
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_ctl_SPH_SGS_MHD_w_viz(model_ctl, psph_ctl,     &
!!     &          smonitor_ctl, crust_filter_ctl, nmtr_ctl,             &
!!     &          SGS_par, MHD_prop, MHD_BC, sph, rj_fld,               &
!!     &          nod_fld, monitor, nod_mntr)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(mhd_model_control), intent(inout) :: model_ctl
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        type(clust_filtering_ctl), intent(in) :: crust_filter_ctl
!!        type(parallel_sph_shell_control), intent(in) :: psph_ctl
!!        type(node_monitor_control), intent(in) :: nmtr_ctl
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(sph_grids), intent(inout) :: sph
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(node_monitor_IO), intent(inout) :: nod_mntr
!!@endverbatim
!
      module set_ctl_SPH_SGS_MHD_w_viz
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
      use t_node_monitor_IO
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_ctl_data_SGS_model
      use t_ctl_data_crust_filter
      use t_sph_grid_maker_in_sim
      use t_bc_data_list
      use t_flex_delta_t_data
      use t_field_on_circle
!
      implicit none
!
      private :: set_control_sph_sgs_mhd_fields
      private :: set_ctl_SPH_SGS_MHD_monitors
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_ctl_SPH_SGS_MHD_w_viz(model_ctl, psph_ctl,       &
     &          smonitor_ctl, crust_filter_ctl, nmtr_ctl,               &
     &          SGS_par, MHD_prop, MHD_BC, sph, rj_fld,                 &
     &          nod_fld, monitor, nod_mntr)
!
      use t_SGS_control_parameter
      use t_phys_data
      use t_sph_mhd_monitor_data_IO
!
      use set_control_SPH_MHD_w_viz
      use set_control_sph_data_MHD
      use set_control_sph_mhd
      use set_controls_4_sph_shell
      use set_field_data_w_SGS
      use set_nodal_field_name
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(mhd_model_control), intent(inout) :: model_ctl
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(clust_filtering_ctl), intent(in) :: crust_filter_ctl
      type(parallel_sph_shell_control), intent(in) :: psph_ctl
      type(node_monitor_control), intent(in) :: nmtr_ctl
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(sph_grids), intent(inout) :: sph
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(node_monitor_IO), intent(inout) :: nod_mntr
!
      integer(kind = kint) :: ierr, j
!
!       set nodal field list
      if(iflag_debug.gt.0) write(*,*) 'set_SGS_field_ctl_by_viz'
      call set_SGS_field_ctl_by_viz                                     &
     &   (model_ctl%fld_ctl%field_ctl, nod_fld, ierr)
!
!
      write(*,*) 'list of numbers: set_control_sph_sgs_mhd_fields', my_rank
      do j = 1, MHD_prop%MHD_coef_list%dimless_list%num
        write(*,*) j, trim(MHD_prop%MHD_coef_list%dimless_list%name(j)), ': ', &
     &                    MHD_prop%MHD_coef_list%dimless_list%value(j)
      end do
      call calypso_MPI_barrier
!
!       set spectr field list
      if(iflag_debug.gt.0) write(*,*) 'set_control_sph_sgs_mhd_fields'
      call set_control_sph_sgs_mhd_fields(SGS_par%model_p, MHD_prop,    &
     &    model_ctl%fld_ctl%field_ctl, rj_fld)
!
!
      write(*,*) 'list of numbers: set_ctl_SPH_SGS_MHD_monitors', my_rank
      do j = 1, MHD_prop%MHD_coef_list%dimless_list%num
        write(*,*) j, trim(MHD_prop%MHD_coef_list%dimless_list%name(j)), ': ', &
     &                    MHD_prop%MHD_coef_list%dimless_list%value(j)
      end do
      call calypso_MPI_barrier
!
!   set_pickup modes
      call set_ctl_SPH_SGS_MHD_monitors                                 &
     &   (smonitor_ctl, MHD_prop, MHD_BC, rj_fld, monitor)
!
      write(*,*) 'list of numbers: set_crustal_filtering_control', my_rank
      do j = 1, MHD_prop%MHD_coef_list%dimless_list%num
        write(*,*) j, trim(MHD_prop%MHD_coef_list%dimless_list%name(j)), ': ', &
     &                    MHD_prop%MHD_coef_list%dimless_list%value(j)
      end do
      call calypso_MPI_barrier
!
      call set_crustal_filtering_control(crust_filter_ctl, monitor)
!
      call set_FEM_mesh_mode_4_SPH(psph_ctl%spctl, sph%sph_params)
!
!
      write(*,*) 'list of numbers: set_control_node_grp_monitor', my_rank
      do j = 1, MHD_prop%MHD_coef_list%dimless_list%num
        write(*,*) j, trim(MHD_prop%MHD_coef_list%dimless_list%name(j)), ': ', &
     &                    MHD_prop%MHD_coef_list%dimless_list%value(j)
      end do
      call calypso_MPI_barrier

      call set_control_node_grp_monitor(nmtr_ctl, nod_mntr)
      call count_field_4_monitor(rj_fld, nod_mntr)
!
      end subroutine s_set_ctl_SPH_SGS_MHD_w_viz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
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
      if(iflag_debug .eq. 1) write(*,*)                                 &
     &    'original number of field ', field_ctl%num
!
      if(field_ctl%num .ne. 0) then
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
      subroutine set_ctl_SPH_SGS_MHD_monitors(smonitor_ctl, MHD_prop, &
     &                                        MHD_BC, rj_fld, monitor)
!
      use t_phys_data
      use t_sph_mhd_monitor_data_IO
      use t_no_heat_Nusselt
      use t_CMB_dipolarity
      use t_sph_typical_scales
      use t_field_on_circle
      use t_field_4_dynamobench
      use t_multi_flag_labels
      use m_file_format_labels
      use m_base_field_labels
!
      use set_control_4_pickup_sph
      use set_control_sph_spectr
      use set_ctl_sph_spectr_w_dbench
      use set_ctl_sph_lorentz_spectr
      use cal_CMB_dipolarity
      use cal_typical_scale
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(phys_data), intent(in) :: rj_fld
type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
      integer :: j
!
      if(allocated(gzip_flags%flags) .eqv. .FALSE.) then
        call init_multi_flags_by_labels(itwo, gzip_names, gzip_flags)
      end if
!
!   Set spectr monitor
      call set_ctl_params_layered_spectr(smonitor_ctl%lp_ctl,           &
     &                                   monitor%pwr)
      call s_set_ctl_sph_spectr_w_dbench(smonitor_ctl, MHD_BC,          &
     &    monitor%pwr, monitor%circ_mid_eq%circle, monitor%bench)
!   Set spectr monitor
!
      write(*,*) 'list of numbers: set_ctl_params_layer_lor_spec', my_rank
      do j = 1, MHD_prop%MHD_coef_list%dimless_list%num
        write(*,*) j, trim(MHD_prop%MHD_coef_list%dimless_list%name(j)), ': ', &
     &                    MHD_prop%MHD_coef_list%dimless_list%value(j)
      end do
      call calypso_MPI_barrier

      call set_ctl_params_layer_lor_spec(smonitor_ctl%lp_ctl,           &
     &                                   monitor%lor_spectr)
!
      write(*,*) 'list of numbers: set_ctl_params_vol_lor_spectr', my_rank
      do j = 1, MHD_prop%MHD_coef_list%dimless_list%num
        write(*,*) j, trim(MHD_prop%MHD_coef_list%dimless_list%name(j)), ': ', &
     &                    MHD_prop%MHD_coef_list%dimless_list%value(j)
      end do
      call calypso_MPI_barrier

      call set_ctl_params_vol_lor_spectr(smonitor_ctl,                  &
     &                                   monitor%lor_spectr)
!   Set parameters for dynamo benchmark output
!
      write(*,*) 'list of numbers: set_ctl_circle_for_dbench', my_rank
      do j = 1, MHD_prop%MHD_coef_list%dimless_list%num
        write(*,*) j, trim(MHD_prop%MHD_coef_list%dimless_list%name(j)), ': ', &
     &                    MHD_prop%MHD_coef_list%dimless_list%value(j)
      end do
      call calypso_MPI_barrier
      if(monitor%bench%iflag_dynamobench .gt. 0) then
        call set_ctl_circle_for_dbench(smonitor_ctl%dbench_ctl,         &
     &      monitor%circ_mid_eq%circle)
      end if
!
      write(*,*) 'list of numbers: set_ctl_params_pick_sph', my_rank
      do j = 1, MHD_prop%MHD_coef_list%dimless_list%num
        write(*,*) j, trim(MHD_prop%MHD_coef_list%dimless_list%name(j)), ': ', &
     &                    MHD_prop%MHD_coef_list%dimless_list%value(j)
      end do
      call calypso_MPI_barrier
!
!   set_pickup modes
      call set_ctl_params_pick_sph                                      &
     &   (smonitor_ctl%pspec_ctl, monitor%pick_list, monitor%pick_coef)
!
      call set_ctl_params_pick_gauss                                    &
     &   (smonitor_ctl%g_pwr, monitor%gauss_list, monitor%gauss_coef)
!
      call set_ctl_params_no_heat_Nu(heat_source%name,                  &
     &    smonitor_ctl%heat_nusselt_file_prefix,                        &
     &    smonitor_ctl%heat_nusselt_file_format,                        &
     &    rj_fld, monitor%heat_Nusselt)
      call set_ctl_params_no_heat_Nu(composition_source%name,           &
     &    smonitor_ctl%comp_nusselt_file_prefix,                        &
     &    smonitor_ctl%comp_nusselt_file_format,                        &
     &    rj_fld, monitor%comp_Nusselt)
!
      call set_ctl_dipolarity_params                                    &
     &   (smonitor_ctl%fdip_ctl%fdip_file_prefix_ctl,                   &
     &    smonitor_ctl%fdip_ctl%fdip_file_format_ctl,                   &
     &    smonitor_ctl%fdip_ctl%fdip_truncation_ctl,                    &
     &    rj_fld, monitor%dip)
      call set_ctl_typical_scale_params                                 &
     &   (smonitor_ctl%typ_scale_file_prefix_ctl,                       &
     &    smonitor_ctl%typ_scale_file_format_ctl, rj_fld, monitor%tsl)
!
      call set_control_circles_def(smonitor_ctl, monitor%mul_circle)
!
      end subroutine set_ctl_SPH_SGS_MHD_monitors
!
! ----------------------------------------------------------------------
!
      end module set_ctl_SPH_SGS_MHD_w_viz
