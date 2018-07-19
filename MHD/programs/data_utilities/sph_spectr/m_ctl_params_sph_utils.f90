!m_ctl_params_sph_utils.f90
!      module m_ctl_params_sph_utils
!
!        programmed by H.Matsui on Oct., 2007
!
!!      subroutine set_ctl_data_4_sph_utils                             &
!!     &         (spu_ctl, time_SHR, rj_fld, pwr)
!!        type(spherical_spectr_data_util_ctl), intent(in) :: spu_ctl
!!        type(time_step_param), intent(inout) :: time_SHR
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!
      module m_ctl_params_sph_utils
!
      use m_precision
!
      use t_step_parameter
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_pickup_sph_spectr_data
      use t_file_IO_parameter
      use t_IO_step_parameter
      use t_VIZ_step_parameter
!
      implicit  none
!
!>      Structure of file name and format for spectr data utilities
      type SPH_UTIL_file_IO_params
!>        FEM mesh IO flags
        type(FEM_file_IO_flags) :: FEM_mesh_flags
!
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
!>        Structure of file name and format for spectr data file
        type(field_IO_params) :: sph_file_IO
!
!>        Structure of old spherical shell mesh file
        type(field_IO_params) :: org_rj_file_IO
!>        Structure for original restart file  paramters
        type(field_IO_params) :: org_rst_file_IO
      end type SPH_UTIL_file_IO_params
!
!
!       Structure for time stepping parameters
      type(time_step_param), save :: t_SHR
!       Structure for time stepping parameters
      type(SPH_UTIL_file_IO_params), save :: files_SHR
!
      type(phys_data), save :: nod_fld
!
      type(field_IO_params), save :: spec_fst_param
      type(field_IO_params), save :: zm_sph_fst_param
!
!>        Structure for pickup list
      type(pickup_mode_list), save :: pick_list_u
!>        Structure for pickup list
      type(picked_spectrum_data), save :: pick_sph_u
!>        Structure for pickup list
      type(picked_spectrum_data), save :: pick_rms1
!
!>      Structure for pickup list for gauss coefficients
      type(pickup_mode_list), save :: gauss_list_u
!>      Structure for gauss coeffciients
!!      Radius to evaluate Gauss coefficients (Default: 6400km/2200km)
!!      gauss_u%radius_gl(1) = 2.82
      type(picked_spectrum_data), save :: gauss_u
!
!
      integer(kind = kint) :: iflag_org_sph_file_fmt = 0
      character(len = kchara) :: org_sph_file_head = 'spectral'
      character(len = kchara), parameter                                &
     &                        :: zm_sph_file_head = 'zm_spectral'
!
      character(len = kchara) :: ene_spec_head =     'ene_spectr'
      character(len = kchara) :: vol_ene_spec_head = 'ene_spectr_vol'
!
      character(len = kchara) :: tave_sph_file_head = 'tave_spectral'
      character(len = kchara) :: sdev_sph_file_head = 'sdev_spectral'
!
      real(kind = kreal) :: buo_ratio
      real(kind = kreal) :: thermal_buo
!
      private :: zm_sph_file_head
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_sph_utils                               &
     &         (spu_ctl, time_SHR, rj_fld, pwr)
!
      use calypso_mpi
      use m_machine_parameter
      use m_file_format_switch
!
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use set_control_4_pickup_sph
!
      use t_ctl_data_4_sph_utils
      use m_default_file_prefix
!
      type(spherical_spectr_data_util_ctl), intent(inout) :: spu_ctl
      type(time_step_param), intent(inout) :: time_SHR
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, spu_ctl%plt)
      call set_control_smp_def(my_rank, spu_ctl%plt)
      call set_control_sph_mesh(spu_ctl%plt, spu_ctl%Fmesh_ctl,         &
     &    files_SHR%mesh_file_IO, files_SHR%sph_file_IO,                &
     &    files_SHR%FEM_mesh_flags)
      call set_control_mesh_file_def(def_org_sph_rj_head,               &
     &    spu_ctl%org_plt, files_SHR%org_rj_file_IO)
      call set_control_mesh_file_def(def_org_rst_header,                &
     &    spu_ctl%org_plt, files_SHR%org_rst_file_IO)
!
!      stepping parameter
!
      call set_fixed_time_step_params                                   &
     &   (spu_ctl%tstep_ctl, time_SHR, ierr, e_message)
      call copy_delta_t(time_SHR%init_d, time_SHR%time_d)
!
!    file header for field data
!
      if(spu_ctl%plt%spectr_field_file_prefix%iflag .gt. 0) then
        call set_parallel_file_ctl_params(org_sph_file_head,            &
     &      spu_ctl%plt%spectr_field_file_prefix,                       &
     &      spu_ctl%plt%restart_file_fmt_ctl, spec_fst_param)
      end if
!
      if(spu_ctl%zm_spec_file_head_ctl%iflag .gt. 0) then
        call set_parallel_file_ctl_params(zm_sph_file_head,             &
     &      spu_ctl%zm_spec_file_head_ctl,                              &
     &      spu_ctl%plt%restart_file_fmt_ctl, zm_sph_fst_param)
      end if
!
!   using restart data for spherical dynamo
!
      if(spu_ctl%plt%restart_file_prefix%iflag .gt. 0) then
        call set_parallel_file_ctl_params(org_sph_file_head,            &
     &      spu_ctl%plt%restart_file_prefix,                            &
     &      spu_ctl%plt%restart_file_fmt_ctl, spec_fst_param)
        time_SHR%ucd_step%increment = time_SHR%rst_step%increment
      end if
!
      if( (files_SHR%org_rj_file_IO%iflag_IO) .gt. 0) then
        call set_parallel_file_ctl_params(org_sph_file_head,            &
     &      spu_ctl%org_plt%restart_file_prefix,                        &
     &      spu_ctl%org_plt%sph_file_fmt_ctl, spec_fst_param)
        time_SHR%ucd_step%increment = time_SHR%rst_step%increment
      end if
!
      write(tave_sph_file_head,'(a,a5)')                                &
     &               trim(spec_fst_param%file_prefix), '_tave'
      write(sdev_sph_file_head,'(a,a5)')                                &
     &               trim(spec_fst_param%file_prefix), '_sdev'
!
!     file header for reduced data
!
      if(spu_ctl%ene_spec_head_ctl%iflag .gt. 0) then
        ene_spec_head = spu_ctl%ene_spec_head_ctl%charavalue
      end if
!
      if(spu_ctl%vol_ene_spec_head_ctl%iflag .gt. 0) then
        vol_ene_spec_head = spu_ctl%vol_ene_spec_head_ctl%charavalue
      end if
!
!   set pickup mode
!
      call set_ctl_params_layered_spectr                                &
     &   (spu_ctl%smonitor_ctl%lp_ctl, pwr)
      call set_ctl_params_sph_spectr(spu_ctl%smonitor_ctl, pwr)
      call set_ctl_params_pick_sph(spu_ctl%smonitor_ctl%pspec_ctl,      &
     &    pick_list_u, pick_sph_u)
!
      call set_ctl_params_pick_gauss                                    &
     &   (spu_ctl%smonitor_ctl%g_pwr, gauss_list_u, gauss_u)
!
      call dealloc_sph_monitoring_ctl(spu_ctl%smonitor_ctl)
!
!   set physical values
!
      call s_set_control_sph_data                                       &
     &   (spu_ctl%fld_ctl%field_ctl, rj_fld, ierr)
      call s_set_control_nodal_data                                     &
     &   (spu_ctl%fld_ctl%field_ctl, nod_fld, ierr)
!
      if(spu_ctl%buoyancy_ratio_ctl%iflag .gt. 0) then
        buo_ratio = spu_ctl%buoyancy_ratio_ctl%realvalue
      end if
!
      if(spu_ctl%thermal_buoyancy_ctl%iflag .gt. 0) then
        thermal_buo = spu_ctl%thermal_buoyancy_ctl%realvalue
      end if
!
      call dealloc_phys_control(spu_ctl%fld_ctl)
      call dealloc_sph_monitoring_ctl(spu_ctl%smonitor_ctl)
!
      end subroutine set_ctl_data_4_sph_utils
!
! -----------------------------------------------------------------------
!
      end module m_ctl_params_sph_utils
