!m_ctl_params_sph_utils.f90
!      module m_ctl_params_sph_utils
!
!        programmed by H.Matsui on Oct., 2007
!
!!      subroutine set_ctl_data_4_sph_utils(time_SHR, rj_fld, pwr)
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
!       Structure for time stepping parameters
      type(time_step_param), save :: t_SHR
!
      type(phys_data), save :: nod_fld
!
      type(field_IO_params), save :: sph_file_spec_p
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
      character(len = kchara) :: pickup_sph_head =  'picked_ene_spec'
      character(len = kchara) :: pickup_sph_rms_head =  'picked_ene_spec'
!
!>      Structure for pickup list for gauss coefficients
      type(pickup_mode_list), save :: gauss_list_u
!>      Structure for gauss coeffciients
!!      Radius to evaluate Gauss coefficients (Default: 6400km/2200km)
!!      gauss_u%radius_gl(1) = 2.82
      type(picked_spectrum_data), save :: gauss_u
!
!>      File prefix for Gauss coefficients file
      character(len = kchara) :: gauss_coefs_file_prefix
!
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
     &         (time_SHR, rj_fld, pwr)
!
      use calypso_mpi
      use m_machine_parameter
      use m_sph_spectr_data
      use m_file_format_switch
!
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use set_control_4_pickup_sph
!
      use m_ctl_data_4_sph_utils
      use m_default_file_prefix
!
      type(time_step_param), intent(inout) :: time_SHR
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mean_squares), intent(inout) :: pwr
!
      type(field_IO_params) :: mesh_file
      type(field_IO_params) :: rj_org_param
      type(field_IO_params) :: rst_org_param
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, su_plt)
      call set_control_smp_def(my_rank, su_plt)
      call set_control_mesh_def(su_plt, mesh_file)
      call set_control_sph_mesh(su_plt, mesh_file, sph_file_spec_p)
      call set_control_mesh_file_def                                    &
     &   (def_org_sph_rj_head, org_su_plt, rj_org_param)
      call set_control_mesh_file_def                                    &
     &   (def_org_rst_header, org_su_plt, rst_org_param)
!
!      stepping parameter
!
      call set_fixed_time_step_params                                   &
     &   (t_su_ctl, time_SHR, ierr, e_message)
      call copy_delta_t(time_SHR%init_d, time_SHR%time_d)
!
!    file header for field data
!
      if(su_plt%spectr_field_file_prefix%iflag .gt. 0) then
        call set_parallel_file_ctl_params(org_sph_file_head,            &
     &      su_plt%spectr_field_file_prefix,                            &
     &      su_plt%restart_file_fmt_ctl, spec_fst_param)
      end if
!
      if(zm_spec_file_head_ctl%iflag .gt. 0) then
        call set_parallel_file_ctl_params(zm_sph_file_head,             &
     &      zm_spec_file_head_ctl, su_plt%restart_file_fmt_ctl,         &
     &      zm_sph_fst_param)
      end if
!
!   using restart data for spherical dynamo
!
      if(su_plt%restart_file_prefix%iflag .gt. 0) then
        call set_parallel_file_ctl_params(org_sph_file_head,            &
     &      su_plt%restart_file_prefix, su_plt%restart_file_fmt_ctl,    &
     &      spec_fst_param)
        time_SHR%ucd_step%increment = time_SHR%rst_step%increment
      end if
!
      if( (rj_org_param%iflag_IO) .gt. 0) then
        call set_parallel_file_ctl_params(org_sph_file_head,            &
     &      org_su_plt%restart_file_prefix,                             &
     &      org_su_plt%sph_file_fmt_ctl, spec_fst_param)
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
      if(ene_spec_head_ctl%iflag .gt. 0) then
        ene_spec_head = ene_spec_head_ctl%charavalue
      end if
!
      if(vol_ene_spec_head_ctl%iflag .gt. 0) then
        vol_ene_spec_head = vol_ene_spec_head_ctl%charavalue
      end if
!
!   set pickup mode
!
      call set_ctl_params_layered_spectr(smonitor_u_ctl%lp_ctl, pwr)
      call set_ctl_params_sph_spectr(smonitor_u_ctl, pwr)
      call set_ctl_params_pick_sph(smonitor_u_ctl%pspec_ctl,            &
     &    pickup_sph_head, pick_list_u, pick_sph_u)
      call set_ctl_params_pick_gauss(smonitor_u_ctl%g_pwr,              &
     &    gauss_coefs_file_prefix, gauss_list_u, gauss_u)
!
!   set physical values
!
      call s_set_control_sph_data(fld_su_ctl%field_ctl, rj_fld, ierr)
      call s_set_control_nodal_data                                     &
     &   (fld_su_ctl%field_ctl, nod_fld, ierr)
!
      if(buoyancy_ratio_ctl%iflag .gt. 0) then
        buo_ratio = buoyancy_ratio_ctl%realvalue
      end if
!
      if(thermal_buoyancy_ctl%iflag .gt. 0) then
        thermal_buo = thermal_buoyancy_ctl%realvalue
      end if
!
      end subroutine set_ctl_data_4_sph_utils
!
! -----------------------------------------------------------------------
!
      end module m_ctl_params_sph_utils
