!m_ctl_params_sph_utils.f90
!      module m_ctl_params_sph_utils
!
!        programmed by H.Matsui on Oct., 2007
!
!!      subroutine set_ctl_data_4_sph_utils(rj_fld, pwr)
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!
      module m_ctl_params_sph_utils
!
      use m_precision
!
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_pickup_sph_spectr_data
!
      implicit  none
!
!
      type(phys_data), save :: nod_fld
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
!!      gauss_u%radius_gl(1) = 2.91
      type(picked_spectrum_data), save :: gauss_u
!
!>      File prefix for Gauss coefficients file
      character(len = kchara) :: gauss_coefs_file_prefix
!
!
!
      integer(kind = kint) :: iflag_org_sph_file_fmt = 0
      character(len = kchara) :: org_sph_file_head = 'spectral'
      character(len = kchara) :: zm_sph_file_head = 'zm_spectral'
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_sph_utils(rj_fld, pwr)
!
      use calypso_mpi
      use m_machine_parameter
      use m_t_step_parameter
      use m_read_mesh_data
      use m_sph_spectr_data
      use m_file_format_switch
      use m_node_id_spherical_IO
      use m_control_params_2nd_files
!
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use set_fixed_time_step_params
      use set_control_4_pickup_sph
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
      use m_ctl_data_4_sph_utils
      use m_ctl_data_4_fields
      use m_ctl_data_4_pickup_sph
      use m_ctl_data_4_org_data
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_mesh_def
      call set_control_sph_mesh
      call set_control_org_sph_mesh
      call set_control_org_fld_file_def
!
!      stepping parameter
!
      call s_set_fixed_time_step_params(ierr, e_message)
!
!    file header for field data
!
      if(spectr_file_head_ctl%iflag .gt. 0) then
        org_sph_file_head =  spectr_file_head_ctl%charavalue
        call choose_para_file_format                                    &
     &     (restart_file_fmt_ctl, iflag_org_sph_file_fmt)
      end if
!
      if(i_zm_sph_spec_file .gt. 0) then
        zm_sph_file_head = zm_spec_file_head_ctl
      end if
!
!   using restart data for spherical dynamo
!
      if(restart_file_prefix%iflag .gt. 0) then
        org_sph_file_head =  restart_file_prefix%charavalue
        call choose_para_file_format                                    &
     &     (restart_file_fmt_ctl, iflag_org_sph_file_fmt)
        i_step_output_ucd =   i_step_output_rst
      end if
!
      if( (iflag_org_sph_rj_head) .gt. 0) then
        org_sph_file_head =  org_rst_header
        i_step_output_ucd =  i_step_output_rst
        call choose_file_format                                         &
     &     (org_sph_file_fmt_ctl, iflag_org_sph_file_fmt)
      end if
!
      write(tave_sph_file_head,'(a,a5)')                                &
     &                     trim(org_sph_file_head), '_tave'
      write(sdev_sph_file_head,'(a,a5)')                                &
     &                     trim(org_sph_file_head), '_sdev'
!
!     file header for reduced data
!
      if(i_ene_spec_head .gt. 0) then
        ene_spec_head = ene_spec_head_ctl
      end if
!
      if(i_vol_ene_spec_head .gt. 0) then
        vol_ene_spec_head = vol_ene_spec_head_ctl
      end if
!   set pickup mode
!
      call set_ctl_params_pick_sph                                      &
     &   (pwr, pickup_sph_head, pick_list_u, pick_sph_u)
      call set_ctl_params_pick_gauss                                    &
     &   (gauss_coefs_file_prefix, gauss_list_u, gauss_u)
!
!   set physical values
!
      call s_set_control_sph_data(rj_fld, ierr)
      call s_set_control_nodal_data(nod_fld, ierr)
!
      if(i_buo_ratio .gt. 0) then
        buo_ratio = buoyancy_ratio_ctl
      end if
!
      if(i_thermal_buo .gt. 0) then
        thermal_buo = thermal_buoyancy_ctl
      end if
!
      end subroutine set_ctl_data_4_sph_utils
!
! -----------------------------------------------------------------------
!
      end module m_ctl_params_sph_utils
