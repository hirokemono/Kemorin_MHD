!m_ctl_params_sph_utils.f90
!      module m_ctl_params_sph_utils
!
!        programmed by H.Matsui on Oct., 2007
!
!      subroutine set_ctl_data_4_sph_utils
!
      module m_ctl_params_sph_utils
!
      use m_precision
!
      implicit  none
!
!
      character(len = kchara) :: org_sph_file_head = 'spectral'
      character(len = kchara) :: zm_sph_file_head = 'zm_spectral'
!
      character(len = kchara) :: ene_spec_head =     'ene_spectr'
      character(len = kchara) :: vol_ene_spec_head = 'ene_spectr_vol'
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
      subroutine set_ctl_data_4_sph_utils
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_t_step_parameter
      use m_read_mesh_data
      use m_ucd_data
      use m_node_phys_data
      use m_sph_spectr_data
      use m_file_format_switch
      use m_rms_4_sph_spectr
      use m_work_4_sph_trans
      use m_global_gauss_coefs
      use m_field_data_IO
      use m_node_id_spherical_IO
!
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use set_fixed_time_step_params
      use set_control_4_pickup_sph
      use set_control_4_2nd_files
      use parallel_udt_IO_select
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
      use m_ctl_data_4_sph_utils
      use m_ctl_data_4_fields
      use m_ctl_data_4_pickup_sph
!
      integer (kind = kint) :: i
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def
      call set_control_mesh_def
      call set_control_sph_mesh
      call set_control_restart_file_def
      call set_control_parallel_field_def
      call set_control_org_sph_mesh
      call set_control_org_field_file_def
!
!    file header for field data
!
      if(i_spectr_header .gt. 0) then
        org_sph_file_head = spectr_file_head_ctl
        phys_file_head =    spectr_file_head_ctl
        iflag_phys_header_def = 1
      end if
!
      if(i_zm_sph_spec_file .gt. 0) then
        zm_sph_file_head = zm_spec_file_head_ctl
        iflag_phys_header_def = 1
      end if
!
!   using rstart data for spherical dynamo
!
      if(i_rst_header .gt. 0) then
        org_sph_file_head = phys_file_head
        phys_file_head =    phys_file_head
        iflag_phys_header_def = 2
      end if
!
      if( (iflag_org_sph_rj_head*iflag_org_rst_head) .gt. 0) then
        org_sph_file_head = org_rst_header
        phys_file_head =    org_rst_header
        iflag_phys_header_def = 2
      end if
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
!
!      stepping parameter
!
      call s_set_fixed_time_step_params(ierr, e_message)
!
      if (iflag_phys_header_def .eq. 2) then
        i_step_output_ucd =   i_step_output_rst
      end if
!
!   set pickup mode
!
      call set_ctl_params_pick_sph
      call set_ctl_params_pick_gauss
!
!   set physical values
!
      call s_set_control_sph_data(ierr)
      call s_set_control_nodal_data(ierr)
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
