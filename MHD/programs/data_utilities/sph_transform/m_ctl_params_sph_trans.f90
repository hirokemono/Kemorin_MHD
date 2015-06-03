!m_ctl_params_sph_trans.f90
!      module m_ctl_params_sph_trans
!
!        programmed by H.Matsui on Oct., 2007
!
!      subroutine s_set_ctl_data_4_sph_trans
!      subroutine set_ctl_data_4_zm_trans
!      subroutine set_ctl_data_4_pick_zm
!      subroutine set_ctl_data_4_zm_streamline
!
      module m_ctl_params_sph_trans
!
      use m_precision
!
      implicit  none
!
!
      character(len = kchara) :: zm_spec_file_head = 'zm_spectral'
      character(len = kchara) :: zonal_udt_head = 'z_mean_out'
!
      character(len = kchara) :: sph_file_head = 'restart/rst'
      integer(kind = kint) ::    ifmt_sph_data = 0
!
      character(len = kchara) :: cmb_radial_grp =     'CMB'
      character(len = kchara) :: icb_radial_grp =     'ICB'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_sph_transform
!
      use calypso_mpi
      use m_global_gauss_coefs
      use m_ucd_data
      use m_FFT_selector
!
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use set_fixed_time_step_params
      use legendre_transform_select
!
      use m_ctl_data_4_sph_trans
      use set_control_4_pickup_sph
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_sph_mesh
      call set_control_ucd_file_def
!
!   setting for spherical transform
!
      if(i_legendre_vect_len .gt. 0) then
        nvector_legendre = legendre_vector_len_ctl
      else
        nvector_legendre = 0
      end if
!
      if(i_sph_transform_mode .gt. 0) then
        call set_legendre_trans_mode_ctl(Legendre_trans_loop_ctl)
      end if
!
      if(i_FFT_package .gt. 0) then
        call set_fft_library_ctl(FFT_library_ctl)
      end if
!
      if (restart_file_prefix%iflag .gt. 0) then
        sph_file_head = restart_file_prefix%charavalue
      end if
      call choose_file_format(restart_file_fmt_ctl, ifmt_sph_data)
!
!      stepping parameter
!
      call s_set_fixed_time_step_params(ierr, e_message)
!
!   set pickup mode
!
      call set_ctl_params_pick_sph
!
!   set physical values
!
      call s_set_control_sph_data(ierr)
      call s_set_control_nodal_data(ierr)
!
!
      if(i_cmb_grp .gt. 0) then
        cmb_radial_grp = cmb_radial_grp_ctl
      end if
      if(i_icb_grp .gt. 0) then
        icb_radial_grp = icb_radial_grp_ctl
      end if
      if(i_gauss_file_name .gt. 0) then
        fhead_gauss = gauss_sph_fhead_ctl
      end if
!
      end subroutine set_control_4_sph_transform
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_sph_back_trans
!
      use calypso_mpi
      use m_global_gauss_coefs
      use m_ucd_data
!
      use m_control_params_2nd_files
      use m_FFT_selector
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use set_fixed_time_step_params
      use legendre_transform_select
!
      use m_ctl_data_4_sph_trans
      use set_control_4_pickup_sph
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_sph_mesh
      call set_control_org_sph_mesh
      call set_control_ucd_file_def
!
!   setting for spherical transform
!
      if(i_legendre_vect_len .gt. 0) then
        nvector_legendre = legendre_vector_len_ctl
      else
        nvector_legendre = 0
      end if
!
      if(i_sph_transform_mode .gt. 0) then
        call set_legendre_trans_mode_ctl(Legendre_trans_loop_ctl)
      end if
!
      if(i_FFT_package .gt. 0) then
        call set_fft_library_ctl(FFT_library_ctl)
      end if
!
      if (restart_file_prefix%iflag .gt. 0) then
        sph_file_head = restart_file_prefix%charavalue
      end if
      call choose_file_format(restart_file_fmt_ctl, ifmt_sph_data)
!
!      stepping parameter
!
      call s_set_fixed_time_step_params(ierr, e_message)
!
!   set pickup mode
!
      call set_ctl_params_pick_sph
!
!   set physical values
!
      call s_set_control_sph_data(ierr)
      call s_set_control_nodal_data(ierr)
!
!
      if(i_cmb_grp .gt. 0) then
        cmb_radial_grp = cmb_radial_grp_ctl
      end if
      if(i_icb_grp .gt. 0) then
        icb_radial_grp = icb_radial_grp_ctl
      end if
      if(i_gauss_file_name .gt. 0) then
        fhead_gauss = gauss_sph_fhead_ctl
      end if
!
      end subroutine set_control_4_sph_back_trans
!
! -----------------------------------------------------------------------
!
      subroutine s_set_ctl_data_4_sph_trans
!
      use calypso_mpi
      use m_machine_parameter
      use m_t_step_parameter
      use m_read_mesh_data
      use m_node_phys_data
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_work_4_sph_trans
      use m_global_gauss_coefs
      use m_node_id_spherical_IO
      use m_control_params_2nd_files
      use m_FFT_selector
!
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use set_fixed_time_step_params
      use legendre_transform_select
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
      use m_ctl_data_4_sph_trans
      use m_ctl_data_4_fields
      use m_ctl_data_4_pickup_sph
      use m_control_params_2nd_files
      use skip_comment_f
      use set_control_4_pickup_sph
      use output_parallel_ucd_file
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_mesh_def
      call set_control_sph_mesh
      call set_control_org_sph_mesh
      call set_control_parallel_field_def
      call set_control_org_fld_file_def
!
!    file header for field data
!
      if(i_zm_sph_spec_file .gt. 0) then
        zm_spec_file_head = zm_spec_file_head_ctl
      end if
!
!   using rstart data for spherical dynamo
!
      if( (iflag_org_sph_rj_head*iflag_org_rst) .gt. 0) then
        sph_file_head = org_rst_header
      end if
!
      if (restart_file_prefix%iflag .gt. 0) then
        sph_file_head = restart_file_prefix%charavalue
      end if
      call choose_file_format(restart_file_fmt_ctl, ifmt_sph_data)
!
!   setting for spherical transform
!
      if(i_legendre_vect_len .gt. 0) then
        nvector_legendre = legendre_vector_len_ctl
      else
        nvector_legendre = 0
      end if
!
      if(i_sph_transform_mode .gt. 0) then
        call set_legendre_trans_mode_ctl(Legendre_trans_loop_ctl)
      end if
!
      if(i_FFT_package .gt. 0) then
        call set_fft_library_ctl(FFT_library_ctl)
      end if
!
!     file header for reduced data
!
      if(i_zm_field_file .gt. 0) then
        zonal_udt_head = zonal_udt_head_ctl
      end if
!
!      stepping parameter
!
      call s_set_fixed_time_step_params(ierr, e_message)
!
!   set pickup mode
!
      call set_ctl_params_pick_sph
!
!   set physical values
!
      call s_set_control_sph_data(ierr)
      call s_set_control_nodal_data(ierr)
!
!
      if(i_cmb_grp .gt. 0) then
        cmb_radial_grp = cmb_radial_grp_ctl
      end if
      if(i_icb_grp .gt. 0) then
        icb_radial_grp = icb_radial_grp_ctl
      end if
      if(i_gauss_file_name .gt. 0) then
        fhead_gauss = gauss_sph_fhead_ctl
      end if
!
      end subroutine s_set_ctl_data_4_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_zm_trans
!
      use m_ctl_data_4_sph_trans
!
!
      if(i_zm_sph_spec_file .gt. 0) then
        sph_file_head =    zm_spec_file_head
      end if
!
      end subroutine set_ctl_data_4_zm_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_pick_zm
!
      use m_ctl_data_4_platforms
      use m_control_params_2nd_files
!
!
      if(udt_file_head_ctl%iflag .eq. 0) return
      org_ucd_header = udt_file_head_ctl%charavalue
!
      end subroutine set_ctl_data_4_pick_zm
!
! -----------------------------------------------------------------------
!
      end module m_ctl_params_sph_trans
