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
!
      character(len = kchara) :: zonal_udt_head = 'z_mean_out'
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
      use m_field_data_IO
      use m_node_id_spherical_IO
      use m_control_params_2nd_files
!
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use set_fixed_time_step_params
      use FFT_selector
      use legendre_transform_select
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
      use m_ctl_data_4_sph_trans
      use m_ctl_data_4_fields
      use m_ctl_data_4_pickup_sph
      use skip_comment_f
      use set_control_4_pickup_sph
      use output_parallel_ucd_file
!
      integer(kind = kint) :: ierr
      character(len = kchara) :: tmpchara
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_mesh_def
      call set_control_sph_mesh
      call set_control_org_sph_mesh
      call set_control_restart_file_def
      call set_control_parallel_field_def
      call set_control_org_fld_file_def
!
!    file header for field data
!
      if(i_spectr_header .gt. 0) iflag_phys_header_def = 1
!
      if(i_zm_sph_spec_file .gt. 0) then
        zm_spec_file_head = zm_spec_file_head_ctl
        iflag_phys_header_def = 1
      end if
!
!   using rstart data for spherical dynamo
!
      if(i_rst_header .gt. 0) iflag_phys_header_def = 2
!
      if( (iflag_org_sph_rj_head*iflag_org_rst_head) .gt. 0) then
        phys_file_head = org_rst_header
        iflag_phys_header_def = 2
      end if
!
!   setting for spherical transform
!
      if(i_sph_transform_mode .gt. 0) then
        call set_legendre_trans_mode_ctl(Legendre_trans_loop_ctl)
      end if
!
      if(i_FFT_package .gt. 0) then
        if(     cmp_no_case(FFT_library_ctl, 'ispack') .gt. 0) then
          iflag_FFT = iflag_ISPACK
        else if(cmp_no_case(FFT_library_ctl, 'fftpack') .gt. 0) then
          iflag_FFT = iflag_FFTPACK
        else if(cmp_no_case(FFT_library_ctl, 'fftw') .gt. 0             &
     &     .or. cmp_no_case(FFT_library_ctl, 'fftw3') .gt. 0) then
          iflag_FFT = iflag_FFTW
        end if
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
      use m_field_data_IO
!
!
      if(i_zm_sph_spec_file .gt. 0) then
        phys_file_head =    zm_spec_file_head
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
      if(i_udt_header .gt. 0) org_ucd_header = udt_file_head_ctl
!
      end subroutine set_ctl_data_4_pick_zm
!
! -----------------------------------------------------------------------
!
      end module m_ctl_params_sph_trans
