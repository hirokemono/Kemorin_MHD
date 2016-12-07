!m_ctl_params_sph_trans.f90
!      module m_ctl_params_sph_trans
!
!        programmed by H.Matsui on Oct., 2007
!
!!      subroutine set_control_4_sph_transform(ucd, rj_fld, d_gauss)
!!      subroutine set_control_4_sph_back_trans(ucd, rj_fld, d_gauss)
!!      subroutine s_set_ctl_data_4_sph_trans(ucd, rj_fld, d_gauss)
!!        type(ucd_data), intent(inout) :: ucd
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine set_ctl_data_4_zm_trans
!!      subroutine set_ctl_data_4_pick_zm
!!      subroutine set_ctl_data_4_zm_streamline
!
      module m_ctl_params_sph_trans
!
      use m_precision
!
      use m_SPH_transforms
!
      use t_phys_data
      use t_global_gauss_coefs
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use m_read_mesh_data
!
      implicit  none
!
!
      type(field_IO_params), save :: sph_file_trns_p
      type(field_IO_params), save :: field_file_param
      type(field_IO_params), save :: zm_source_file_param
!
!>      Structure for field data IO paramters
      type(field_IO_params), save :: rj_org_param
!>      Structure for field data IO paramters
      type(field_IO_params), save :: udt_org_param
!>      Structure for original restart file  paramters
      type(field_IO_params), save :: rst_org_param
!
      character(len = kchara) :: zm_spec_file_head = 'zm_spectral'
      character(len = kchara) :: zonal_udt_head = 'z_mean_out'
!
      character(len = kchara) :: sph_rst_file_head = 'restart/rst'
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
      subroutine set_control_4_sph_transform(ucd, rj_fld, d_gauss)
!
      use t_ucd_data
      use calypso_mpi
      use m_FFT_selector
!
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use set_fixed_time_step_params
      use legendre_transform_select
      use ucd_IO_select
!
      use m_sel_spherical_SRs
      use m_ctl_data_4_sph_trans
!
      type(ucd_data), intent(inout) :: ucd
      type(phys_data), intent(inout) :: rj_fld
      type(global_gauss_points), intent(inout) :: d_gauss
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_sph_mesh(mesh1_file, sph_file_trns_p)
      call set_ucd_file_define(ucd)
      field_file_param%file_prefix =  ucd%file_prefix
      field_file_param%iflag_format = ucd%ifmt_file
!
!   setting for spherical transform
!
      if(legendre_vector_len_ctl%iflag .gt. 0) then
        nvector_legendre = legendre_vector_len_ctl%intvalue
      else
        nvector_legendre = 0
      end if
!
      if(Legendre_trans_loop_ctl%iflag .gt. 0) then
        call set_legendre_trans_mode_ctl                                &
     &     (Legendre_trans_loop_ctl%charavalue)
      end if
!
      if(FFT_library_ctl%iflag .gt. 0) then
        call set_fft_library_ctl(FFT_library_ctl%charavalue)
      end if
      if(import_mode_ctl%iflag .gt. 0) then
        call set_import_table_ctl(import_mode_ctl%charavalue)
      end if
!
      if (restart_file_prefix%iflag .gt. 0) then
        sph_rst_file_head = restart_file_prefix%charavalue
      end if
      call choose_para_file_format                                      &
     &   (restart_file_fmt_ctl, ifmt_sph_data)
!
!      stepping parameter
!
      call s_set_fixed_time_step_params(ierr, e_message)
!
!   set physical values
!
      call s_set_control_sph_data(rj_fld, ierr)
      call s_set_control_nodal_data(field_STR, ierr)
!
!
      if(i_cmb_grp .gt. 0) then
        cmb_radial_grp = cmb_radial_grp_ctl
      end if
      if(i_icb_grp .gt. 0) then
        icb_radial_grp = icb_radial_grp_ctl
      end if
      if(i_gauss_file_name .gt. 0) then
        d_gauss%fhead_gauss = gauss_sph_fhead_ctl
      end if
!
      end subroutine set_control_4_sph_transform
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_sph_back_trans(ucd, rj_fld, d_gauss)
!
      use calypso_mpi
      use t_ucd_data
!
      use m_FFT_selector
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use set_ctl_params_2nd_files
      use set_fixed_time_step_params
      use legendre_transform_select
      use ucd_IO_select
      use m_sel_spherical_SRs
!
      use m_ctl_data_4_sph_trans
!
      type(ucd_data), intent(inout) :: ucd
      type(phys_data), intent(inout) :: rj_fld
      type(global_gauss_points), intent(inout) :: d_gauss
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_sph_mesh(mesh1_file, sph_file_trns_p)
      call set_control_org_sph_mesh(rj_org_param)
      call set_ucd_file_define(ucd)
!
!   setting for spherical transform
!
      if(legendre_vector_len_ctl%iflag .gt. 0) then
        nvector_legendre = legendre_vector_len_ctl%intvalue
      else
        nvector_legendre = 0
      end if
!
      if(Legendre_trans_loop_ctl%iflag .gt. 0) then
        call set_legendre_trans_mode_ctl                                &
     &     (Legendre_trans_loop_ctl%charavalue)
      end if
!
      if(FFT_library_ctl%iflag .gt. 0) then
        call set_fft_library_ctl(FFT_library_ctl%charavalue)
      end if
      if(import_mode_ctl%iflag .gt. 0) then
        call set_import_table_ctl(import_mode_ctl%charavalue)
      end if
!
      if (restart_file_prefix%iflag .gt. 0) then
        sph_rst_file_head = restart_file_prefix%charavalue
      end if
      call choose_para_file_format                                      &
     &   (restart_file_fmt_ctl, ifmt_sph_data)
!
!      stepping parameter
!
      call s_set_fixed_time_step_params(ierr, e_message)
!
!   set physical values
!
      call s_set_control_sph_data(rj_fld, ierr)
      call s_set_control_nodal_data(field_STR, ierr)
!
!
      if(i_cmb_grp .gt. 0) then
        cmb_radial_grp = cmb_radial_grp_ctl
      end if
      if(i_icb_grp .gt. 0) then
        icb_radial_grp = icb_radial_grp_ctl
      end if
      if(i_gauss_file_name .gt. 0) then
        d_gauss%fhead_gauss = gauss_sph_fhead_ctl
      end if
!
      end subroutine set_control_4_sph_back_trans
!
! -----------------------------------------------------------------------
!
      subroutine s_set_ctl_data_4_sph_trans(ucd, rj_fld, d_gauss)
!
      use calypso_mpi
      use t_ucd_data
      use m_machine_parameter
      use m_t_step_parameter
      use m_read_mesh_data
      use m_FFT_selector
!
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use set_fixed_time_step_params
      use set_ctl_params_2nd_files
      use legendre_transform_select
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
      use m_ctl_data_4_sph_trans
      use m_ctl_data_4_fields
      use skip_comment_f
      use parallel_ucd_IO_select
!
      type(ucd_data), intent(inout) :: ucd
      type(phys_data), intent(inout) :: rj_fld
      type(global_gauss_points), intent(inout) :: d_gauss
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_mesh_def(mesh1_file)
      call set_control_sph_mesh(mesh1_file, sph_file_trns_p)
      call set_merged_ucd_file_define(ucd)
      call set_control_org_sph_mesh(rj_org_param)
      call set_control_org_rst_file_def(rst_org_param)
      call set_control_org_udt_file_def(udt_org_param)
!
!    file header for field data
!
      if(i_zm_sph_spec_file .gt. 0) then
        zm_spec_file_head = zm_spec_file_head_ctl
      end if
!
!   using rstart data for spherical dynamo
!
      if( (rj_org_param%iflag_IO*rst_org_param%iflag_IO) .gt. 0) then
        sph_rst_file_head = rst_org_param%file_prefix
      end if
!
      if (restart_file_prefix%iflag .gt. 0) then
        sph_rst_file_head = restart_file_prefix%charavalue
      end if
      call choose_para_file_format                                      &
     &   (restart_file_fmt_ctl, ifmt_sph_data)
!
!   setting for spherical transform
!
      if(legendre_vector_len_ctl%iflag .gt. 0) then
        nvector_legendre = legendre_vector_len_ctl%intvalue
      else
        nvector_legendre = 0
      end if
!
      if(Legendre_trans_loop_ctl%iflag .gt. 0) then
        call set_legendre_trans_mode_ctl                                &
     &     (Legendre_trans_loop_ctl%charavalue)
      end if
!
      if(FFT_library_ctl%iflag .gt. 0) then
        call set_fft_library_ctl(FFT_library_ctl%charavalue)
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
!   set physical values
!
      call s_set_control_sph_data(rj_fld, ierr)
      call s_set_control_nodal_data(field_STR, ierr)
!
!
      if(i_cmb_grp .gt. 0) then
        cmb_radial_grp = cmb_radial_grp_ctl
      end if
      if(i_icb_grp .gt. 0) then
        icb_radial_grp = icb_radial_grp_ctl
      end if
      if(i_gauss_file_name .gt. 0) then
        d_gauss%fhead_gauss = gauss_sph_fhead_ctl
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
        sph_rst_file_head =    zm_spec_file_head
      end if
!
      end subroutine set_ctl_data_4_zm_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_pick_zm
!
      use m_ctl_data_4_platforms
!
!
      if(udt_file_head_ctl%iflag .eq. 0) return
      zm_source_file_param%file_prefix = udt_file_head_ctl%charavalue
!
      end subroutine set_ctl_data_4_pick_zm
!
! -----------------------------------------------------------------------
!
      end module m_ctl_params_sph_trans
