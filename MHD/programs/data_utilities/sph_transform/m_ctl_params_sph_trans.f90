!m_ctl_params_sph_trans.f90
!      module m_ctl_params_sph_trans
!
!        programmed by H.Matsui on Oct., 2007
!
!!      subroutine set_control_4_sph_transform(time_STR, mesh_file,     &
!!     &          ucd_param, rj_fld, d_gauss, fem_fld, WK_sph)
!!      subroutine s_set_ctl_data_4_sph_trans                           &
!!     &         (time_STR, mesh_file, ucd_param, sph_fst_param,        &
!!     &          rj_fld, d_gauss, fem_fld, WK_sph)
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!      subroutine set_ctl_data_4_zm_trans
!!      subroutine set_ctl_data_4_pick_zm
!
      module m_ctl_params_sph_trans
!
      use m_precision
!
      use t_step_parameter
      use t_phys_data
      use t_global_gauss_coefs
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use t_IO_step_parameter
      use t_VIZ_step_parameter
      use t_sph_transforms
!
      implicit  none
!
!
!>      Increment for mean square data
      type(IO_step_param), save :: rms_step_STR
!>      Increment for visualizations
      type(VIZ_step_params), save :: viz_step_STR
!
      type(field_IO_params), save :: sph_file_trns_p
      type(field_IO_params), save :: field_file_param
      type(field_IO_params), save :: zm_source_file_param
      type(field_IO_params), save :: zonal_ucd_param
!
!>      Structure for field data IO paramters
      type(field_IO_params), save :: ucd_file_param
!>      Structure for field data IO paramters
      type(field_IO_params), save :: sph_fst_param
!>      Structure for field data IO paramters
      type(field_IO_params), save :: rj_org_param
!>      Structure for field data IO paramters
      type(field_IO_params), save :: udt_org_param
!>      Structure for original restart file  paramters
      type(field_IO_params), save :: rst_org_param
!
      character(len = kchara) :: zm_spec_file_head = 'zm_spectral'
!      character(len = kchara) :: zonal_udt_head = 'z_mean_out'
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
      subroutine set_control_4_sph_transform(time_STR, mesh_file,       &
     &          ucd_param, rj_fld, d_gauss, fem_fld, WK_sph)
!
      use calypso_mpi
      use m_FFT_selector
      use m_legendre_transform_list
!
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
      use ucd_IO_select
!
      use m_sel_spherical_SRs
      use m_ctl_data_4_sph_trans
!
      type(time_step_param), intent(inout) :: time_STR
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: ucd_param
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: fem_fld
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(global_gauss_points), intent(inout) :: d_gauss
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, st_plt)
      call set_control_smp_def(my_rank, st_plt)
      call set_control_sph_mesh(st_plt, mesh_file, sph_file_trns_p)
      call set_control_restart_file_def(st_plt, sph_fst_param)
      call set_ucd_file_define(st_plt, ucd_param)
      call copy_file_params_type(ucd_param, field_file_param)
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
        WK_sph%WK_leg%id_legendre = set_legendre_trans_mode_ctl         &
     &                       (Legendre_trans_loop_ctl%charavalue)
      end if
!
      if(FFT_lib_ctl%iflag .gt. 0) then
        call set_fft_library_ctl(FFT_lib_ctl%charavalue)
      end if
      if(import_mode_ctl%iflag .gt. 0) then
        call set_import_table_ctl(import_mode_ctl%charavalue)
      end if
!
!      stepping parameter
!
      call set_fixed_time_step_params                                   &
     &   (t_st_ctl, time_STR, ierr, e_message)
      call viz_fixed_time_step_params                                   &
     &   (time_STR%init_d%dt, t_st_ctl, viz_step_STR)
      call copy_delta_t(time_STR%init_d, time_STR%time_d)
!
      call set_output_step_4_fixed_step(ione, time_STR%time_d%dt,       &
     &    t_st_ctl%i_step_check_ctl, t_st_ctl%delta_t_check_ctl,        &
     &    rms_step_STR)
!
!   set physical values
!
      call s_set_control_sph_data(fld_st_ctl%field_ctl, rj_fld, ierr)
      call s_set_control_nodal_data                                     &
     &   (fld_st_ctl%field_ctl, fem_fld, ierr)
!
!
      cmb_radial_grp =  'CMB'
      if(cmb_radial_grp_ctl%iflag .gt. 0) then
        cmb_radial_grp = cmb_radial_grp_ctl%charavalue
      end if
      icb_radial_grp = 'ICB'
      if(icb_radial_grp_ctl%iflag .gt. 0) then
        icb_radial_grp = icb_radial_grp_ctl%charavalue
      end if
      if(gauss_sph_fhead_ctl%iflag .gt. 0) then
        d_gauss%fhead_gauss = gauss_sph_fhead_ctl%charavalue
      end if
!
      end subroutine set_control_4_sph_transform
!
! -----------------------------------------------------------------------
!
      subroutine s_set_ctl_data_4_sph_trans                             &
     &         (time_STR, mesh_file, ucd_param, sph_fst_param,          &
     &          rj_fld, d_gauss, fem_fld, WK_sph)
!
      use calypso_mpi
      use t_file_IO_parameter
      use m_machine_parameter
      use m_FFT_selector
      use m_legendre_transform_list
!
      use set_control_nodal_data
      use set_control_sph_data
      use set_control_platform_data
!
      use m_ctl_data_4_sph_trans
      use m_default_file_prefix
      use skip_comment_f
      use parallel_ucd_IO_select
!
      type(time_step_param), intent(inout) :: time_STR
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: ucd_param
      type(field_IO_params), intent(inout) :: sph_fst_param
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: fem_fld
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(global_gauss_points), intent(inout) :: d_gauss
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, st_plt)
      call set_control_smp_def(my_rank, st_plt)
      call set_control_mesh_def(st_plt, mesh_file)
      call set_control_sph_mesh(st_plt, mesh_file, sph_file_trns_p)
      call set_control_restart_file_def(st_plt, sph_fst_param)
      call set_merged_ucd_file_define(st_plt, ucd_param)
      call set_control_mesh_file_def                                    &
     &   (def_org_sph_rj_head, org_st_plt, rj_org_param)
      call set_control_mesh_file_def                                    &
     &   (def_org_rst_header, org_st_plt, rst_org_param)
      call set_control_mesh_file_def                                    &
     &   (def_org_ucd_header, org_st_plt, udt_org_param)
!
!    file header for field data
!
      if(zm_spec_file_head_ctl%iflag .gt. 0) then
        zm_spec_file_head = zm_spec_file_head_ctl%charavalue
      end if
!
!   using rstart data for spherical dynamo
!
      if( (rj_org_param%iflag_IO*rst_org_param%iflag_IO) .gt. 0) then
        sph_fst_param%file_prefix = rst_org_param%file_prefix
      end if
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
        WK_sph%WK_leg%id_legendre = set_legendre_trans_mode_ctl         &
     &                       (Legendre_trans_loop_ctl%charavalue)
      end if
!
      if(FFT_lib_ctl%iflag .gt. 0) then
        call set_fft_library_ctl(FFT_lib_ctl%charavalue)
      end if
!
!     file header for reduced data
!
      if(zonal_udt_head_ctl%iflag .gt. 0) then
        zonal_ucd_param%file_prefix = zonal_udt_head_ctl%charavalue
      end if
      zonal_ucd_param%iflag_format = ucd_param%iflag_format
!
!      stepping parameter
!
      call set_fixed_time_step_params                                   &
     &   (t_st_ctl, time_STR, ierr, e_message)
      call viz_fixed_time_step_params                                   &
     &   (time_STR%init_d%dt, t_st_ctl, viz_step_STR)
      call copy_delta_t(time_STR%init_d, time_STR%time_d)
!
      call set_output_step_4_fixed_step(ione, time_STR%time_d%dt,       &
     &    t_st_ctl%i_step_check_ctl, t_st_ctl%delta_t_check_ctl,        &
     &    rms_step_STR)
!
!   set physical values
!
      call s_set_control_sph_data(fld_st_ctl%field_ctl, rj_fld, ierr)
      call s_set_control_nodal_data                                     &
     &   (fld_st_ctl%field_ctl, fem_fld, ierr)
!
!
      cmb_radial_grp =  'CMB'
      if(cmb_radial_grp_ctl%iflag .gt. 0) then
        cmb_radial_grp = cmb_radial_grp_ctl%charavalue
      end if
      icb_radial_grp = 'ICB'
      if(icb_radial_grp_ctl%iflag .gt. 0) then
        icb_radial_grp = icb_radial_grp_ctl%charavalue
      end if
      if(gauss_sph_fhead_ctl%iflag .gt. 0) then
        d_gauss%fhead_gauss = gauss_sph_fhead_ctl%charavalue
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
      if(zm_spec_file_head_ctl%iflag .gt. 0) then
        sph_fst_param%file_prefix = zm_spec_file_head
      end if
!
      end subroutine set_ctl_data_4_zm_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_pick_zm
!
      use m_ctl_data_4_sph_trans
!
!
      if(st_plt%field_file_prefix%iflag .eq. 0) return
      zm_source_file_param%file_prefix                                  &
     &              = st_plt%field_file_prefix%charavalue
!
      end subroutine set_ctl_data_4_pick_zm
!
! -----------------------------------------------------------------------
!
      end module m_ctl_params_sph_trans
