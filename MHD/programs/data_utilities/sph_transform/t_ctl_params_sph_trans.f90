!t_ctl_params_sph_trans.f90
!      module t_ctl_params_sph_trans
!
!        programmed by H.Matsui on Oct., 2007
!
!!      subroutine set_control_4_sph_transform                          &
!!     &         (spt_ctl, time_STR, viz_step_STR, files_param,         &
!!     &          rj_fld, d_gauss, fem_fld, WK_sph)
!!      subroutine s_set_ctl_data_4_sph_trans                           &
!!     &         (spt_ctl, time_STR, viz_step_STR, files_param,         &
!!     &          rj_fld, d_gauss, fem_fld, WK_sph)
!!        type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!!        type(SPH_TRNS_file_IO_params), intent(inout) :: files_param
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!      subroutine set_ctl_data_4_zm_trans(fst_file_IO)
!!        type(spherical_transform_util_ctl), intent(in) :: spt_ctl
!!        type(field_IO_params), intent(inout) :: fst_file_IO
!!      subroutine set_ctl_data_4_pick_zm(spt_ctl, zm_source_file_param)
!!        type(spherical_transform_util_ctl), intent(in) :: spt_ctl
!!        type(field_IO_params), intent(inout) :: zm_source_file_param
!
      module t_ctl_params_sph_trans
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
      use t_ctl_data_4_sph_trans
!
      implicit  none
!
!
!>      Structure of file name and format for spherical transaform
      type SPH_TRNS_file_IO_params
!>        FEM mesh IO flags
        type(FEM_file_IO_flags) :: FEM_mesh_flags
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
!>        Structure of file name and format for restart file
        type(field_IO_params) :: fst_file_IO
!
!>        Structure for field data IO paramters
        type(field_IO_params) :: ucd_file_IO
!>        Structure of file name and format for spectr data file
        type(field_IO_params) :: sph_file_IO
!
!>        Structure of old spherical shell mesh file
        type(field_IO_params) :: org_rj_file_IO
!>        Structure for original restart file  paramters
        type(field_IO_params) :: org_rst_file_IO
!>        Structure for original restart file  paramters
        type(field_IO_params) :: org_ucd_file_IO
!
        type(field_IO_params) :: zm_source_file_param
        type(field_IO_params) :: zonal_ucd_param
!
        character(len = kchara) :: cmb_radial_grp =     'CMB'
        character(len = kchara) :: icb_radial_grp =     'ICB'
      end type SPH_TRNS_file_IO_params
!
!>      Increment for mean square data
      type(IO_step_param), save, private  :: rms_step_STR
!
!
      character(len = kchara) :: zm_spec_file_head = 'zm_spectral'
!      character(len = kchara) :: zonal_udt_head = 'z_mean_out'
!
      private :: zm_spec_file_head
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_sph_transform                            &
     &         (spt_ctl, time_STR, viz_step_STR, files_param,           &
     &          rj_fld, d_gauss, fem_fld, WK_sph)
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
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(time_step_param), intent(inout) :: time_STR
      type(VIZ_step_params), intent(inout) :: viz_step_STR
      type(SPH_TRNS_file_IO_params), intent(inout) :: files_param
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: fem_fld
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(global_gauss_points), intent(inout) :: d_gauss
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, spt_ctl%plt)
      call set_control_smp_def(my_rank, spt_ctl%plt)
      call set_control_sph_mesh(spt_ctl%plt, spt_ctl%Fmesh_ctl,         &
     &    files_param%mesh_file_IO, files_param%sph_file_IO,            &
     &    files_param%FEM_mesh_flags)
      call set_control_restart_file_def                                 &
     &   (spt_ctl%plt, files_param%fst_file_IO)
      call set_ucd_file_define(spt_ctl%plt, files_param%ucd_file_IO)
!
!   setting for spherical transform
!
      if(spt_ctl%legendre_vector_len_ctl%iflag .gt. 0) then
        nvector_legendre = spt_ctl%legendre_vector_len_ctl%intvalue
      else
        nvector_legendre = 0
      end if
!
      if(spt_ctl%Legendre_trans_loop_ctl%iflag .gt. 0) then
        WK_sph%WK_leg%id_legendre = set_legendre_trans_mode_ctl         &
     &                    (spt_ctl%Legendre_trans_loop_ctl%charavalue)
      end if
!
      if(spt_ctl%FFT_lib_ctl%iflag .gt. 0) then
        call set_fft_library_ctl(spt_ctl%FFT_lib_ctl%charavalue)
      end if
      if(spt_ctl%import_mode_ctl%iflag .gt. 0) then
        call set_import_table_ctl(spt_ctl%import_mode_ctl%charavalue)
      end if
!
!      stepping parameter
!
      call set_fixed_time_step_params                                   &
     &   (spt_ctl%t_ctl, time_STR, ierr, e_message)
      call viz_fixed_time_step_params                                   &
     &   (time_STR%init_d%dt, spt_ctl%t_ctl, viz_step_STR)
      call copy_delta_t(time_STR%init_d, time_STR%time_d)
!
      call set_output_step_4_fixed_step(ione, time_STR%time_d%dt,       &
     &    spt_ctl%t_ctl%i_step_check_ctl,                               &
     &    spt_ctl%t_ctl%delta_t_check_ctl, rms_step_STR)
!
!   set physical values
!
      call s_set_control_sph_data                                       &
     &   (spt_ctl%fld_ctl%field_ctl, rj_fld, ierr)
      call s_set_control_nodal_data                                     &
     &   (spt_ctl%fld_ctl%field_ctl, fem_fld, ierr)
!
      files_param%cmb_radial_grp =  'CMB'
      if(spt_ctl%cmb_radial_grp_ctl%iflag .gt. 0) then
        files_param%cmb_radial_grp                                      &
     &        = spt_ctl%cmb_radial_grp_ctl%charavalue
      end if
      files_param%icb_radial_grp = 'ICB'
      if(spt_ctl%icb_radial_grp_ctl%iflag .gt. 0) then
        files_param%icb_radial_grp                                      &
     &         = spt_ctl%icb_radial_grp_ctl%charavalue
      end if
      if(spt_ctl%gauss_sph_fhead_ctl%iflag .gt. 0) then
        d_gauss%fhead_gauss = spt_ctl%gauss_sph_fhead_ctl%charavalue
      end if
!
      call dealloc_phys_control(spt_ctl%fld_ctl)
!
      end subroutine set_control_4_sph_transform
!
! -----------------------------------------------------------------------
!
      subroutine s_set_ctl_data_4_sph_trans                             &
     &         (spt_ctl, time_STR, viz_step_STR, files_param,           &
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
      use m_default_file_prefix
      use skip_comment_f
      use parallel_ucd_IO_select
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(time_step_param), intent(inout) :: time_STR
      type(VIZ_step_params), intent(inout) :: viz_step_STR
      type(SPH_TRNS_file_IO_params), intent(inout) :: files_param
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: fem_fld
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(global_gauss_points), intent(inout) :: d_gauss
!
      integer(kind = kint) :: ierr, iflag
!
!
      call turn_off_debug_flag_by_ctl(my_rank, spt_ctl%plt)
      call set_control_smp_def(my_rank, spt_ctl%plt)
      call set_control_sph_mesh(spt_ctl%plt, spt_ctl%Fmesh_ctl,         &
     &    files_param%mesh_file_IO, files_param%sph_file_IO,            &
     &    files_param%FEM_mesh_flags)
      call set_control_restart_file_def                                 &
     &   (spt_ctl%plt, files_param%fst_file_IO)
      call set_merged_ucd_file_define                                   &
     &   (spt_ctl%plt, files_param%ucd_file_IO)
      call set_control_mesh_file_def(def_org_sph_rj_head,               &
     &    spt_ctl%org_plt, files_param%org_rj_file_IO)
      call set_control_mesh_file_def(def_org_rst_header,                &
     &    spt_ctl%org_plt, files_param%org_rst_file_IO)
      call set_control_mesh_file_def(def_org_ucd_header,                &
     &    spt_ctl%org_plt, files_param%org_ucd_file_IO)
!
!    file header for field data
!
      if(spt_ctl%zm_spec_file_head_ctl%iflag .gt. 0) then
        zm_spec_file_head = spt_ctl%zm_spec_file_head_ctl%charavalue
      end if
!
!   using rstart data for spherical dynamo
!
      iflag = files_param%org_rj_file_IO%iflag_IO                       &
     &       * files_param%org_rst_file_IO%iflag_IO
      if(iflag .gt. 0) then
        files_param%fst_file_IO%file_prefix                             &
     &       = files_param%org_rst_file_IO%file_prefix
      end if
!
!   setting for spherical transform
!
      if(spt_ctl%legendre_vector_len_ctl%iflag .gt. 0) then
        nvector_legendre = spt_ctl%legendre_vector_len_ctl%intvalue
      else
        nvector_legendre = 0
      end if
!
      if(spt_ctl%Legendre_trans_loop_ctl%iflag .gt. 0) then
        WK_sph%WK_leg%id_legendre = set_legendre_trans_mode_ctl         &
     &                    (spt_ctl%Legendre_trans_loop_ctl%charavalue)
      end if
!
      if(spt_ctl%FFT_lib_ctl%iflag .gt. 0) then
        call set_fft_library_ctl(spt_ctl%FFT_lib_ctl%charavalue)
      end if
!
!     file header for reduced data
!
      if(spt_ctl%zonal_udt_head_ctl%iflag .gt. 0) then
        files_param%zonal_ucd_param%file_prefix                         &
     &       = spt_ctl%zonal_udt_head_ctl%charavalue
      end if
      files_param%zonal_ucd_param%iflag_format                          &
     &       = files_param%ucd_file_IO%iflag_format
!
!      stepping parameter
!
      call set_fixed_time_step_params                                   &
     &   (spt_ctl%t_ctl, time_STR, ierr, e_message)
      call viz_fixed_time_step_params                                   &
     &   (time_STR%init_d%dt, spt_ctl%t_ctl, viz_step_STR)
      call copy_delta_t(time_STR%init_d, time_STR%time_d)
!
      call set_output_step_4_fixed_step(ione, time_STR%time_d%dt,       &
     &    spt_ctl%t_ctl%i_step_check_ctl,                               &
     &    spt_ctl%t_ctl%delta_t_check_ctl, rms_step_STR)
!
!   set physical values
!
      call s_set_control_sph_data                                       &
     &   (spt_ctl%fld_ctl%field_ctl, rj_fld, ierr)
      call s_set_control_nodal_data                                     &
     &   (spt_ctl%fld_ctl%field_ctl, fem_fld, ierr)
!
      files_param%cmb_radial_grp =  'CMB'
      if(spt_ctl%cmb_radial_grp_ctl%iflag .gt. 0) then
        files_param%cmb_radial_grp                                      &
     &        = spt_ctl%cmb_radial_grp_ctl%charavalue
      end if
      files_param%icb_radial_grp = 'ICB'
      if(spt_ctl%icb_radial_grp_ctl%iflag .gt. 0) then
        files_param%icb_radial_grp                                      &
     &         = spt_ctl%icb_radial_grp_ctl%charavalue
      end if
      if(spt_ctl%gauss_sph_fhead_ctl%iflag .gt. 0) then
        d_gauss%fhead_gauss = spt_ctl%gauss_sph_fhead_ctl%charavalue
      end if
!
      call dealloc_phys_control(spt_ctl%fld_ctl)
!
      end subroutine s_set_ctl_data_4_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_zm_trans(spt_ctl, fst_file_IO)
!
!
      type(spherical_transform_util_ctl), intent(in) :: spt_ctl
      type(field_IO_params), intent(inout) :: fst_file_IO
!
!
      if(spt_ctl%zm_spec_file_head_ctl%iflag .gt. 0) then
        fst_file_IO%file_prefix = zm_spec_file_head
      end if
!
      end subroutine set_ctl_data_4_zm_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_pick_zm(spt_ctl, zm_source_file_param)
!
      type(spherical_transform_util_ctl), intent(in) :: spt_ctl
      type(field_IO_params), intent(inout) :: zm_source_file_param
!
!
      if(spt_ctl%plt%field_file_prefix%iflag .eq. 0) return
      zm_source_file_param%file_prefix                                  &
     &              = spt_ctl%plt%field_file_prefix%charavalue
!
      end subroutine set_ctl_data_4_pick_zm
!
! -----------------------------------------------------------------------
!
      end module t_ctl_params_sph_trans
