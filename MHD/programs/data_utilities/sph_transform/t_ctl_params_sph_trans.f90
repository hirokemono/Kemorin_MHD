!t_ctl_params_sph_trans.f90
!      module t_ctl_params_sph_trans
!
!        programmed by H.Matsui on Oct., 2007
!
!!      subroutine set_control_4_sph_transform(spt_ctl, time_STR,       &
!!     &          SPH_TRNS, FEM_STR, SPH_STR)
!!      subroutine s_set_ctl_data_4_sph_trans(spt_ctl, time_STR,        &
!!     &          SPH_TRNS, FEM_STR, SPH_STR)
!!      subroutine set_ctl_data_4_zm_trans(spt_ctl, SPH_STR)
!!      subroutine set_ctl_data_4_pick_zm(spt_ctl, zm_source_file_param)
!!        type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!!        type(time_step_param), intent(inout) :: time_STR
!!        type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!!        type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_TRNS
!
      module t_ctl_params_sph_trans
!
      use m_precision
!
      use t_step_parameter
      use t_global_gauss_coefs
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use t_IO_step_parameter
      use t_VIZ_step_parameter
      use t_legendre_trans_select
      use t_ctl_data_4_sph_trans
      use t_work_4_sph_trans
      use t_FEM_data_4_SPH_trans
      use t_SPH_data_4_SPH_trans
      use t_ctl_params_gen_sph_shell
      use t_sph_grid_maker_in_sim
      use t_SPH_mesh_field_data
!
      implicit  none
!
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
      subroutine set_control_4_sph_transform(spt_ctl, time_STR,         &
     &          SPH_TRNS, FEM_STR, SPH_STR)
!
      use t_work_4_sph_trans
!
      use calypso_mpi
      use m_FFT_selector
      use m_legendre_transform_list
!
      use set_field_data_w_SGS
      use set_control_platform_item
      use set_control_platform_data
      use set_ctl_4_shell_grids
      use parallel_ucd_IO_select
!
      use sel_spherical_SRs
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(time_step_param), intent(inout) :: time_STR
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(SPH_mesh_field_data), intent(inout) :: SPH_TRNS
!
      integer(kind = kint) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank, spt_ctl%plt)
      call set_control_smp_def(my_rank, spt_ctl%plt)
      call set_control_sph_mesh(spt_ctl%plt, spt_ctl%Fmesh_ctl,         &
     &    SPH_STR%sph_file_param, FEM_STR%mesh_file_IO,                 &
     &    SPH_STR%sph_file_IO, SPH_STR%FEM_mesh_flags)
      call set_control_restart_file_def                                 &
     &   (spt_ctl%plt, SPH_STR%fst_file_IO)
      call set_merged_ucd_file_define(spt_ctl%plt,                      &
     &                                FEM_STR%ucd_file_IO)
!
!   setting for spherical transform
!
      if(spt_ctl%legendre_vector_len_ctl%iflag .gt. 0) then
        SPH_STR%trans_p%nvector_legendre                                &
     &           = spt_ctl%legendre_vector_len_ctl%intvalue
      else
        SPH_STR%trans_p%nvector_legendre = 0
      end if
!
      if(spt_ctl%Legendre_trans_loop_ctl%iflag .gt. 0) then
        SPH_STR%WK_leg%id_legendre                                      &
     &    = set_legendre_trans_mode_ctl                                 &
     &    (spt_ctl%Legendre_trans_loop_ctl%charavalue)
      end if
!
      SPH_STR%trans_p%iflag_FFT                                         &
     &     = set_fft_library_ctl(spt_ctl%FFT_lib_ctl%iflag,             &
     &                           spt_ctl%FFT_lib_ctl%charavalue)
      if(spt_ctl%import_mode_ctl%iflag .gt. 0) then
        call set_import_table_ctl                                       &
     &    (spt_ctl%import_mode_ctl%charavalue, SPH_STR%trans_p)
      end if
!
!      stepping parameter
!
      call set_fixed_time_step_params                                   &
     &   (spt_ctl%t_ctl, time_STR, ierr, e_message)
      call viz_fixed_time_step_params                                   &
     &   (time_STR%init_d%dt, spt_ctl%t_ctl, FEM_STR%viz_step)
      call copy_delta_t(time_STR%init_d, time_STR%time_d)
!
      call output_step_4_fixed_step_ctl(ione, time_STR%time_d%dt,       &
     &    spt_ctl%t_ctl%i_step_check_ctl,                               &
     &    spt_ctl%t_ctl%delta_t_check_ctl, rms_step_STR)
!
!   set physical values
!
      call set_SGS_field_ctl_by_viz                                     &
     &   (spt_ctl%fld_ctl%field_ctl, SPH_TRNS%fld, ierr)
      call set_SGS_field_ctl_by_viz                                     &
     &   (spt_ctl%fld_ctl%field_ctl, FEM_STR%field, ierr)
!
      SPH_STR%cmb_radial_grp =  'CMB'
      if(spt_ctl%cmb_radial_grp_ctl%iflag .gt. 0) then
        SPH_STR%cmb_radial_grp                                          &
     &     = spt_ctl%cmb_radial_grp_ctl%charavalue
      end if
      SPH_STR%icb_radial_grp = 'ICB'
      if(spt_ctl%icb_radial_grp_ctl%iflag .gt. 0) then
        SPH_STR%icb_radial_grp                                          &
     &     = spt_ctl%icb_radial_grp_ctl%charavalue
      end if
!
      call set_ctl_4_global_gauss_coefs(spt_ctl%gauss_sph_fhead_ctl,    &
     &                                  SPH_STR%d_gauss)
!
!   set spherical shell parameters
      call set_ctl_4_sph_grid_maker(nprocs, spt_ctl%psph_ctl,           &
     &    spt_ctl%plt%sph_file_prefix, SPH_STR%sph_file_param,          &
     &    SPH_TRNS%sph_maker, ierr)
      call dealloc_phys_control(spt_ctl%fld_ctl)
!
      end subroutine set_control_4_sph_transform
!
! -----------------------------------------------------------------------
!
      subroutine s_set_ctl_data_4_sph_trans(spt_ctl, time_STR,          &
     &          SPH_TRNS, FEM_STR, SPH_STR)
!
      use calypso_mpi
      use t_file_IO_parameter
!
      use m_machine_parameter
      use m_FFT_selector
      use m_legendre_transform_list
!
      use set_field_data_w_SGS
      use set_control_platform_item
      use set_control_platform_data
!
      use m_default_file_prefix
      use skip_comment_f
      use parallel_ucd_IO_select
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(time_step_param), intent(inout) :: time_STR
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(SPH_mesh_field_data), intent(inout) :: SPH_TRNS
!
      integer(kind = kint) :: ierr, iflag
!
!
      call turn_off_debug_flag_by_ctl(my_rank, spt_ctl%plt)
      call set_control_smp_def(my_rank, spt_ctl%plt)
      call set_control_sph_mesh(spt_ctl%plt, spt_ctl%Fmesh_ctl,         &
     &    SPH_STR%sph_file_param, FEM_STR%mesh_file_IO,                 &
     &    SPH_STR%sph_file_IO, SPH_STR%FEM_mesh_flags)
      call set_control_restart_file_def                                 &
     &   (spt_ctl%plt, SPH_STR%fst_file_IO)
      call set_merged_ucd_file_define(spt_ctl%plt, FEM_STR%ucd_file_IO)
      call set_control_mesh_file_def(def_org_sph_rj_head,               &
     &    spt_ctl%org_plt, SPH_STR%org_rj_file_IO)
      call set_control_mesh_file_def(def_org_rst_header,                &
     &    spt_ctl%org_plt, SPH_STR%org_rst_file_IO)
      call set_control_mesh_file_def(def_org_ucd_header,                &
     &    spt_ctl%org_plt, FEM_STR%org_ucd_file_IO)
!
!    file header for field data
!
      if(spt_ctl%zm_spec_file_head_ctl%iflag .gt. 0) then
        zm_spec_file_head = spt_ctl%zm_spec_file_head_ctl%charavalue
      end if
!
!   using rstart data for spherical dynamo
!
      iflag = SPH_STR%org_rj_file_IO%iflag_IO                           &
     &       * SPH_STR%org_rst_file_IO%iflag_IO
      if(iflag .gt. 0) then
        SPH_STR%fst_file_IO%file_prefix                                 &
     &       = SPH_STR%org_rst_file_IO%file_prefix
      end if
!
!   setting for spherical transform
!
      if(spt_ctl%legendre_vector_len_ctl%iflag .gt. 0) then
        SPH_STR%trans_p%nvector_legendre                                &
     &          = spt_ctl%legendre_vector_len_ctl%intvalue
      else
        SPH_STR%trans_p%nvector_legendre = 0
      end if
!
      if(spt_ctl%Legendre_trans_loop_ctl%iflag .gt. 0) then
        SPH_STR%WK_leg%id_legendre                                      &
     &        = set_legendre_trans_mode_ctl                             &
     &        (spt_ctl%Legendre_trans_loop_ctl%charavalue)
      end if
!
      SPH_STR%trans_p%iflag_FFT                                         &
     &     = set_fft_library_ctl(spt_ctl%FFT_lib_ctl%iflag,             &
     &                           spt_ctl%FFT_lib_ctl%charavalue)
!
!     file header for reduced data
!
      if(spt_ctl%zonal_udt_head_ctl%iflag .gt. 0) then
        FEM_STR%zonal_ucd_param%file_prefix                             &
     &       = spt_ctl%zonal_udt_head_ctl%charavalue
      end if
      FEM_STR%zonal_ucd_param%iflag_format                              &
     &       = FEM_STR%ucd_file_IO%iflag_format
!
!      stepping parameter
!
      call set_fixed_time_step_params                                   &
     &   (spt_ctl%t_ctl, time_STR, ierr, e_message)
      call viz_fixed_time_step_params                                   &
     &   (time_STR%init_d%dt, spt_ctl%t_ctl, FEM_STR%viz_step)
      call copy_delta_t(time_STR%init_d, time_STR%time_d)
!
      call output_step_4_fixed_step_ctl(ione, time_STR%time_d%dt,       &
     &    spt_ctl%t_ctl%i_step_check_ctl,                               &
     &    spt_ctl%t_ctl%delta_t_check_ctl, rms_step_STR)
!
!   set physical values
!
      call set_SGS_field_ctl_by_viz                                     &
     &   (spt_ctl%fld_ctl%field_ctl, SPH_TRNS%fld, ierr)
      call set_SGS_field_ctl_by_viz                                     &
     &   (spt_ctl%fld_ctl%field_ctl, FEM_STR%field, ierr)
!
      SPH_STR%cmb_radial_grp =  'CMB'
      if(spt_ctl%cmb_radial_grp_ctl%iflag .gt. 0) then
        SPH_STR%cmb_radial_grp                                          &
     &        = spt_ctl%cmb_radial_grp_ctl%charavalue
      end if
      SPH_STR%icb_radial_grp = 'ICB'
      if(spt_ctl%icb_radial_grp_ctl%iflag .gt. 0) then
        SPH_STR%icb_radial_grp                                          &
     &         = spt_ctl%icb_radial_grp_ctl%charavalue
      end if
!
      call set_ctl_4_global_gauss_coefs(spt_ctl%gauss_sph_fhead_ctl,    &
     &                                  SPH_STR%d_gauss)
!
!   set spherical shell parameters
      call set_ctl_4_sph_grid_maker(nprocs, spt_ctl%psph_ctl,           &
     &    spt_ctl%plt%sph_file_prefix, SPH_STR%sph_file_param,          &
     &    SPH_TRNS%sph_maker, ierr)
      call dealloc_phys_control(spt_ctl%fld_ctl)
!
      end subroutine s_set_ctl_data_4_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_zm_trans(spt_ctl, SPH_STR)
!
!
      type(spherical_transform_util_ctl), intent(in) :: spt_ctl
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
!
!
      if(spt_ctl%zm_spec_file_head_ctl%iflag .gt. 0) then
        SPH_STR%fst_file_IO%file_prefix = zm_spec_file_head
      end if
!
      end subroutine set_ctl_data_4_zm_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_pick_zm(spt_ctl, FEM_STR)
!
      type(spherical_transform_util_ctl), intent(in) :: spt_ctl
      type(FEM_for_SPH_transforms), intent(inout) :: FEM_STR
!
!
      if(spt_ctl%plt%field_file_prefix%iflag .eq. 0) return
      FEM_STR%zm_ucd_input_file%file_prefix                             &
     &              = spt_ctl%plt%field_file_prefix%charavalue
!
      end subroutine set_ctl_data_4_pick_zm
!
! -----------------------------------------------------------------------
!
      end module t_ctl_params_sph_trans
