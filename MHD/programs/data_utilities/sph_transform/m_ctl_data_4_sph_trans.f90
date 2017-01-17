!
!      module m_ctl_data_4_sph_trans
!
!        programmed by H.Matsui on Oct., 2007
!
!      subroutine read_control_data_sph_trans
!
      module m_ctl_data_4_sph_trans
!
      use m_precision
      use m_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_control_elements
      use skip_comment_f
!
      implicit  none
!
      integer (kind = kint) :: control_file_code = 13
      character (len = kchara) :: control_file_name='ctl_sph_transform'
!
!>      Structure for file names
      type(platform_data_control), save :: st_plt
!>      Structure for original file names
      type(platform_data_control), save :: org_st_plt
!>      Structure for field information control
      type(field_control), save :: fld_st_ctl
!>      Structure for time stepping control
      type(time_data_control), save :: t_st_ctl
!
      character(len = kchara) :: zm_spec_file_head_ctl
      character(len = kchara) :: zonal_udt_head_ctl
!
      character(len=kchara) :: cmb_radial_grp_ctl =  'CMB'
      character(len=kchara) :: icb_radial_grp_ctl =  'ICB'
      character(len=kchara) :: gauss_sph_fhead_ctl
!
      type(read_character_item) :: Legendre_trans_loop_ctl
      type(read_character_item) :: FFT_lib_ctl
      type(read_character_item) :: import_mode_ctl
!
      type(read_integer_item) :: legendre_vector_len_ctl
!
!   Top level
!
      character(len=kchara) :: hd_sph_trans_ctl = 'spherical_transform'
      integer (kind=kint) :: i_sph_trans_ctl = 0
!
!   1st level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_sph_trans_model =  'model'
      character(len=kchara), parameter                                  &
     &                    :: hd_sph_trans_params = 'sph_transform_ctl'

      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
!
      integer (kind=kint) :: i_platform =   0
      integer (kind=kint) :: i_org_data =      0
      integer (kind=kint) :: i_sph_trans_model =  0
      integer (kind=kint) :: i_sph_trans_params = 0
      integer (kind=kint) :: i_phys_values =   0
      integer (kind=kint) :: i_tstep =      0
!
!   2nd level
!
      character(len=kchara), parameter :: hd_sph_transform_mode         &
     &                        =  'Legendre_trans_loop_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_FFT_package =  'FFT_library_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_import_mode =  'import_table_mode_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_legendre_vect_len = 'Legendre_vector_length_ctl'
!
      character(len=kchara), parameter :: hd_zm_sph_spec_file           &
     &                        =  'zm_spectr_head_ctl'
      character(len=kchara), parameter :: hd_zm_field_file              &
     &                        =  'zonal_udt_head_ctl'
!
      character(len=kchara), parameter :: hd_cmb_grp =                  &
     &                        'radial_CMB_group_name'
      character(len=kchara), parameter :: hd_icb_grp =                  &
     &                        'radial_ICB_group_name'
      character(len=kchara), parameter :: hd_gauss_file_name            &
     &                        = 'sph_gauss_coefs_head_ctl'
!
      integer (kind=kint) :: i_zm_sph_spec_file =    0
      integer (kind=kint) :: i_zm_field_file =       0
!
      integer (kind=kint) :: i_cmb_grp =         0
      integer (kind=kint) :: i_icb_grp =         0
      integer (kind=kint) :: i_gauss_file_name = 0
!
      private :: control_file_code, control_file_name
      private :: hd_sph_trans_ctl, i_sph_trans_ctl
      private :: hd_sph_trans_model, i_sph_trans_model
      private :: hd_phys_values, i_phys_values
      private :: hd_platform, i_platform
      private :: hd_org_data, i_org_data
      private :: hd_time_step, i_tstep
      private :: hd_FFT_package, hd_import_mode
      private :: hd_sph_trans_params, i_sph_trans_params
      private :: hd_zm_sph_spec_file, hd_zm_field_file
      private :: hd_sph_transform_mode, hd_legendre_vect_len
      private :: hd_cmb_grp, hd_icb_grp, hd_gauss_file_name
      private :: read_sph_trans_control_data, read_sph_trans_params_ctl
      private :: read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_sph_trans
!
      use m_machine_parameter
!
!
      ctl_file_code = control_file_code
!
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_sph_trans_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_data_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_control_data
!
      use m_machine_parameter
      use m_control_data_pvrs
!
!   2 begin phys_values_ctl
!
      if(right_begin_flag(hd_sph_trans_ctl) .eq. 0) return
      if (i_sph_trans_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sph_trans_ctl, i_sph_trans_ctl)
        if(i_sph_trans_ctl .gt. 0) exit
!
        call read_control_platforms(hd_platform, i_platform, st_plt)
        call read_control_platforms                                     &
     &     (hd_org_data, i_org_data, org_st_plt)
!
        call read_sph_trans_model_ctl
        call read_sph_trans_params_ctl
!
        call read_viz_control_data
      end do
!
!      call bcast_viz_control_data
!
      end subroutine read_sph_trans_control_data
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_model_ctl
!
!
      if(right_begin_flag(hd_sph_trans_model) .eq. 0) return
      if (i_sph_trans_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sph_trans_model,                  &
     &      i_sph_trans_model)
        if(i_sph_trans_model .gt. 0) exit
!
        call read_phys_data_control                                     &
     &     (hd_phys_values, i_phys_values, fld_st_ctl)
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, t_st_ctl)
      end do
!
      end subroutine read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_params_ctl
!
!
      if(right_begin_flag(hd_sph_trans_params) .eq. 0) return
      if (i_sph_trans_params .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sph_trans_params,                 &
     &      i_sph_trans_params)
        if(i_sph_trans_params .gt. 0) exit
!
!
        call read_integer_ctl_type(hd_legendre_vect_len,                &
     &      legendre_vector_len_ctl)
!
        call read_character_ctl_item(hd_zm_sph_spec_file,               &
     &          i_zm_sph_spec_file, zm_spec_file_head_ctl)
        call read_character_ctl_item(hd_zm_field_file,                  &
     &          i_zm_field_file, zonal_udt_head_ctl)
        call read_character_ctl_item(hd_cmb_grp,                        &
     &          i_cmb_grp, cmb_radial_grp_ctl)
!
        call read_character_ctl_item(hd_icb_grp,                        &
     &          i_icb_grp, icb_radial_grp_ctl)
!
        call read_chara_ctl_type(hd_sph_transform_mode,                 &
     &      Legendre_trans_loop_ctl)
        call read_chara_ctl_type(hd_FFT_package, FFT_lib_ctl)
        call read_chara_ctl_type(hd_import_mode, import_mode_ctl)
!
        call read_character_ctl_item(hd_gauss_file_name,                &
     &          i_gauss_file_name, gauss_sph_fhead_ctl)
      end do
!
      end subroutine read_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_sph_trans
