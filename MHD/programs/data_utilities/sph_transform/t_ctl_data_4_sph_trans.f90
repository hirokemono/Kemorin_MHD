!
!      module t_ctl_data_4_sph_trans
!
!        programmed by H.Matsui on Oct., 2007
!
!!      subroutine read_control_data_sph_trans(spt_ctl)
!!        type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
      module t_ctl_data_4_sph_trans
!
      use m_precision
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_control_data_vizs
      use t_control_elements
      use skip_comment_f
!
      implicit  none
!
      integer (kind = kint) :: control_file_code = 13
      character (len = kchara) :: control_file_name='ctl_sph_transform'
!
      type spherical_transform_util_ctl
!>        Structure for file names
        type(platform_data_control) :: plt
!>        Structure for original file names
        type(platform_data_control) :: org_plt
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
!
!
!>        Structure for field information control
        type(field_control) :: fld_ctl
!>        Structure for time stepping control
        type(time_data_control) :: t_ctl
!>        Structures of visualization controls
        type(visualization_controls) :: viz_ctls
!
        type(read_character_item) :: zm_spec_file_head_ctl
        type(read_character_item) :: zonal_udt_head_ctl
!
        type(read_character_item) :: cmb_radial_grp_ctl
        type(read_character_item) :: icb_radial_grp_ctl
        type(read_character_item) :: gauss_sph_fhead_ctl
!
        type(read_character_item) :: Legendre_trans_loop_ctl
        type(read_character_item) :: FFT_lib_ctl
        type(read_character_item) :: import_mode_ctl
!
        type(read_integer_item) :: legendre_vector_len_ctl
      end type spherical_transform_util_ctl
!
!   Top level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_ctl = 'spherical_transform'
      integer (kind=kint), private :: i_sph_trans_ctl = 0
!
!   1st level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_model =  'model'
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_trans_params = 'sph_transform_ctl'

      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_FEM_mesh = 'FEM_mesh_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_phys_values =  'phys_values_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_time_step = 'time_step_ctl'
!
      integer(kind=kint), private :: i_sph_trans_model =  0
      integer(kind=kint), private :: i_sph_trans_params = 0
!
!   2nd level
!
      character(len=kchara), parameter, private                         &
     &      :: hd_sph_transform_mode  =  'Legendre_trans_loop_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_FFT_package =  'FFT_library_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_import_mode =  'import_table_mode_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_legendre_vect_len = 'Legendre_vector_length_ctl'
!
      character(len=kchara), parameter, private                         &
     &             ::  hd_zm_sph_spec_file =  'zm_spectr_head_ctl'
      character(len=kchara), parameter, private                         &
     &             ::  hd_zm_field_file =  'zonal_udt_head_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_cmb_grp =    'radial_CMB_group_name'
      character(len=kchara), parameter, private                         &
     &             :: hd_icb_grp =  'radial_ICB_group_name'
      character(len=kchara), parameter, private                         &
     &             :: hd_gauss_file_name = 'sph_gauss_coefs_head_ctl'
!
      private :: control_file_code, control_file_name
      private :: read_sph_trans_control_data, read_sph_trans_params_ctl
      private :: read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_data_sph_trans(spt_ctl)
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      ctl_file_code = control_file_code
!
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_sph_trans_control_data(spt_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_data_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_control_data(spt_ctl)
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!   2 begin phys_values_ctl
!
      if(right_begin_flag(hd_sph_trans_ctl) .eq. 0) return
      if (i_sph_trans_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_sph_trans_ctl = find_control_end_flag(hd_sph_trans_ctl)
        if(i_sph_trans_ctl .gt. 0) exit
!
        call read_control_platforms                                     &
     &     (ctl_file_code, hd_platform, spt_ctl%plt, c_buf1)
        call read_control_platforms                                     &
     &     (ctl_file_code, hd_org_data, spt_ctl%org_plt, c_buf1)
        call read_FEM_mesh_control                                      &
     &     (ctl_file_code, hd_FEM_mesh, spt_ctl%Fmesh_ctl, c_buf1)
!
        call read_sph_trans_model_ctl(spt_ctl)
        call read_sph_trans_params_ctl(spt_ctl)
!
        call read_viz_controls                                          &
     &     (ctl_file_code, spt_ctl%viz_ctls, c_buf1)
      end do
!
      end subroutine read_sph_trans_control_data
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_model_ctl(spt_ctl)
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      if(right_begin_flag(hd_sph_trans_model) .eq. 0) return
      if (i_sph_trans_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_sph_trans_model = find_control_end_flag(hd_sph_trans_model)
        if(i_sph_trans_model .gt. 0) exit
!
        call read_phys_data_control                                     &
     &     (ctl_file_code, hd_phys_values, spt_ctl%fld_ctl, c_buf1)
        call read_control_time_step_data                                &
     &     (ctl_file_code, hd_time_step, spt_ctl%t_ctl, c_buf1)
      end do
!
      end subroutine read_sph_trans_model_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_params_ctl(spt_ctl)
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      if(right_begin_flag(hd_sph_trans_params) .eq. 0) return
      if (i_sph_trans_params .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_sph_trans_params = find_control_end_flag(hd_sph_trans_params)
        if(i_sph_trans_params .gt. 0) exit
!
!
        call read_integer_ctl_type(c_buf1, hd_legendre_vect_len,        &
     &      spt_ctl%legendre_vector_len_ctl)
!
        call read_chara_ctl_type(c_buf1, hd_zm_sph_spec_file,           &
     &      spt_ctl%zm_spec_file_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_zm_field_file, spt_ctl%zonal_udt_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_cmb_grp, spt_ctl%cmb_radial_grp_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_icb_grp, spt_ctl%icb_radial_grp_ctl)
!
        call read_chara_ctl_type(c_buf1, hd_sph_transform_mode,         &
     &      spt_ctl%Legendre_trans_loop_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_FFT_package, spt_ctl%FFT_lib_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf1, hd_import_mode, spt_ctl%import_mode_ctl)
!
        call read_chara_ctl_type(c_buf1, hd_gauss_file_name,            &
     &      spt_ctl%gauss_sph_fhead_ctl)
      end do
!
      end subroutine read_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_4_sph_trans
