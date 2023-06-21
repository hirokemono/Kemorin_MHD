!>@file   t_ctl_data_4_sph_trans.f90
!!@brief  module t_ctl_data_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine read_sph_trans_params_ctl                            &
!!     &         (id_control, hd_block, spt_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_sph_trans_params_ctl                           &
!!     &         (id_control, hd_block, spt_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(spherical_transform_util_ctl), intent(in) :: spt_ctl
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine reset_sph_trans_params_ctl(spt_ctl)
!!        type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!!@endverbatim
!
      module t_ctl_data_4_sph_trans
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_ctl_data_gen_sph_shell
      use t_control_data_vizs
      use t_control_array_integer
      use t_control_array_character
      use skip_comment_f
!
      implicit  none
!
      type spherical_transform_util_ctl
!>        Structure for file names
        type(platform_data_control) :: plt
!>        Structure for original file names
        type(platform_data_control) :: org_plt
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
!
!>        File name to read spherical shell control file
        character (len = kchara) :: fname_psph = 'NO_FILE'
!>        structure of parallel spherical shell data
        type(parallel_sph_shell_control) :: psph_ctl
!
!>        Structure for field information control
        type(field_control) :: fld_ctl
!>        Structure for time stepping control
        type(time_data_control) :: t_ctl
!
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
!
        integer(kind=kint) :: i_sph_trans_ctl = 0
        integer(kind=kint) :: i_sph_trans_model =  0
        integer(kind=kint) :: i_sph_trans_params = 0
      end type spherical_transform_util_ctl
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_trans_params_ctl                              &
     &         (id_control, hd_block, spt_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(spt_ctl%i_sph_trans_params .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_integer_ctl_type(c_buf, hd_legendre_vect_len,         &
     &      spt_ctl%legendre_vector_len_ctl)
!
        call read_chara_ctl_type(c_buf, hd_zm_sph_spec_file,            &
     &      spt_ctl%zm_spec_file_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_zm_field_file, spt_ctl%zonal_udt_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_cmb_grp, spt_ctl%cmb_radial_grp_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_icb_grp, spt_ctl%icb_radial_grp_ctl)
!
        call read_chara_ctl_type(c_buf, hd_sph_transform_mode,          &
     &      spt_ctl%Legendre_trans_loop_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_FFT_package, spt_ctl%FFT_lib_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_import_mode, spt_ctl%import_mode_ctl)
!
        call read_chara_ctl_type(c_buf, hd_gauss_file_name,             &
     &      spt_ctl%gauss_sph_fhead_ctl)
      end do
      spt_ctl%i_sph_trans_params = 1
!
      end subroutine read_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_trans_params_ctl                             &
     &         (id_control, hd_block, spt_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(spherical_transform_util_ctl), intent(in) :: spt_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(spt_ctl%i_sph_trans_params .le. 0) return
!
      maxlen = len_trim(hd_zm_sph_spec_file)
      maxlen = max(maxlen, len_trim(hd_zm_field_file))
      maxlen = max(maxlen, len_trim(hd_gauss_file_name))
      maxlen = max(maxlen, len_trim(hd_cmb_grp))
      maxlen = max(maxlen, len_trim(hd_icb_grp))
      maxlen = max(maxlen, len_trim(hd_import_mode))
      maxlen = max(maxlen, len_trim(hd_FFT_package))
      maxlen = max(maxlen, len_trim(hd_sph_transform_mode))
      maxlen = max(maxlen, len_trim(hd_legendre_vect_len))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_zm_sph_spec_file, spt_ctl%zm_spec_file_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_zm_field_file, spt_ctl%zonal_udt_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_gauss_file_name, spt_ctl%gauss_sph_fhead_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_icb_grp, spt_ctl%icb_radial_grp_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_cmb_grp, spt_ctl%cmb_radial_grp_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_import_mode, spt_ctl%import_mode_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_FFT_package, spt_ctl%FFT_lib_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_sph_transform_mode, spt_ctl%Legendre_trans_loop_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_legendre_vect_len, spt_ctl%legendre_vector_len_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      subroutine reset_sph_trans_params_ctl(spt_ctl)
!
      type(spherical_transform_util_ctl), intent(inout) :: spt_ctl
!
!
      spt_ctl%legendre_vector_len_ctl%iflag = 0
!
      spt_ctl%zm_spec_file_head_ctl%iflag = 0
      spt_ctl%zonal_udt_head_ctl%iflag = 0
      spt_ctl%cmb_radial_grp_ctl%iflag = 0
      spt_ctl%icb_radial_grp_ctl%iflag = 0
!
      spt_ctl%Legendre_trans_loop_ctl%iflag = 0
      spt_ctl%FFT_lib_ctl%iflag = 0
      spt_ctl%import_mode_ctl%iflag = 0
!
      spt_ctl%gauss_sph_fhead_ctl%iflag = 0
!
      spt_ctl%i_sph_trans_params = 0
!
      end subroutine reset_sph_trans_params_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_4_sph_trans
