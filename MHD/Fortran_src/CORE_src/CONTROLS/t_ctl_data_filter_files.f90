!>@file   t_ctl_data_filter_files.f90
!!@brief  module t_ctl_data_filter_files
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief  Structure for reading parameters for filtering files
!!
!!@verbatim
!!      subroutine read_filter_fnames_control                           &
!!     &         (id_control, hd_block, ffile_ctl, c_buf)
!!      subroutine reset_filter_fnames_control(ffile_ctl)
!!        type(filter_file_control), intent(inout) :: ffile_ctl
!!        type(buffer_for_control), intent(inout) :: c_buf
!!
!!  ---------------------------------------------------------------------
!!
!!      begin filter_files_def
!!        filter_file_prefix           'filter/filter_node'
!!        filter_elength_prefix        'filter/filter_elength'
!!        filter_moment_prefix         'filter/filter_moms'
!!        filter_coefs_prefix          'filter/filter_coef'
!!        wider_filter_prefix          'filter/filter_coef_2'
!!
!!        filter_elen_format        'ascii'
!!        filter_3d_format          'binary'
!!        filter_wide_format        'gzip'
!!
!!        model_coef_rst_prefix       'model_coefs_ini'
!!        commutel_coef_rst_prefix    'commute_coefs_ini'
!!        model_coef_rst_format      'merged_gz'
!!        commute_coef_rst_format    'merged_gz'
!!      end filter_files_def
!!
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_filter_files
!
      use m_precision
      use t_control_elements
      use t_read_control_elements
!
      implicit  none
!
!
!>      Structure for filtering files
      type filter_file_control
!>        Structure for filter file for nodes
        type(read_character_item) :: filter_head_ctl
!>        Structure for filter coefficients file for nodes
        type(read_character_item) :: filter_coef_head_ctl
!>        Structure for filter size file for nodes
        type(read_character_item) :: filter_elen_head_ctl
!>        Structure for filter moments file for nodes
        type(read_character_item) :: filter_moms_head_ctl
!
!>        Structure for wider filter file for nodes
        type(read_character_item) :: filter_wide_head_ctl
!
!>        Structure for model coefficients file for nodes
        type(read_character_item) :: model_coef_ini_head_ctl
!>        Structure for commutation coefficients file for nodes
        type(read_character_item) :: commute_coef_ini_head_ctl
!
!>        Structure for file format of element length
        type(read_character_item) :: filter_elen_format
!>        Structure for file format of 3D filter file
        type(read_character_item) :: filter_3d_format
!>        Structure for file format of wider filter file
        type(read_character_item) :: filter_wide_format
!
!>        Structure for file format of model coefficient
        type(read_character_item) :: model_coef_rst_format
!>        Structure for file format of commutation coefficient
        type(read_character_item) :: commute_coef_rst_format
!
        integer (kind=kint) :: i_filter_fnames = 0
      end type filter_file_control
!
!     flags for filter file headers
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_head_ctl =       'filter_file_prefix'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_elen_head_ctl =  'filter_elength_prefix'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_moms_head_ctl =  'filter_moment_prefix'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_coef_head_ctl =  'filter_coefs_prefix'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_wide_head =      'wider_filter_prefix'
      character(len=kchara), parameter                                  &
     &         :: hd_model_coef_ini_head =   'model_coef_rst_prefix'
      character(len=kchara), parameter                                  &
     &         :: hd_commute_coef_ini_head = 'commutel_coef_rst_prefix'
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_elen_fmt = 'filter_elen_format'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_3d_fmt =   'filter_3d_format'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_wide_fmt = 'filter_wide_format'
      character(len=kchara), parameter                                  &
     &         :: hd_model_coef_rst_format = 'model_coef_rst_format'
      character(len=kchara), parameter                                  &
     &         :: hd_commute_c_rst_format = 'commute_coef_rst_format'
!
      private :: hd_filter_head_ctl, hd_filter_elen_head_ctl
      private :: hd_filter_moms_head_ctl, hd_filter_coef_head_ctl
      private :: hd_filter_wide_head, hd_commute_c_rst_format
      private :: hd_model_coef_ini_head, hd_commute_coef_ini_head
      private :: hd_filter_elen_fmt, hd_filter_3d_fmt
      private :: hd_filter_wide_fmt, hd_model_coef_rst_format
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_fnames_control                             &
     &         (id_control, hd_block, ffile_ctl, c_buf)
!
      use m_machine_parameter
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(filter_file_control), intent(inout) :: ffile_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(ffile_ctl%i_filter_fnames .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_filter_head_ctl, ffile_ctl%filter_head_ctl)
        call read_chara_ctl_type(c_buf, hd_filter_coef_head_ctl,        &
     &      ffile_ctl%filter_coef_head_ctl)
        call read_chara_ctl_type(c_buf, hd_filter_elen_head_ctl,        &
     &      ffile_ctl%filter_elen_head_ctl)
        call read_chara_ctl_type(c_buf, hd_filter_moms_head_ctl,        &
     &      ffile_ctl%filter_moms_head_ctl)
        call read_chara_ctl_type(c_buf, hd_filter_wide_head,            &
     &      ffile_ctl%filter_wide_head_ctl)
        call read_chara_ctl_type(c_buf, hd_model_coef_ini_head,         &
     &      ffile_ctl%model_coef_ini_head_ctl)
        call read_chara_ctl_type(c_buf, hd_commute_coef_ini_head,       &
     &      ffile_ctl%commute_coef_ini_head_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_filter_elen_fmt, ffile_ctl%filter_elen_format)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_filter_3d_fmt, ffile_ctl%filter_3d_format)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_filter_wide_fmt, ffile_ctl%filter_wide_format)
        call read_chara_ctl_type(c_buf, hd_model_coef_rst_format,       &
     &      ffile_ctl%model_coef_rst_format)
        call read_chara_ctl_type(c_buf, hd_commute_c_rst_format,        &
     &      ffile_ctl%commute_coef_rst_format)
      end do
      ffile_ctl%i_filter_fnames = 1
!
      end subroutine read_filter_fnames_control
!
!  ---------------------------------------------------------------------
!
      subroutine reset_filter_fnames_control(ffile_ctl)
!
      type(filter_file_control), intent(inout) :: ffile_ctl
!
!
      ffile_ctl%filter_head_ctl%iflag =           0
      ffile_ctl%filter_coef_head_ctl%iflag =      0
      ffile_ctl%filter_elen_head_ctl%iflag =      0
      ffile_ctl%filter_moms_head_ctl%iflag =      0
      ffile_ctl%filter_wide_head_ctl%iflag =      0
      ffile_ctl%model_coef_ini_head_ctl%iflag =   0
      ffile_ctl%commute_coef_ini_head_ctl%iflag = 0
!
      ffile_ctl%filter_elen_format%iflag =      0
      ffile_ctl%filter_3d_format%iflag =        0
      ffile_ctl%filter_wide_format%iflag =      0
      ffile_ctl%model_coef_rst_format%iflag =   0
      ffile_ctl%commute_coef_rst_format%iflag = 0
!
      ffile_ctl%i_filter_fnames = 0
!
      end subroutine reset_filter_fnames_control
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_filter_files
