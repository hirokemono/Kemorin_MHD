!>@file   t_ctl_data_org_filter_fname.f90
!!@brief  module t_ctl_data_org_filter_fname
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief control data for 3D filter functions
!!
!!@verbatim
!!      subroutine read_org_filter_fnames_ctl                           &
!!     &         (id_control, hd_block, org_fil_files_ctl, c_buf)
!!      subroutine write_org_filter_fnames_ctl                          &
!!     &         (id_control, hd_block, org_fil_files_ctl, level)
!!      subroutine reset_org_filter_fnames_ctl(org_fil_files_ctl)
!!        type(org_filter_prefix_ctls), intent(inout)                   &
!!       &                             :: org_fil_files_ctl
!!
!!
!!      begin org_filter_filtes_ctl
!!        org_filter_file_header       'org/filter_node'
!!        org_filter_elength_header    'org/filter_elength'
!!        org_filter_moment_header     'org/filter_moms'
!!        org_filter_coefs_header      'org/filter_coef'
!!      end org_filter_filtes_ctl
!!@endverbatim
      module t_ctl_data_org_filter_fname
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use skip_comment_f
!
      implicit  none
!
!
      type org_filter_prefix_ctls
        type(read_character_item) :: org_filter_head_ctl
        type(read_character_item) :: org_filter_coef_head_ctl
        type(read_character_item) :: org_filter_elen_head_ctl
        type(read_character_item) :: org_filter_moms_head_ctl
!
        integer (kind=kint) :: i_org_filter_fnames =  0
      end type org_filter_prefix_ctls
!
!     flags for filter file headers
!
      character(len=kchara), parameter, private                         &
     &         :: hd_org_filter_head =      'org_filter_file_header'
      character(len=kchara), parameter, private                         &
     &         :: hd_org_filter_elen_head = 'org_filter_elength_header'
      character(len=kchara), parameter, private                         &
     &         :: hd_org_filter_moms_head = 'org_filter_moment_header'
      character(len=kchara), parameter, private                         &
     &         :: hd_org_filter_coef_head = 'org_filter_coefs_header'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_org_filter_fnames_ctl                             &
     &         (id_control, hd_block, org_fil_files_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(org_filter_prefix_ctls), intent(inout) :: org_fil_files_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(org_fil_files_ctl%i_org_filter_fnames .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type(c_buf, hd_org_filter_head,             &
     &      org_fil_files_ctl%org_filter_head_ctl)
        call read_chara_ctl_type(c_buf, hd_org_filter_coef_head,        &
     &      org_fil_files_ctl%org_filter_coef_head_ctl)
        call read_chara_ctl_type(c_buf, hd_org_filter_elen_head,        &
     &      org_fil_files_ctl%org_filter_elen_head_ctl)
        call read_chara_ctl_type(c_buf, hd_org_filter_moms_head,        &
     &      org_fil_files_ctl%org_filter_moms_head_ctl)
      end do
      org_fil_files_ctl%i_org_filter_fnames = 1
!
      end subroutine read_org_filter_fnames_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_org_filter_fnames_ctl                            &
     &         (id_control, hd_block, org_fil_files_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(org_filter_prefix_ctls), intent(in) :: org_fil_files_ctl
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(org_fil_files_ctl%i_org_filter_fnames .le. 0) return
!
      maxlen = len_trim(hd_org_filter_head)
      maxlen = max(maxlen, len_trim(hd_org_filter_coef_head))
      maxlen = max(maxlen, len_trim(hd_org_filter_elen_head))
      maxlen = max(maxlen, len_trim(hd_org_filter_moms_head))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_org_filter_head, org_fil_files_ctl%org_filter_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_org_filter_coef_head,                                      &
     &    org_fil_files_ctl%org_filter_coef_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_org_filter_elen_head,                                      &
     &    org_fil_files_ctl%org_filter_elen_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_org_filter_moms_head,                                      &
     &    org_fil_files_ctl%org_filter_moms_head_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_org_filter_fnames_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_org_filter_fnames_ctl(org_fil_files_ctl)
!
      type(org_filter_prefix_ctls), intent(inout) :: org_fil_files_ctl
!
!
      org_fil_files_ctl%org_filter_head_ctl%iflag =      0
      org_fil_files_ctl%org_filter_coef_head_ctl%iflag = 0
      org_fil_files_ctl%org_filter_elen_head_ctl%iflag = 0
      org_fil_files_ctl%org_filter_moms_head_ctl%iflag = 0
      org_fil_files_ctl%i_org_filter_fnames = 0
!
      end subroutine reset_org_filter_fnames_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_org_filter_fname
