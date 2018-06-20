!m_ctl_data_org_filter_name.f90
!      module m_ctl_data_org_filter_name
!
!      Written by H. Matsui on Nov., 2008
!
!      subroutine read_org_filter_fnames_ctl
!
!      begin org_filter_filtes_ctl
!        org_filter_file_header       'org/filter_node'
!        org_filter_elength_header    'org/filter_elength'
!        org_filter_moment_header     'org/filter_moms'
!        org_filter_coefs_header      'org/filter_coef'
!      end org_filter_filtes_ctl
!
      module m_ctl_data_org_filter_name
!
      use m_precision
      use t_control_elements
!
      implicit  none
!
!
      type(read_character_item), save :: org_filter_head_ctl
      type(read_character_item), save :: org_filter_coef_head_ctl
      type(read_character_item), save :: org_filter_elen_head_ctl
      type(read_character_item), save :: org_filter_moms_head_ctl
!
!     label for entry
!
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_fnames =  'orginal_filter_files_ctl'
      integer (kind=kint) :: i_org_filter_fnames =  0
!
!     flags for filter file headers
!
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_head =      'org_filter_file_header'
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_elen_head = 'org_filter_elength_header'
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_moms_head = 'org_filter_moment_header'
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_coef_head = 'org_filter_coefs_header'
!
!
      private :: hd_org_filter_fnames, i_org_filter_fnames
      private :: hd_org_filter_head, hd_org_filter_elen_head
      private :: hd_org_filter_moms_head, hd_org_filter_coef_head
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_org_filter_fnames_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_org_filter_fnames) .eq. 0) return
      if (i_org_filter_fnames .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_org_filter_fnames                                             &
     &      = find_control_end_flag(hd_org_filter_fnames)
        if(i_org_filter_fnames .gt. 0) exit
!
!
        call read_chara_ctl_type(hd_org_filter_head,                    &
     &      org_filter_head_ctl)
        call read_chara_ctl_type(hd_org_filter_coef_head,               &
     &      org_filter_coef_head_ctl)
        call read_chara_ctl_type(hd_org_filter_elen_head,               &
     &      org_filter_elen_head_ctl)
        call read_chara_ctl_type(hd_org_filter_moms_head,               &
     &      org_filter_moms_head_ctl)
      end do
!
      end subroutine read_org_filter_fnames_ctl
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_org_filter_name
