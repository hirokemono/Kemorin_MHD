!m_ctl_data_plane_spec_file.f90
!      module m_ctl_data_plane_spec_file
!
!        programmed by H.Matsui on Oct., 2008
!
!      subroutine read_ctl_data_plane_spec_file
!
!   file and domain controls
!
!  begin plane_spectr_file_def
!    plane_spectr_mode_head    'spectr/spectral'
!    plane_spectr_data_head    'spectr/spec_mode'
!    plane_spectr_ene_head     'spectr/ene_spec'
!  end
!
      module m_ctl_data_plane_spec_file
!
      use m_precision
!
      implicit  none
!
      character(len=kchara) :: plane_spectr_mode_head_ctl
      character(len=kchara) :: plane_spectr_data_head_ctl
      character(len=kchara) :: plane_spectr_ene_head_ctl
      character(len=kchara) :: plane_spectr_h_ene_head_ctl
!
!   label for entry
!
      character(len=kchara), parameter                                  &
     &                      :: hd_spec_file = 'plane_spectr_file_def'
      integer (kind=kint) :: i_spec_file = 0
!
!   read flags
!
      character(len=kchara), parameter                                  &
     &       :: hd_plane_spec_mode_head = 'plane_spectr_mode_head'
      character(len=kchara), parameter                                  &
     &       :: hd_plane_spec_data_head = 'plane_spectr_data_head'
      character(len=kchara), parameter                                  &
     &       :: hd_plane_spec_ene_head =  'plane_spectr_ene_head'
      character(len=kchara), parameter                                  &
     &       :: hd_plane_sp_h_ene_head =  'plane_spectr_horiz_ene_head'
      integer(kind = kint) :: i_plane_spec_mode_head = 0
      integer(kind = kint) :: i_plane_spec_data_head = 0
      integer(kind = kint) :: i_plane_spec_ene_head =  0
      integer(kind = kint) :: i_plane_sp_h_ene_head =  0
!
      private :: hd_spec_file, i_spec_file
      private :: hd_plane_spec_mode_head, hd_plane_spec_data_head
      private :: hd_plane_spec_ene_head, hd_plane_sp_h_ene_head
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_plane_spec_file
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_spec_file) .eq. 0) return
      if (i_spec_file .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_spec_file, i_spec_file)
        if(i_spec_file .gt. 0) exit
!
        call read_character_ctl_item(hd_plane_spec_mode_head,           &
     &        i_plane_spec_mode_head, plane_spectr_mode_head_ctl)
        call read_character_ctl_item(hd_plane_spec_data_head,           &
     &        i_plane_spec_data_head, plane_spectr_data_head_ctl)
        call read_character_ctl_item(hd_plane_spec_ene_head,            &
     &        i_plane_spec_ene_head, plane_spectr_ene_head_ctl)
        call read_character_ctl_item(hd_plane_sp_h_ene_head,            &
     &        i_plane_sp_h_ene_head, plane_spectr_h_ene_head_ctl)
      end do
!
      end subroutine read_ctl_data_plane_spec_file
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_plane_spec_file
