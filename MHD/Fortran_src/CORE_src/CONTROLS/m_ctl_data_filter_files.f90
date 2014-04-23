!
! m_ctl_data_filter_files.f90
!
!      module m_ctl_data_filter_files
!
!      Written by H. Matsui on July, 2006
!
!      subroutine read_filter_fnames_ctl
!
!  ---------------------------------------------------------------------
!
!      begin filter_files_def
!        filter_file_header           'filter/filter_node'
!        filter_elength_header        'filter/filter_elength'
!        filter_moment_header         'filter/filter_moms'
!        filter_coefs_header          'filter/filter_coef'
!        wider_filter_header          'filter/filter_coef_2'
!
!        filter_elen_format        'ascii'
!        filter_3d_format          'binary'
!        filter_wide_format        'gzip'
!
!        model_coef_ini_header    'model_coefs_ini'
!      end filter_files_def
!
!  ---------------------------------------------------------------------
!
      module m_ctl_data_filter_files
!
      use m_precision
!
      implicit  none
!
!
      character(len = kchara) :: filter_head_ctl = "mesh/filter_node"
      character(len = kchara) :: filter_coef_head_ctl = "filter_coef"
      character(len = kchara) ::                                        &
     &             filter_elen_head_ctl = "filter_elength"
      character(len = kchara) :: filter_moms_head_ctl = "filter_moms"
!
      character(len = kchara)                                           &
     &          :: filter_wide_head_ctl = "mesh/filter_node_2"
!
      character(len = kchara)                                           &
     &          :: model_coef_ini_head_ctl = "restart/sgs_coef_rst"
!
      character(len = kchara) :: filter_elen_format = "ascii"
      character(len = kchara) :: filter_3d_format =   "ascii"
      character(len = kchara) :: filter_wide_format = "ascii"
!
!    label for entry
!
      character(len=kchara), parameter :: hd_filter_fnames              &
     &                        = 'filter_files_def'
      integer (kind=kint) :: i_filter_fnames = 0
!
!     flags for filter file headers
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_head_ctl =      'filter_file_header'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_elen_head_ctl = 'filter_elength_header'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_moms_head_ctl = 'filter_moment_header'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_coef_head_ctl = 'filter_coefs_header'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_wide_head =     'wider_filter_header'
      character(len=kchara), parameter                                  &
     &         :: hd_model_coef_ini_head =  'model_coef_ini_header'
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_elen_fmt = 'filter_elen_format'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_3d_fmt =   'filter_3d_format'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_wide_fmt = 'filter_wide_format'
!
      integer (kind=kint) :: i_filter_head_ctl =       0
      integer (kind=kint) :: i_filter_elen_head_ctl =  0
      integer (kind=kint) :: i_filter_moms_head_ctl =  0
      integer (kind=kint) :: i_filter_coef_head_ctl =  0
      integer (kind=kint) :: i_filter_wide_head =      0
      integer (kind=kint) :: i_model_coef_ini_head =   0
!
      integer (kind=kint) :: i_filter_elen_fmt =   0
      integer (kind=kint) :: i_filter_3d_fmt =     0
      integer (kind=kint) :: i_filter_wide_fmt =   0
!
      private :: hd_filter_fnames, i_filter_fnames
      private :: hd_filter_head_ctl, hd_filter_elen_head_ctl
      private :: hd_filter_moms_head_ctl, hd_filter_coef_head_ctl
      private :: hd_filter_wide_head, hd_model_coef_ini_head
      private :: hd_filter_elen_fmt, hd_filter_3d_fmt
      private :: hd_filter_wide_fmt
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_fnames_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_filter_fnames) .eq. 0) return
      if (i_filter_fnames .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_filter_fnames, i_filter_fnames)
        if(i_filter_fnames .gt. 0) exit
!
!
        call read_character_ctl_item(hd_filter_head_ctl,                &
     &        i_filter_head_ctl, filter_head_ctl)
        call read_character_ctl_item(hd_filter_coef_head_ctl,           &
     &        i_filter_coef_head_ctl, filter_coef_head_ctl)
        call read_character_ctl_item(hd_filter_elen_head_ctl,           &
     &        i_filter_elen_head_ctl, filter_elen_head_ctl)
        call read_character_ctl_item(hd_filter_moms_head_ctl,           &
     &        i_filter_moms_head_ctl, filter_moms_head_ctl)
        call read_character_ctl_item(hd_filter_wide_head,               &
     &        i_filter_wide_head, filter_wide_head_ctl)
        call read_character_ctl_item(hd_model_coef_ini_head,            &
     &        i_model_coef_ini_head, model_coef_ini_head_ctl)
!
        call read_character_ctl_item(hd_filter_elen_fmt,                &
     &        i_filter_elen_fmt, filter_elen_format)
        call read_character_ctl_item(hd_filter_3d_fmt,                  &
     &        i_filter_3d_fmt, filter_3d_format)
        call read_character_ctl_item(hd_filter_wide_fmt,                &
     &        i_filter_wide_fmt, filter_wide_format)
      end do
!
      end subroutine read_filter_fnames_ctl
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_filter_files
