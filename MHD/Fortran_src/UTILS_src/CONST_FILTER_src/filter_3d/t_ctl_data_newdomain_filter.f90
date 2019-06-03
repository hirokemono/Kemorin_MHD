!t_ctl_data_newdomain_filter.f90
!      module t_ctl_data_newdomain_filter
!
!      subroutine read_control_filter_newdomain(newd_fil_ctl)
!
!      begin org_filter_filtes_ctl
!        org_filter_file_header       'org/filter_node'
!        org_filter_elength_header    'org/filter_elength'
!        org_filter_moment_header     'org/filter_moms'
!        org_filter_coefs_header      'org/filter_coef'
!      end
!
!      Written by H. Matsui on Apr., 2008
!
      module t_ctl_data_newdomain_filter
!
      use m_precision
      use t_ctl_data_4_platforms
      use t_ctl_data_filter_files
      use t_ctl_data_3d_filter
!
      implicit  none
!
      integer(kind = kint), parameter :: id_filter_ctl_file = 11
      character(len = kchara), parameter                                &
     &             :: fname_trans_flt_ctl = "ctl_new_domain_filter"
!
      type ctl_data_newdomain_filter
        type(platform_data_control) :: org_filter_plt
        type(platform_data_control) :: new_filter_plt
        type(org_filter_prefix_ctls) :: org_filter_file_ctls
!
!>        Structure for filtering files
        type(filter_file_control) :: ffile_ndom_ctl
      end type ctl_data_newdomain_filter
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_newdomain_ctl = 'change_filter_domain_ctl'
      integer (kind=kint) :: i_filter_newdomain_ctl = 0
!
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
!
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      character(len=kchara), parameter :: hd_filter_fnames              &
     &                        = 'filter_files_def'
      integer (kind=kint) :: i_platform =   0
      integer (kind=kint) :: i_new_data =      0
      integer (kind=kint) :: i_filter_fnames = 0
!
      private :: hd_platform, i_platform
      private :: hd_new_data, i_new_data
      private :: hd_filter_fnames, i_filter_fnames
!
      private :: id_filter_ctl_file, fname_trans_flt_ctl
      private :: hd_filter_newdomain_ctl, i_filter_newdomain_ctl
      private :: read_ctl_filter_newdomain_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_filter_newdomain(newd_fil_ctl)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      type(ctl_data_newdomain_filter), intent(inout) :: newd_fil_ctl
!
      integer(kind = kint) :: iflag
!
!
      ctl_file_code = id_filter_ctl_file
!
      open(ctl_file_code, file=fname_trans_flt_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_ctl_filter_newdomain_data(newd_fil_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_filter_newdomain
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_filter_newdomain_data(newd_fil_ctl)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      type(ctl_data_newdomain_filter), intent(inout) :: newd_fil_ctl
!
!
      if(right_begin_flag(hd_filter_newdomain_ctl) .eq. 0) return
      if (i_filter_newdomain_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_filter_newdomain_ctl                                          &
     &    = find_control_end_flag(hd_filter_newdomain_ctl)
        if(i_filter_newdomain_ctl .gt. 0) exit
!
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, newd_fil_ctl%org_filter_plt)
        call read_control_platforms                                     &
     &     (hd_new_data, i_new_data, newd_fil_ctl%new_filter_plt)
        call read_filter_fnames_control                                 &
     &     (ctl_file_code, hd_filter_fnames, i_filter_fnames,           &
     &      newd_fil_ctl%ffile_ndom_ctl, c_buf1)
        call read_org_filter_fnames_ctl                                 &
     &     (newd_fil_ctl%org_filter_file_ctls)
      end do
!
      end subroutine read_ctl_filter_newdomain_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_newdomain_filter
