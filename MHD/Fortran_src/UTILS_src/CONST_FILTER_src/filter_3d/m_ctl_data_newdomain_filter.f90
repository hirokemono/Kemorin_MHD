!m_ctl_data_newdomain_filter.f90
!      module m_ctl_data_newdomain_filter
!
!      subroutine read_control_filter_newdomain
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
      module m_ctl_data_newdomain_filter
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), parameter :: id_filter_ctl_file = 11
      character(len = kchara), parameter                                &
     &             :: fname_trans_flt_ctl = "ctl_new_domain_filter"
!
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_newdomain_ctl = 'change_filter_domain_ctl'
      integer (kind=kint) :: i_filter_newdomain_ctl = 0
!
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
      subroutine read_control_filter_newdomain
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      integer(kind = kint) :: iflag
!
!
      ctl_file_code = id_filter_ctl_file
!
      open(ctl_file_code, file=fname_trans_flt_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_ctl_filter_newdomain_data
!
      close(ctl_file_code)
!
      end subroutine read_control_filter_newdomain
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_filter_newdomain_data
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_ctl_data_filter_files
      use m_ctl_data_org_filter_name
!
!
      if(right_begin_flag(hd_filter_newdomain_ctl) .eq. 0) return
      if (i_filter_newdomain_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_filter_newdomain_ctl,             &
     &      i_filter_newdomain_ctl)
        if(i_filter_newdomain_ctl .gt. 0) exit
!
!
        call read_ctl_data_4_platform(plt1)
        call read_ctl_data_4_new_data
        call read_filter_fnames_ctl
        call read_org_filter_fnames_ctl
      end do
!
      end subroutine read_ctl_filter_newdomain_data
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_newdomain_filter
