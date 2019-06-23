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
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
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
!
        integer (kind=kint) :: i_filter_newdomain_ctl = 0
      end type ctl_data_newdomain_filter
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_newdomain_ctl = 'change_filter_domain_ctl'
!
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
!
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      character(len=kchara), parameter :: hd_filter_fnames              &
     &                        = 'filter_files_def'
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_fnames =  'orginal_filter_files_ctl'
!
      private :: hd_platform, hd_new_data, hd_org_filter_fnames
      private :: hd_filter_fnames
!
      private :: id_filter_ctl_file, fname_trans_flt_ctl
      private :: hd_filter_newdomain_ctl
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
      use skip_comment_f
!
      type(ctl_data_newdomain_filter), intent(inout) :: newd_fil_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(id_filter_ctl_file, file=fname_trans_flt_ctl,              &
     &       status='old')
!
        do
          call load_one_line_from_control(id_filter_ctl_file, c_buf1)
          call read_ctl_filter_newdomain_data(id_filter_ctl_file,       &
     &        hd_filter_newdomain_ctl, newd_fil_ctl, c_buf1)
          if(newd_fil_ctl%i_filter_newdomain_ctl .gt. 0) exit
        end do
        close(id_filter_ctl_file)
      end if
!
      call bcast_ctl_filter_newdomain_data(newd_fil_ctl)
!
      end subroutine read_control_filter_newdomain
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_filter_newdomain_data                         &
     &         (id_control, hd_block, newd_fil_ctl, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_newdomain_filter), intent(inout) :: newd_fil_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(newd_fil_ctl%i_filter_newdomain_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_platforms(id_control, hd_platform,            &
     &      newd_fil_ctl%org_filter_plt, c_buf)
        call read_control_platforms(id_control, hd_new_data,            &
     &      newd_fil_ctl%new_filter_plt, c_buf)
        call read_filter_fnames_control                                 &
     &     (id_control, hd_filter_fnames,                               &
     &      newd_fil_ctl%ffile_ndom_ctl, c_buf)
        call read_org_filter_fnames_ctl                                 &
     &     (id_control, hd_org_filter_fnames,                           &
     &      newd_fil_ctl%org_filter_file_ctls, c_buf)
      end do
      newd_fil_ctl%i_filter_newdomain_ctl = 1
!
      end subroutine read_ctl_filter_newdomain_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_filter_newdomain_data(newd_fil_ctl)
!
      use bcast_control_arrays
      use bcast_4_platform_ctl
      use bcast_4_filter_files_ctl
!
      type(ctl_data_newdomain_filter), intent(inout) :: newd_fil_ctl
!
!
      call bcast_ctl_data_4_platform(newd_fil_ctl%org_filter_plt)
      call bcast_ctl_data_4_platform(newd_fil_ctl%new_filter_plt)
      call bcast_filter_fnames_control(newd_fil_ctl%ffile_ndom_ctl)
      call bcast_org_filter_fnames_ctl                                  &
     &   (newd_fil_ctl%org_filter_file_ctls)
!
      call MPI_BCAST(newd_fil_ctl%i_filter_newdomain_ctl, 1,            &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_filter_newdomain_data
!
!  ---------------------------------------------------------------------
!
      subroutine reset_ctl_filter_newdomain_data(newd_fil_ctl)
!
      type(ctl_data_newdomain_filter), intent(inout) :: newd_fil_ctl
!
!
      call reset_control_platforms(newd_fil_ctl%org_filter_plt)
      call reset_control_platforms(newd_fil_ctl%new_filter_plt)
      call reset_filter_fnames_control(newd_fil_ctl%ffile_ndom_ctl)
      call reset_org_filter_fnames_ctl                                  &
     &   (newd_fil_ctl%org_filter_file_ctls)
!
      newd_fil_ctl%i_filter_newdomain_ctl = 0
!
      end subroutine reset_ctl_filter_newdomain_data
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_newdomain_filter
