!>@file   t_ctl_data_newdomain_filter.f90
!!@brief  module t_ctl_data_newdomain_filter
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR projection and streo parameter
!!
!!@verbatim
!!      subroutine read_control_filter_newdomain(file_name,             &
!!     &                                         newd_fil_ctl)
!!      subroutine write_control_filter_newdomain(file_name,            &
!!     &                                          newd_fil_ctl)
!!@endverbatim
!
      module t_ctl_data_newdomain_filter
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_filter_files
      use t_ctl_data_3d_filter
      use t_ctl_data_org_filter_fname
!
      implicit  none
!
      integer(kind = kint), parameter :: id_filter_ctl_file = 11
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
      character(len=kchara), parameter , private                        &
     &         :: hd_filter_newdomain_ctl = 'change_filter_domain_ctl'
!
!
      character(len=kchara), parameter , private                        &
     &                    :: hd_platform = 'data_files_def'
!
      character(len=kchara), parameter , private                        &
     &                    :: hd_new_data = 'new_data_files_def'
      character(len=kchara), parameter , private                        &
     &                    :: hd_filter_fnames = 'filter_files_def'
      character(len=kchara), parameter , private                        &
     &         :: hd_org_filter_fnames =  'orginal_filter_files_ctl'
!
      private :: id_filter_ctl_file
      private :: read_ctl_filter_newdomain_data
      private :: write_ctl_filter_newdomain_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_filter_newdomain(file_name,               &
     &                                         newd_fil_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: file_name
      type(ctl_data_newdomain_filter), intent(inout) :: newd_fil_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      open(id_filter_ctl_file, file=file_name, status='old')
!
      do
        call load_one_line_from_control(id_filter_ctl_file, c_buf1)
        call read_ctl_filter_newdomain_data(id_filter_ctl_file,         &
     &      hd_filter_newdomain_ctl, newd_fil_ctl, c_buf1)
        if(newd_fil_ctl%i_filter_newdomain_ctl .gt. 0) exit
      end do
      close(id_filter_ctl_file)
!
      end subroutine read_control_filter_newdomain
!
!   --------------------------------------------------------------------
!
      subroutine write_control_filter_newdomain(file_name,             &
     &                                          newd_fil_ctl)
!
      use delete_data_files
!
      character(len = kchara), intent(in) :: file_name
      type(ctl_data_newdomain_filter), intent(in) :: newd_fil_ctl
!
      integer(kind = kint) :: level1
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      open(id_filter_ctl_file, file=file_name)
      level1 = 0
      call write_ctl_filter_newdomain_data(id_filter_ctl_file,          &
     &    hd_filter_newdomain_ctl, newd_fil_ctl, level1)
      close(id_filter_ctl_file)
!
      end subroutine write_control_filter_newdomain
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_filter_newdomain_data                         &
     &         (id_control, hd_block, newd_fil_ctl, c_buf)
!
      use ctl_data_platforms_IO
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
      subroutine write_ctl_filter_newdomain_data                        &
     &         (id_control, hd_block, newd_fil_ctl, level)
!
      use ctl_data_platforms_IO
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(ctl_data_newdomain_filter), intent(in) :: newd_fil_ctl
      integer(kind = kint), intent(inout) :: level
!
!
      if(newd_fil_ctl%i_filter_newdomain_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms(id_control, hd_platform,             &
     &    newd_fil_ctl%org_filter_plt, level)
      call write_control_platforms(id_control, hd_new_data,             &
     &    newd_fil_ctl%new_filter_plt, level)
      call write_filter_fnames_control                                  &
     &   (id_control, hd_filter_fnames,                                 &
     &    newd_fil_ctl%ffile_ndom_ctl, level)
      call write_org_filter_fnames_ctl                                  &
     &   (id_control, hd_org_filter_fnames,                             &
     &    newd_fil_ctl%org_filter_file_ctls, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_ctl_filter_newdomain_data
!
!   --------------------------------------------------------------------
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
