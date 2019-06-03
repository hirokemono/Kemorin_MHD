!
!      module t_ctl_data_gen_3d_filter
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine read_control_4_gen_filter(filter3d_ctl)
!!      subroutine read_control_4_sort_filter(filter3d_ctl)
!!      subroutine dealloc_ctl_data_gen_3d_filter(fil3_ctl)
!!        type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
!!        type(ctl_data_gen_3d_filter), intent(inout) :: fil3_ctl
!!
!
      module t_ctl_data_gen_3d_filter
!
      use m_precision
      use m_read_control_elements
      use t_ctl_data_3d_filter
      use t_ctl_data_gen_filter
      use t_ctl_data_4_platforms
      use t_ctl_data_filter_files
      use t_control_elements
      use skip_comment_f
!
      implicit  none
!
!
      integer(kind = kint), parameter :: filter_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_filter_ctl = "ctl_filter"
      character(len = kchara), parameter                                &
     &                        :: fname_sort_flt_ctl = "ctl_sort_filter"
!
!
      type ctl_data_gen_3d_filter
        type(ctl_data_gen_filter) :: gen_f_ctl
        type(ctl_data_3d_filter) :: fil3_ctl
!>        Structure for filtering files
        type(org_filter_prefix_ctls) :: org_fil_files_ctl
      end type ctl_data_gen_3d_filter
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_control = 'const_filter'
      integer (kind=kint) :: i_filter_control = 0
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      integer (kind=kint) :: i_platform =   0
!
      private :: hd_filter_control, i_filter_control
      private :: hd_platform, i_platform
!
      character(len=kchara), parameter :: hd_filter_fnames              &
     &                        = 'filter_files_def'
      integer (kind=kint) :: i_filter_fnames = 0
!
      private :: hd_filter_fnames, i_filter_fnames
      private :: read_const_filter_ctl_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_gen_filter(filter3d_ctl)
!
      type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
!
!
      ctl_file_code = filter_ctl_file_code
!
      open(ctl_file_code, file=fname_filter_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_const_filter_ctl_data                                   &
     &   (filter3d_ctl%gen_f_ctl, filter3d_ctl%fil3_ctl,                &
     &    filter3d_ctl%org_fil_files_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_gen_filter
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_sort_filter(filter3d_ctl)
!
      type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
!
!
      ctl_file_code = filter_ctl_file_code
!
      open(ctl_file_code, file=fname_sort_flt_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_const_filter_ctl_data                                   &
     &   (filter3d_ctl%gen_f_ctl, filter3d_ctl%fil3_ctl,                &
     &    filter3d_ctl%org_fil_files_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_sort_filter
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ctl_data_gen_3d_filter(filter3d_ctl)
!
      type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
!
!
      call dealloc_filter_param_ctl(filter3d_ctl%gen_f_ctl)
      call dealloc_dx_solver_param_ctl(filter3d_ctl%fil3_ctl)
!
      end subroutine dealloc_ctl_data_gen_3d_filter
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_const_filter_ctl_data                             &
     &         (gen_f_ctl, fil3_ctl, org_fil_files_ctl)
!
      type(ctl_data_gen_filter), intent(inout) :: gen_f_ctl
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
      type(org_filter_prefix_ctls), intent(inout) :: org_fil_files_ctl
!
!
      if(right_begin_flag(hd_filter_control) .eq. 0) return
      if (i_filter_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_filter_control = find_control_end_flag(hd_filter_control)
        if(i_filter_control .gt. 0) exit
!
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, fil3_ctl%gen_filter_plt)
!
        call read_filter_param_ctl(gen_f_ctl)
        call read_filter_fnames_control                                 &
     &     (ctl_file_code, hd_filter_fnames, i_filter_fnames,           &
     &      fil3_ctl%ffile_3d_ctl, c_buf1)
        call read_org_filter_fnames_ctl(org_fil_files_ctl)
!
        call read_filter_area_ctl(fil3_ctl)
        call read_element_size_ctl(fil3_ctl)
      end do
!
      end subroutine read_const_filter_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_gen_3d_filter
