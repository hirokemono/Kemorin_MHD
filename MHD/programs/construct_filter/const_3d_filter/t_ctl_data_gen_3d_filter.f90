!
!      module t_ctl_data_gen_3d_filter
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine read_control_4_gen_filter(filter3d_ctl)
!!      subroutine read_control_4_sort_filter(filter3d_ctl)
!!      subroutine dealloc_const_filter_ctl_data(fil3_ctl)
!!        type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
!!        type(ctl_data_gen_3d_filter), intent(inout) :: fil3_ctl
!!
!
      module t_ctl_data_gen_3d_filter
!
      use m_precision
      use calypso_mpi
      use t_read_control_elements
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
!>        Structure for file settings
        type(platform_data_control) :: gen_filter_plt
!
        type(ctl_data_gen_filter) :: gen_f_ctl
        type(ctl_data_3d_filter) :: fil3_ctl
!>        Structure for filtering files
        type(org_filter_prefix_ctls) :: org_fil_files_ctl
!
        integer (kind=kint) :: i_filter_control = 0
      end type ctl_data_gen_3d_filter
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_control = 'const_filter'
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_filter_fnames = 'filter_files_def'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_param_ctl = 'filter_control'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_area_ctl =  'filter_area_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_deltax_ctl =       'element_size_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_fnames =  'orginal_filter_files_ctl'
!
      private :: hd_filter_control, hd_platform
      private :: hd_filter_fnames, hd_org_filter_fnames, hd_deltax_ctl
      private :: hd_filter_area_ctl, hd_filter_param_ctl
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
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(filter_ctl_file_code, file=fname_filter_ctl, status='old')
!
        do
          call load_one_line_from_control(filter_ctl_file_code, c_buf1)
          call read_const_filter_ctl_data(filter_ctl_file_code,         &
     &        hd_filter_control, filter3d_ctl, c_buf1)
          if(filter3d_ctl%i_filter_control .gt. 0) exit
        end do
        close(filter_ctl_file_code)
      end if
!
      call bcast_const_filter_ctl_data(filter3d_ctl)
!
      end subroutine read_control_4_gen_filter
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_sort_filter(filter3d_ctl)
!
      type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(filter_ctl_file_code, file=fname_sort_flt_ctl,             &
     &       status='old')
!
        do
          call load_one_line_from_control(filter_ctl_file_code, c_buf1)
          call read_const_filter_ctl_data(filter_ctl_file_code,         &
     &        hd_filter_control, filter3d_ctl, c_buf1)
          if(filter3d_ctl%i_filter_control .gt. 0) exit
        end do
        close(filter_ctl_file_code)
      end if
!
      call bcast_const_filter_ctl_data(filter3d_ctl)
!
      end subroutine read_control_4_sort_filter
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_const_filter_ctl_data                             &
     &         (id_control, hd_block, filter3d_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(filter3d_ctl%i_filter_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_platforms(id_control, hd_platform,            &
     &     filter3d_ctl%gen_filter_plt, c_buf)
!
        call read_filter_param_ctl(id_control, hd_filter_param_ctl,     &
     &      filter3d_ctl%gen_f_ctl, c_buf)
        call read_filter_fnames_control                                 &
     &     (id_control, hd_filter_fnames,                               &
     &      filter3d_ctl%fil3_ctl%ffile_3d_ctl, c_buf)
!
        call read_filter_area_ctl(id_control, hd_filter_area_ctl,       &
     &      filter3d_ctl%fil3_ctl, c_buf)
        call read_element_size_ctl(id_control, hd_deltax_ctl,           &
     &     filter3d_ctl%fil3_ctl, c_buf)
        call read_org_filter_fnames_ctl                                 &
     &     (id_control, hd_org_filter_fnames,                           &
     &      filter3d_ctl%org_fil_files_ctl, c_buf)
      end do
      filter3d_ctl%i_filter_control = 1
!
      end subroutine read_const_filter_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_const_filter_ctl_data(filter3d_ctl)
!
      use bcast_control_arrays
      use bcast_4_platform_ctl
      use bcast_4_filter_files_ctl
!
      type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
!
!
      call bcast_ctl_data_4_platform(filter3d_ctl%gen_filter_plt)
      call bcast_filter_param_ctl(filter3d_ctl%gen_f_ctl)
!
      call bcast_filter_fnames_control                                  &
     &   (filter3d_ctl%fil3_ctl%ffile_3d_ctl)
      call bcast_filter_area_ctl(filter3d_ctl%fil3_ctl)
      call bcast_element_size_ctl(filter3d_ctl%fil3_ctl)
      call bcast_org_filter_fnames_ctl(filter3d_ctl%org_fil_files_ctl)
!
      call MPI_BCAST(filter3d_ctl%i_filter_control, 1,                  &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_const_filter_ctl_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_const_filter_ctl_data(filter3d_ctl)
!
      type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
!
!
      call reset_control_platforms(filter3d_ctl%gen_filter_plt)
      call dealloc_filter_param_ctl(filter3d_ctl%gen_f_ctl)
!
      call reset_filter_fnames_control                                  &
     &   (filter3d_ctl%fil3_ctl%ffile_3d_ctl)
      call dealloc_filter_area_ctl(filter3d_ctl%fil3_ctl)
      call reset_element_size_ctl(filter3d_ctl%fil3_ctl)
      call reset_org_filter_fnames_ctl(filter3d_ctl%org_fil_files_ctl)
!
      filter3d_ctl%i_filter_control = 0
!
      end subroutine dealloc_const_filter_ctl_data
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_gen_3d_filter
