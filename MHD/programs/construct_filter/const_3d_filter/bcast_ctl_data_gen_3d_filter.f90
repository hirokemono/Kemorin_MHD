!bcast_ctl_data_gen_3d_filter.f90
!      module bcast_ctl_data_gen_3d_filter
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine load_control_4_gen_filter(filter3d_ctl)
!!      subroutine load_control_4_sort_filter(filter3d_ctl)
!!      subroutine dealloc_const_filter_ctl_data(fil3_ctl)
!!        type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
!!        type(ctl_data_gen_3d_filter), intent(inout) :: fil3_ctl
!!
!
      module bcast_ctl_data_gen_3d_filter
!
      use m_precision
      use calypso_mpi
      use t_ctl_data_gen_3d_filter
      use skip_comment_f
!
      implicit  none
!
!
      character(len = kchara), parameter                                &
     &                        :: fname_filter_ctl = "ctl_filter"
      character(len = kchara), parameter                                &
     &                        :: fname_sort_flt_ctl = "ctl_sort_filter"
!
      private :: bcast_const_filter_ctl_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_control_4_gen_filter(filter3d_ctl)
!
      type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        call read_control_4_gen_filter(fname_filter_ctl, filter3d_ctl)
      end if
!
      call bcast_const_filter_ctl_data(filter3d_ctl)
!
      end subroutine load_control_4_gen_filter
!
!  ---------------------------------------------------------------------
!
      subroutine load_control_4_sort_filter(filter3d_ctl)
!
      type(ctl_data_gen_3d_filter), intent(inout) :: filter3d_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        call read_control_4_sort_filter(fname_sort_flt_ctl,             &
     &                                  filter3d_ctl)
      end if
!
      call bcast_const_filter_ctl_data(filter3d_ctl)
!
      end subroutine load_control_4_sort_filter
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_const_filter_ctl_data(filter3d_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
      use bcast_4_platform_ctl
      use bcast_4_filter_files_ctl
      use bcast_ctl_data_3d_filter
      use bcast_ctl_data_gen_filter
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
      call calypso_mpi_bcast_one_int(filter3d_ctl%i_filter_control, 0)
!
      end subroutine bcast_const_filter_ctl_data
!
!  ---------------------------------------------------------------------
!
      end module bcast_ctl_data_gen_3d_filter
