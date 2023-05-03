!bcast_control_newdom_filter.f90
!      module bcast_control_newdom_filter
!
!>@file   t_ctl_data_4_projection.f90
!!@brief  module t_ctl_data_4_projection
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR projection and streo parameter
!!
!!@verbatim
!!      subroutine load_control_filter_newdomain(newd_fil_ctl)
!!        type(ctl_data_newdomain_filter), intent(inout) :: newd_fil_ctl
!!@endverbatim
!
      module bcast_control_newdom_filter
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_newdomain_filter
!
      implicit  none
!
      character(len = kchara), parameter, private                       &
     &             :: fname_trans_flt_ctl = "ctl_new_domain_filter"
!
      private :: bcast_ctl_filter_newdomain_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_control_filter_newdomain(newd_fil_ctl)
!
      use skip_comment_f
!
      type(ctl_data_newdomain_filter), intent(inout) :: newd_fil_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        call read_control_filter_newdomain(fname_trans_flt_ctl,         &
     &                                     newd_fil_ctl)
      end if
!
      call bcast_ctl_filter_newdomain_data(newd_fil_ctl)
!
      end subroutine load_control_filter_newdomain
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_filter_newdomain_data(newd_fil_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
      use bcast_4_platform_ctl
      use bcast_4_filter_files_ctl
      use bcast_ctl_data_3d_filter
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
      call calypso_mpi_bcast_one_int                                    &
     &   (newd_fil_ctl%i_filter_newdomain_ctl, 0)
!
      end subroutine bcast_ctl_filter_newdomain_data
!
!  ---------------------------------------------------------------------
!
      end module bcast_control_newdom_filter
