!input_ctl_filter_newdomain.f90
!      module input_ctl_filter_newdomain
!
!>@file   input_ctl_filter_newdomain.f90
!!@brief  module input_ctl_filter_newdomain
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR projection and streo parameter
!!
!!@verbatim
!!      subroutine s_input_ctl_filter_newdomain                         &
!!     &         (ctl_file_name, nprocs_2nd, newfil_p)
!!        character(len = kchara), intent(in) :: ctl_file_name
!!        integer(kind = kint), intent(inout) :: nprocs_2nd
!!        type(ctl_param_newdom_filter), intent(inout) :: newfil_p
!!@endverbatim
!
      module input_ctl_filter_newdomain
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_newdomain_filter
      use t_ctl_param_newdom_filter
!
      implicit  none
!
      private :: bcast_ctl_filter_newdomain_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_input_ctl_filter_newdomain                           &
     &         (ctl_file_name, nprocs_2nd, newfil_p)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: ctl_file_name
      integer(kind = kint), intent(inout) :: nprocs_2nd
      type(ctl_param_newdom_filter), intent(inout) :: newfil_p
!
      type(ctl_data_newdomain_filter) :: newd_fil_ctl
      integer(kind = kint) :: ierr
!
!
      if(my_rank .eq. 0) then
        call read_control_filter_newdomain(ctl_file_name, newd_fil_ctl)
      end if
      call bcast_ctl_filter_newdomain_data(newd_fil_ctl)
!
      if(newd_fil_ctl%i_filter_newdomain_ctl.ne. 1) then
        call calypso_MPI_abort(newd_fil_ctl%i_filter_newdomain_ctl,     &
     &                         trim(ctl_file_name))
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_filter_newdomain'
      call set_control_filter_newdomain                                 &
     &   (nprocs_2nd, newd_fil_ctl%org_filter_plt,                      &
     &    newd_fil_ctl%new_filter_plt, newd_fil_ctl%ffile_ndom_ctl,     &
     &    newd_fil_ctl%org_filter_file_ctls, newfil_p, ierr)
      if(ierr .gt. 0) call calypso_mpi_abort(ierr,                      &
     &                    'Failed s_input_ctl_filter_newdomain')
!
      end subroutine s_input_ctl_filter_newdomain
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
      end module input_ctl_filter_newdomain
