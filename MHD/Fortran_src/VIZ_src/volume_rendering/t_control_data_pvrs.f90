!>@file   t_control_data_pvrs.f90
!!@brief  module t_control_data_pvrs
!!
!!@author  H. Matsui
!!@date Programmed in July, 2006
!
!>@brief structure of control data for multiple PVRs
!!
!!@verbatim
!!      subroutine read_files_4_pvr_ctl(pvr_ctls)
!!      subroutine bcast_files_4_pvr_ctl(pvr_ctls)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  volume_rendering  1
!!      file  volume_rendering  'ctl_pvr_temp'
!!    end array volume_rendering
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_pvrs
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_4_pvr
!
      implicit  none
!
      character(len=kchara), parameter                                  &
     &                    :: hd_pvr_ctl = 'volume_rendering'
      character(len=kchara), parameter                                  &
     &                    :: hd_pvr_colordef =  'pvr_color_ctl'
!
      type volume_rendering_controls
        integer(kind = kint) :: num_pvr_ctl = 0
        character(len = kchara), allocatable :: fname_pvr_ctl(:)
        type(pvr_parameter_ctl), allocatable :: pvr_ctl_type(:)
      end type volume_rendering_controls
!
!     lavel for volume rendering
!
      integer (kind=kint) :: i_pvr_ctl =   0
!
      private :: i_pvr_ctl
      private :: alloc_pvr_ctl_struct
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_ctl_struct(pvr_ctls)
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
!
!
      allocate(pvr_ctls%fname_pvr_ctl(pvr_ctls%num_pvr_ctl))
      allocate(pvr_ctls%pvr_ctl_type(pvr_ctls%num_pvr_ctl))
!
      end subroutine alloc_pvr_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_file_header_ctl(pvr_ctls)
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
!
!
      deallocate(pvr_ctls%pvr_ctl_type)
      deallocate(pvr_ctls%fname_pvr_ctl)
!
      end subroutine dealloc_pvr_file_header_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_pvr_ctl(pvr_ctls)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
!
!
      if (i_pvr_ctl .gt. 0) return
!
      call alloc_pvr_ctl_struct(pvr_ctls)
      do
        call load_ctl_label_and_line
!
        call find_control_end_array_flag(hd_pvr_ctl,                    &
     &      pvr_ctls%num_pvr_ctl, i_pvr_ctl)
        if(i_pvr_ctl .ge. pvr_ctls%num_pvr_ctl) exit
!
        if(right_file_flag(hd_pvr_ctl) .gt. 0) then
          call read_file_names_from_ctl_line(pvr_ctls%num_pvr_ctl,      &
     &        i_pvr_ctl, pvr_ctls%fname_pvr_ctl)
        end if
!
        if(right_begin_flag(hd_pvr_ctl) .gt. 0) then
          i_pvr_ctl = i_pvr_ctl + 1
          pvr_ctls%fname_pvr_ctl(i_pvr_ctl) = 'NO_FILE'
          call read_pvr_ctl(hd_pvr_ctl, hd_pvr_colordef,                &
     &        pvr_ctls%pvr_ctl_type(i_pvr_ctl))
        end if
      end do
!
      end subroutine read_files_4_pvr_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_files_4_pvr_ctl(pvr_ctls)
!
      use calypso_mpi
      use bcast_control_data_4_pvr
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
!
!
      call MPI_BCAST(pvr_ctls%num_pvr_ctl,  1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      if(pvr_ctls%num_pvr_ctl .le. 0) return
!
      if(my_rank .gt. 0)  call alloc_pvr_ctl_struct(pvr_ctls)
!
      call MPI_BCAST                                                    &
     &   (pvr_ctls%fname_pvr_ctl, int(kchara*pvr_ctls%num_pvr_ctl),     &
     &    CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_files_4_pvr_ctl
!
!   --------------------------------------------------------------------
!
      end module t_control_data_pvrs
