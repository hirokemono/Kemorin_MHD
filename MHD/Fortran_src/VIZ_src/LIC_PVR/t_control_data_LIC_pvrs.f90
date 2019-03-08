!>@file   t_control_data_LIC_pvrs.f90
!!@brief  module t_control_data_LIC_pvrs
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine read_files_4_lic_ctl(lic_ctls)
!!      subroutine bcast_files_4_lic_ctl(lic_ctls)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  LIC_rendering  1
!!      file  LIC_rendering  'ctl_pvr_temp'
!!    end array LIC_rendering
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module t_control_data_LIC_pvrs
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_4_pvr
      use t_control_data_lic_pvr
!
      implicit  none
!
      character(len=kchara), parameter                                  &
     &                    :: hd_lic_ctl = 'LIC_rendering'
      character(len=kchara), parameter                                  &
     &                    :: hd_lic_colordef =  'LIC_color_ctl'
!
      type lic_rendering_controls
        integer(kind = kint) :: num_lic_ctl = 0
        character(len = kchara), allocatable :: fname_lic_ctl(:)
        type(pvr_parameter_ctl), allocatable :: pvr_ctl_type(:)
        type(lic_parameter_ctl), allocatable :: lic_ctl_type(:)
      end type lic_rendering_controls
!
!     lavel for volume rendering
!
      integer (kind=kint) :: i_lic_ctl =   0
!
      private :: i_lic_ctl
      private :: alloc_lic_ctl_struct
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_lic_ctl_struct(lic_ctls)
!
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
!
      allocate(lic_ctls%fname_lic_ctl(lic_ctls%num_lic_ctl))
      allocate(lic_ctls%pvr_ctl_type(lic_ctls%num_lic_ctl))
      allocate(lic_ctls%lic_ctl_type(lic_ctls%num_lic_ctl))
!
      end subroutine alloc_lic_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_file_header_ctl(lic_ctls)
!
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
!
      deallocate(lic_ctls%lic_ctl_type)
      deallocate(lic_ctls%pvr_ctl_type)
      deallocate(lic_ctls%fname_lic_ctl)
!
      end subroutine dealloc_pvr_file_header_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_lic_ctl(lic_ctls)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
!
      if (i_lic_ctl .gt. 0) return
!
      call alloc_lic_ctl_struct(lic_ctls)
      do
        call load_ctl_label_and_line
!
        call find_control_end_array_flag(hd_lic_ctl,                    &
     &      lic_ctls%num_lic_ctl, i_lic_ctl)
        if(i_lic_ctl .ge. lic_ctls%num_lic_ctl) exit
!
        if(right_file_flag(hd_lic_ctl) .gt. 0) then
          call read_file_names_from_ctl_line(lic_ctls%num_lic_ctl,      &
     &        i_lic_ctl, lic_ctls%fname_lic_ctl)
        end if
!
        if(right_begin_flag(hd_lic_ctl) .gt. 0) then
          i_lic_ctl = i_lic_ctl + 1
          lic_ctls%fname_lic_ctl(i_lic_ctl) = 'NO_FILE'
          call read_lic_pvr_ctl(hd_lic_ctl, hd_lic_colordef,            &
     &        lic_ctls%pvr_ctl_type(i_lic_ctl),                         &
     &        lic_ctls%lic_ctl_type(i_lic_ctl))
        end if
      end do
!
      end subroutine read_files_4_lic_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_files_4_lic_ctl(lic_ctls)
!
      use calypso_mpi
      use bcast_control_data_4_pvr
!
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
!
      call MPI_BCAST(lic_ctls%num_lic_ctl,  1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call calypso_mpi_barrier
      if(lic_ctls%num_lic_ctl .le. 0) return
!
      if(my_rank .gt. 0)  call alloc_lic_ctl_struct(lic_ctls)
!
      call MPI_BCAST                                                    &
     &   (lic_ctls%fname_lic_ctl, (kchara*lic_ctls%num_lic_ctl),        &
     &    CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_files_4_lic_ctl
!
!   --------------------------------------------------------------------
!
      end module t_control_data_LIC_pvrs
