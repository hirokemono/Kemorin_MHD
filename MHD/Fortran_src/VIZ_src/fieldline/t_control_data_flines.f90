!
!      module t_control_data_flines
!
!      Written by H. Matsui on July, 2006
!
!      subroutine alloc_fline_ctl_struct(fline_ctls)
!      subroutine dealloc_fline_fhead_ctl(fline_ctls)
!
!      subroutine read_files_4_fline_ctl(fline_ctls)
!      subroutine bcast_files_4_fline_ctl(fline_ctls)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  fieldline  1
!!      file  fieldline  'ctl_fline_magne'
!!    end array fieldline
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module t_control_data_flines
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use t_control_data_4_fline
!
      implicit  none
!
!
      character(len=kchara), parameter :: hd_fline_ctl =  'fieldline'
!
      type fieldline_controls
        integer(kind = kint) :: num_fline_ctl = 0
        character(len = kchara), allocatable :: fname_fline_ctl(:)
        type(fline_ctl), allocatable :: fline_ctl_struct(:)
      end type fieldline_controls
!
!      fieldline flag
!
      integer (kind=kint) :: i_fline_ctl =  0
!
      private :: i_fline_ctl
      private :: alloc_fline_ctl_struct
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_fline_ctl_struct(fline_ctls)
!
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      allocate(fline_ctls%fname_fline_ctl(fline_ctls%num_fline_ctl))
      allocate(fline_ctls%fline_ctl_struct(fline_ctls%num_fline_ctl))
!
      end subroutine alloc_fline_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_fline_fhead_ctl(fline_ctls)
!
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      deallocate(fline_ctls%fname_fline_ctl)
      deallocate(fline_ctls%fline_ctl_struct)
!
      end subroutine dealloc_fline_fhead_ctl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_files_4_fline_ctl(fline_ctls)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(fieldline_controls), intent(inout) :: fline_ctls
!
!
      if (i_fline_ctl .gt. 0) return
!
      call alloc_fline_ctl_struct(fline_ctls)
      do
        call load_ctl_label_and_line
!
        call find_control_end_array_flag(hd_fline_ctl,                  &
     &      fline_ctls%num_fline_ctl, i_fline_ctl)
        if(i_fline_ctl .ge. fline_ctls%num_fline_ctl) exit
!
        if(right_file_flag(hd_fline_ctl) .gt. 0) then
          call read_file_names_from_ctl_line(fline_ctls%num_fline_ctl,  &
     &        i_fline_ctl, fline_ctls%fname_fline_ctl)
        end if
!
        if(right_begin_flag(hd_fline_ctl) .gt. 0) then
          i_fline_ctl = i_fline_ctl + 1
          fline_ctls%fname_fline_ctl(i_fline_ctl) = 'NO_FILE'
          call read_field_line_ctl                                      &
     &        (hd_fline_ctl, fline_ctls%fline_ctl_struct(i_fline_ctl))
        end if
      end do
!
      end subroutine read_files_4_fline_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_files_4_fline_ctl(fline_ctls)
!
      use calypso_mpi
!
      type(fieldline_controls), intent(inout) :: fline_ctls
      integer (kind=kint) :: i_fline
!
!
      call MPI_BCAST(fline_ctls%num_fline_ctl,  ione,                   &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      if(fline_ctls%num_fline_ctl .le. 0) return
!
      if(my_rank .gt. 0)  call alloc_fline_ctl_struct(fline_ctls)
!
      call MPI_BCAST                                                    &
     &  (fline_ctls%fname_fline_ctl, (kchara*fline_ctls%num_fline_ctl), &
     &   CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
      do i_fline = 1, fline_ctls%num_fline_ctl
        if(fline_ctls%fname_fline_ctl(i_fline) .eq. 'NO_FILE') then
          call bcast_field_line_ctl                                     &
     &       (fline_ctls%fline_ctl_struct(i_fline))
        end if
      end do
!
      end subroutine bcast_files_4_fline_ctl
!
!   --------------------------------------------------------------------
!
      end module t_control_data_flines
