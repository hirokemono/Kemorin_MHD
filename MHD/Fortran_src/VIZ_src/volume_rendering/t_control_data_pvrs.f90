!>@file   t_control_data_pvrs.f90
!!@brief  module t_control_data_pvrs
!!
!!@author  H. Matsui
!!@date Programmed in July, 2006
!
!>@brief structure of control data for multiple PVRs
!!
!!@verbatim
!!      subroutine read_files_4_pvr_ctl                                 &
!!     &         (id_control, hd_pvr_ctl, pvr_ctls, c_buf)
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
     &                    :: hd_pvr_colordef =  'pvr_color_ctl'
!
      type volume_rendering_controls
        integer(kind = kint) :: num_pvr_ctl = 0
        character(len = kchara), allocatable :: fname_pvr_ctl(:)
        type(pvr_parameter_ctl), allocatable :: pvr_ctl_type(:)
      end type volume_rendering_controls
!
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
      subroutine dealloc_pvr_ctl_struct(pvr_ctls)
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
!
      integer(kind = kint) :: i
!
!
      do i = 1, pvr_ctls%num_pvr_ctl
        call deallocate_cont_dat_pvr(pvr_ctls%pvr_ctl_type(i))
      end do
!
      deallocate(pvr_ctls%pvr_ctl_type)
      deallocate(pvr_ctls%fname_pvr_ctl)
!
      end subroutine dealloc_pvr_ctl_struct
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_pvr_ctl                                   &
     &         (id_control, hd_pvr_ctl, pvr_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_pvr_ctl
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(allocated(pvr_ctls%fname_pvr_ctl)) return
      pvr_ctls%num_pvr_ctl = 0
      call alloc_pvr_ctl_struct(pvr_ctls)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_pvr_ctl)) exit
!
        if(check_file_flag(c_buf, hd_pvr_ctl)) then
          call append_new_pvr_ctl_struct(pvr_ctls)
          pvr_ctls%fname_pvr_ctl(pvr_ctls%num_pvr_ctl)                  &
     &        = third_word(c_buf)
        end if
!
        if(right_begin_flag(hd_pvr_ctl) .gt. 0) then
          call append_new_pvr_ctl_struct(pvr_ctls)
          pvr_ctls%fname_pvr_ctl(pvr_ctls%num_pvr_ctl) = 'NO_FILE'
          call read_pvr_ctl(id_control, hd_pvr_ctl, hd_pvr_colordef,    &
     &        pvr_ctls%pvr_ctl_type(pvr_ctls%num_pvr_ctl), c_buf)
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
!   --------------------------------------------------------------------
!
      subroutine append_new_pvr_ctl_struct(pvr_ctls)
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
!
      type(volume_rendering_controls) :: tmp_pvrs_c
!
!
      tmp_pvrs_c%num_pvr_ctl = pvr_ctls%num_pvr_ctl
      call alloc_pvr_ctl_struct(tmp_pvrs_c)
      call dup_pvr_ctl_struct                                           &
     &   (pvr_ctls%num_pvr_ctl, pvr_ctls, tmp_pvrs_c)
      call dealloc_pvr_ctl_struct(pvr_ctls)
!
      pvr_ctls%num_pvr_ctl = tmp_pvrs_c%num_pvr_ctl + 1
      call alloc_pvr_ctl_struct(pvr_ctls)
      call dup_pvr_ctl_struct                                           &
     &   (tmp_pvrs_c%num_pvr_ctl, tmp_pvrs_c, pvr_ctls)
      call dealloc_pvr_ctl_struct(tmp_pvrs_c)
!
      end subroutine append_new_pvr_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_ctl_struct(num_pvr, org_pvrs_c, new_pvrs_c)
!
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: num_pvr
      type(volume_rendering_controls), intent(in) :: org_pvrs_c
      type(volume_rendering_controls), intent(inout) :: new_pvrs_c
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_pvr
        new_pvrs_c%fname_pvr_ctl(i) = org_pvrs_c%fname_pvr_ctl(i)
        call dup_pvr_ctl(org_pvrs_c%pvr_ctl_type(i),                    &
     &                   new_pvrs_c%pvr_ctl_type(i))
      end do
!
      end subroutine dup_pvr_ctl_struct
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_pvrs
