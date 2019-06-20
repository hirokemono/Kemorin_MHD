!>@file   t_control_data_pvr_isosurfs.f90
!!@brief  module t_control_data_pvr_isosurfs
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_pvr_isosurfs_ctl                                &
!!     &         (id_control, hd_block, pvr_isos_c, c_buf)
!!      subroutine bcast_pvr_isosurfs_ctl(pvr_isos_c)
!!
!!      subroutine alloc_pvr_isosurfs_ctl(pvr_isos_c)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dup_pvr_isosurfs_ctl(org_pvr_iso_c, new_pvr_isos_c)
!!        type(pvr_isosurfs_ctl), intent(in) :: org_pvr_iso_c
!!        type(pvr_isosurfs_ctl), intent(inout) :: new_pvr_isos_c
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  array isosurface_ctl  2
!!    begin isosurface_ctl
!!      isosurf_value       0.3
!!      opacity_ctl         0.9
!!      surface_direction   normal
!!    end isosurface_ctl
!!     ...
!!  end array isosurface_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_pvr_isosurfs
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_elements
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_isosurf_ctl
        type(read_character_item) :: isosurf_type_ctl
        type(read_real_item) :: isosurf_value_ctl
        type(read_real_item) :: opacity_ctl
      end type pvr_isosurf_ctl
!
      type pvr_isosurfs_ctl
        integer(kind = kint) :: num_pvr_iso_ctl = 0
        type(pvr_isosurf_ctl), allocatable :: pvr_iso_ctl(:)
      end type pvr_isosurfs_ctl
!
!     3rd level for isosurface
!
      character(len=kchara) :: hd_isosurf_value = 'isosurf_value'
      character(len=kchara) :: hd_pvr_opacity =   'opacity_ctl'
      character(len=kchara) :: hd_iso_direction = 'surface_direction'
!
!
      private :: hd_isosurf_value, hd_pvr_opacity, hd_iso_direction
!
      private :: alloc_pvr_isosurfs_ctl, read_pvr_isosurface_ctl
      private :: bcast_pvr_isosurface_ctl, append_new_pvr_isosurf_ctl
      private :: copy_pvr_isosurfs_ctl, reset_pvr_isosurfs_ctl
      private :: dup_pvr_isosurface_ctl, reset_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_isosurfs_ctl                                  &
     &         (id_control, hd_block, pvr_isos_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(allocated(pvr_isos_c%pvr_iso_ctl)) return
      pvr_isos_c%num_pvr_iso_ctl = 0
      call alloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_new_pvr_isosurf_ctl(pvr_isos_c)
          call read_pvr_isosurface_ctl(id_control, hd_block,            &
     &        pvr_isos_c%pvr_iso_ctl(pvr_isos_c%num_pvr_iso_ctl),       &
     &        c_buf)
        end if
      end do
!
      end subroutine read_pvr_isosurfs_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_pvr_isosurfs_ctl(pvr_isos_c)
!
      use calypso_mpi
      use bcast_control_arrays
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!
      integer(kind = kint) :: i
!
!
      call MPI_BCAST(pvr_isos_c%num_pvr_iso_ctl,  1,                    &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      if(pvr_isos_c%num_pvr_iso_ctl .gt. 0 .and. my_rank .gt. 0) then
        call alloc_pvr_isosurfs_ctl(pvr_isos_c)
      end if
!
      do i = 1, pvr_isos_c%num_pvr_iso_ctl
        call bcast_pvr_isosurface_ctl(pvr_isos_c%pvr_iso_ctl(i))
      end do
!
      end subroutine bcast_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!
!
      call reset_pvr_isosurfs_ctl                                       &
     &    (pvr_isos_c%num_pvr_iso_ctl, pvr_isos_c%pvr_iso_ctl)
      deallocate(pvr_isos_c%pvr_iso_ctl)
!
      pvr_isos_c%num_pvr_iso_ctl = 0
!
      end subroutine dealloc_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!
!
      allocate(pvr_isos_c%pvr_iso_ctl(pvr_isos_c%num_pvr_iso_ctl))
      call reset_pvr_isosurfs_ctl                                       &
     &    (pvr_isos_c%num_pvr_iso_ctl, pvr_isos_c%pvr_iso_ctl)
!
      end subroutine alloc_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_isosurface_ctl                                &
     &         (id_control, hd_block, pvr_iso_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type(c_buf, hd_iso_direction,               &
     &      pvr_iso_ctl%isosurf_type_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_isosurf_value, pvr_iso_ctl%isosurf_value_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_pvr_opacity, pvr_iso_ctl%opacity_ctl)
      end do
!
      end subroutine read_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_isosurface_ctl(pvr_iso_ctl)
!
      use bcast_control_arrays
!
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!
!
        call bcast_ctl_type_c1(pvr_iso_ctl%isosurf_type_ctl)
        call bcast_ctl_type_r1(pvr_iso_ctl%isosurf_value_ctl)
        call bcast_ctl_type_r1(pvr_iso_ctl%opacity_ctl)
!
      end subroutine bcast_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_new_pvr_isosurf_ctl(pvr_isos_c)
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!
      type(pvr_isosurfs_ctl) :: tmp_pvr_isos
!
!
      tmp_pvr_isos%num_pvr_iso_ctl = pvr_isos_c%num_pvr_iso_ctl
      call alloc_pvr_isosurfs_ctl(tmp_pvr_isos)
      call copy_pvr_isosurfs_ctl(tmp_pvr_isos%num_pvr_iso_ctl,          &
     &    pvr_isos_c%pvr_iso_ctl, tmp_pvr_isos%pvr_iso_ctl)
!
      call dealloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      pvr_isos_c%num_pvr_iso_ctl = tmp_pvr_isos%num_pvr_iso_ctl + 1
      call alloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      call copy_pvr_isosurfs_ctl(tmp_pvr_isos%num_pvr_iso_ctl,          &
     &    tmp_pvr_isos%pvr_iso_ctl, pvr_isos_c%pvr_iso_ctl(1))
!
      call dealloc_pvr_isosurfs_ctl(tmp_pvr_isos)
!
      end subroutine append_new_pvr_isosurf_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dup_pvr_isosurfs_ctl(org_pvr_iso_c, new_pvr_isos_c)
!
      type(pvr_isosurfs_ctl), intent(in) :: org_pvr_iso_c
      type(pvr_isosurfs_ctl), intent(inout) :: new_pvr_isos_c
!
!
      new_pvr_isos_c%num_pvr_iso_ctl = org_pvr_iso_c%num_pvr_iso_ctl
      call alloc_pvr_isosurfs_ctl(new_pvr_isos_c)
      call copy_pvr_isosurfs_ctl(org_pvr_iso_c%num_pvr_iso_ctl,         &
     &    org_pvr_iso_c%pvr_iso_ctl, new_pvr_isos_c%pvr_iso_ctl)
!
      end subroutine dup_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_pvr_isosurfs_ctl                                  &
     &         (num_pvr_iso, org_pvr_iso_c, new_pvr_iso_c)
!
      integer(kind = kint), intent(in) :: num_pvr_iso
      type(pvr_isosurf_ctl), intent(in) :: org_pvr_iso_c(num_pvr_iso)
      type(pvr_isosurf_ctl), intent(inout) :: new_pvr_iso_c(num_pvr_iso)
!
      integer(kind = kint) :: i
!
      do i = 1, num_pvr_iso
        call dup_pvr_isosurface_ctl(org_pvr_iso_c(i), new_pvr_iso_c(i))
      end do
!
      end subroutine copy_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_isosurfs_ctl(num_pvr_iso, pvr_iso_ctl)
!
      integer(kind = kint), intent(in) :: num_pvr_iso
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl(num_pvr_iso)
!
      integer(kind = kint) :: i
!
      do i = 1, num_pvr_iso
        call reset_pvr_isosurface_ctl(pvr_iso_ctl(i))
      end do
!
      end subroutine reset_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_isosurface_ctl(org_pvr_iso_c, new_pvr_iso_c)
!
      use copy_control_elements
!
      type(pvr_isosurf_ctl), intent(in) :: org_pvr_iso_c
      type(pvr_isosurf_ctl), intent(inout) :: new_pvr_iso_c
!
!
      call copy_chara_ctl(org_pvr_iso_c%isosurf_type_ctl,               &
     &                    new_pvr_iso_c%isosurf_type_ctl)
      call copy_real_ctl(org_pvr_iso_c%isosurf_value_ctl,               &
     &                   new_pvr_iso_c%isosurf_value_ctl)
      call copy_real_ctl(org_pvr_iso_c%opacity_ctl,                     &
     &                   new_pvr_iso_c%opacity_ctl)
!
      end subroutine dup_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_isosurface_ctl(pvr_iso_ctl)
!
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!
!
      pvr_iso_ctl%isosurf_type_ctl%iflag =  0
      pvr_iso_ctl%isosurf_value_ctl%iflag = 0
      pvr_iso_ctl%opacity_ctl%iflag =       0
!
      end subroutine reset_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_pvr_isosurfs
