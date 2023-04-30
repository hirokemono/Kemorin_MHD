!>@file   t_control_data_isosurfaces.f90
!!@brief  module t_control_data_isosurfaces
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for isosurfaces
!!
!!@verbatim
!!      subroutine bcast_files_4_iso_ctl(iso_ctls)
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!      subroutine dealloc_iso_ctl_stract(iso_ctls)
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!
!!       subroutine add_fields_4_isos_to_fld_ctl(iso_ctls, field_ctl)
!!        type(isosurf_controls), intent(in) :: iso_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array isosurface_ctl     2
!!      file   isosurface_ctl   'ctl_iso_p_n1e4'
!!      file   isosurface_ctl   'ctl_iso_p_p1e4'
!!    end array isosurface_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_isosurfaces
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_iso
!
      implicit  none
!
!
      type isosurf_controls
        integer(kind = kint) :: num_iso_ctl = 0
        type(iso_ctl), allocatable :: iso_ctl_struct(:)
      end type isosurf_controls
!
!
      private :: dup_control_4_isos
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_files_4_iso_ctl(iso_ctls)
!
      use t_control_data_4_iso
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(isosurf_controls), intent(inout) :: iso_ctls
      integer (kind=kint) :: i_iso
!
!
      call calypso_mpi_bcast_one_int(iso_ctls%num_iso_ctl, 0)
      if(iso_ctls%num_iso_ctl .le. 0) return
!
      if(my_rank .gt. 0) call alloc_iso_ctl_stract(iso_ctls)
!
      do i_iso = 1, iso_ctls%num_iso_ctl
        call bcast_iso_control_data(iso_ctls%iso_ctl_struct(i_iso))
      end do
!
      end subroutine bcast_files_4_iso_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_iso_ctl_stract(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      if(allocated(iso_ctls%iso_ctl_struct)) then
        deallocate(iso_ctls%iso_ctl_struct)
      end if
      iso_ctls%num_iso_ctl = 0
!
      end subroutine dealloc_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine add_fields_4_isos_to_fld_ctl(iso_ctls, field_ctl)
!
      use t_control_array_character3
!
      type(isosurf_controls), intent(in) :: iso_ctls
      type(ctl_array_c3), intent(inout) :: field_ctl
      integer(kind = kint) :: i_iso
!
!
      do i_iso = 1, iso_ctls%num_iso_ctl
        call add_fields_4_iso_to_fld_ctl                                &
     &     (iso_ctls%iso_ctl_struct(i_iso), field_ctl)
      end do
!
      end subroutine add_fields_4_isos_to_fld_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_new_isosurface_control(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      type(isosurf_controls) :: tmp_iso_c
!
!
      tmp_iso_c%num_iso_ctl = iso_ctls%num_iso_ctl
      call alloc_iso_ctl_stract(tmp_iso_c)
      call dup_control_4_isos                                           &
     &    (tmp_iso_c%num_iso_ctl, iso_ctls, tmp_iso_c)
!
      call dealloc_cont_dat_4_isos                                      &
     &   (iso_ctls%num_iso_ctl, iso_ctls%iso_ctl_struct)
      call dealloc_iso_ctl_stract(iso_ctls)
!
      iso_ctls%num_iso_ctl = tmp_iso_c%num_iso_ctl + 1
      call alloc_iso_ctl_stract(iso_ctls)
!
      call dup_control_4_isos                                           &
     &   (tmp_iso_c%num_iso_ctl, tmp_iso_c, iso_ctls)
!
      call dealloc_cont_dat_4_isos                                      &
     &   (tmp_iso_c%num_iso_ctl, tmp_iso_c%iso_ctl_struct)
      call dealloc_iso_ctl_stract(tmp_iso_c)
!
      end subroutine append_new_isosurface_control
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_4_isos                                     &
     &         (num_iso, org_iso_ctls, new_iso_ctls)
!
      integer(kind = kint), intent(in) :: num_iso
      type(isosurf_controls), intent(in) :: org_iso_ctls
      type(isosurf_controls), intent(inout) :: new_iso_ctls
!
      integer(kind = kint) :: i
!
      do i = 1, num_iso
        call dup_control_4_iso(org_iso_ctls%iso_ctl_struct(i),          &
            new_iso_ctls%iso_ctl_struct(i))
      end do
!
      end subroutine dup_control_4_isos
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_iso_ctl_stract(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      allocate(iso_ctls%iso_ctl_struct(iso_ctls%num_iso_ctl))
!
      end subroutine alloc_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cont_dat_4_isos(num_iso, iso_c)
!
      integer(kind = kint), intent(in) :: num_iso
      type(iso_ctl), intent(inout) :: iso_c(num_iso)
!
      integer(kind = kint) :: i
!
      do i = 1, num_iso
        call dealloc_cont_dat_4_iso(iso_c(i))
      end do
!
      end subroutine dealloc_cont_dat_4_isos
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_isosurfaces
