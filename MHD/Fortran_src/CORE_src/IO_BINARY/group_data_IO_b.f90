!
!      module group_data_IO_b
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_group_stack_b(id_file, ngrp, ntot, istack)
!      subroutine read_group_item_b(id_file, ngrp, ntot,                &
!     &          istack, name, item)
!      subroutine read_surface_group_item_b(id_file, ngrp, ntot,        &
!     &          istack, name, item_sf)
!
!      subroutine write_group_data_b(id_file, ngrp, ntot, istack, name, &
!     &          item)
!      subroutine write_surface_group_data_b(id_file, ngrp, ntot,       &
!     &          istack, name, item_sf)
!
      module group_data_IO_b
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_group_stack_b(id_file, ngrp, ntot, istack)
!
      use stack_array_IO
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ngrp
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:ngrp)
!
!
      call read_arrays_for_stacks_b(id_file, ngrp, izero, ntot, istack)
!
      end subroutine read_group_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine read_group_item_b(id_file, ngrp, ntot,                 &
     &          istack, name, item)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
!
      integer(kind = kint), intent(inout) :: item(ntot)
      character(len = kchara), intent(inout) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, ied
!
!
      if (ngrp .gt. 0) then
        do i = 1, ngrp
          ist = istack(i-1) + 1
          ied = istack(i  )
!
          read(id_file) name(i)
          read(id_file) item(ist:ied)
        end do
      end if
      end subroutine read_group_item_b
!
! -----------------------------------------------------------------------
!
      subroutine read_surface_group_item_b(id_file, ngrp, ntot,         &
     &          istack, name, item_sf)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
!
      integer(kind = kint), intent(inout) :: item_sf(ntot,2)
      character(len = kchara), intent(inout) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, ied
!
!
      if (ngrp .gt. 0) then
        do i = 1, ngrp
          ist = istack(i-1) + 1
          ied = istack(i  )
!
          read(id_file) name(i)
          read(id_file) item_sf(ist:ied,1)
          read(id_file) item_sf(ist:ied,2)
        end do
      end if
!
      end subroutine read_surface_group_item_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_group_data_b(id_file, ngrp, ntot, istack, name,  &
     &          item)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
      integer(kind = kint), intent(in) :: item(ntot)
      character(len = kchara), intent(in) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, ied
!
!
      write(id_file) ngrp
!
      if (ngrp .gt. 0) then
        write(id_file) istack(1:ngrp)
!
        do i = 1, ngrp
          ist = istack(i-1) + 1
          ied = istack(i  )
          write(id_file) name(i)
          write(id_file) item(ist:ied)
        end do
      end if
!
      end subroutine write_group_data_b
!
! -----------------------------------------------------------------------
!
      subroutine write_surface_group_data_b(id_file, ngrp, ntot,        &
     &          istack, name, item_sf)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
      integer(kind = kint), intent(in) :: item_sf(ntot,2)
      character(len = kchara), intent(in) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, ied
!
!
      write(id_file) ngrp
!
      if(ngrp .gt. 0) then
        write(id_file) istack(1:ngrp)
!
        do i = 1, ngrp
          ist = istack(i-1) + 1
          ied = istack(i  )
          write(id_file) name(i)
          write(id_file) item_sf(ist:ied,1)
          write(id_file) item_sf(ist:ied,2)
        end do
      end if
!
      end subroutine write_surface_group_data_b
!
! -----------------------------------------------------------------------
!
      end module group_data_IO_b
