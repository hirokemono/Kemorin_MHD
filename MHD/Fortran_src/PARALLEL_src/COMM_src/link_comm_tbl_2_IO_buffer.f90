!>@file   link_comm_tbl_2_IO_buffer.f90
!!@brief  module link_comm_tbl_2_IO_buffer
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Structure for grid and comm table for spherical transform
!!
!!@verbatim
!!      subroutine link_num_neib_4_mpi_IO(nloop, comm_IO, i_array)
!!      subroutine link_neib_id_4_mpi_IO(nloop, comm_IO, i_array)
!!      subroutine link_import_stack_4_mpi_IO(nloop, comm_IO, i_array)
!!      subroutine link_export_stack_4_mpi_IO(nloop, comm_IO, i_array)
!!      subroutine link_import_item_4_mpi_IO(nloop, comm_IO, i_array)
!!      subroutine link_export_item_4_mpi_IO(nloop, comm_IO, i_array)
!!        type(communication_table), intent(in) :: comm_IO(nloop)
!!        type(intarray_IO),  intent(inout) :: i_array(nloop)
!!@endverbatim
!
      module link_comm_tbl_2_IO_buffer
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_comm_table
      use t_calypso_mpi_IO_param
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine link_num_neib_4_mpi_IO(nloop, comm_IO, i_array)
!
      use t_comm_table
!
      integer(kind = kint), intent(in) :: nloop
      type(communication_table), intent(in) :: comm_IO(nloop)
      type(intarray_IO),  intent(inout) :: i_array(nloop)
!
      integer(kind = kint), target :: itmp_IO(1)
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        allocate(i_array(i)%i_IO(1))
        i_array(i)%num =    ione
        i_array(i)%i_IO(1) = comm_IO(i)%num_neib
      end do
!
      end subroutine link_num_neib_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_neib_id_4_mpi_IO(nloop, comm_IO, i_array)
!
      use t_comm_table
!
      integer(kind = kint), intent(in) :: nloop
      type(communication_table), intent(in) :: comm_IO(nloop)
      type(intarray_IO),  intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i_array(i)%num =   comm_IO(i)%num_neib
        i_array(i)%i_IO => comm_IO(i)%id_neib
      end do
!
      end subroutine link_neib_id_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_import_stack_4_mpi_IO(nloop, comm_IO, i_array)
!
      use t_comm_table
!
      integer(kind = kint), intent(in) :: nloop
      type(communication_table), intent(in) :: comm_IO(nloop)
      type(intarray_IO),  intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i_array(i)%num =   comm_IO(i)%num_neib
        if(comm_IO(i)%num_neib .gt. 0) then
          i_array(i)%i_IO                                               &
     &         => comm_IO(i)%istack_import(1:comm_IO(i)%num_neib)
        end if
      end do
!
      end subroutine link_import_stack_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_export_stack_4_mpi_IO(nloop, comm_IO, i_array)
!
      use t_comm_table
!
      integer(kind = kint), intent(in) :: nloop
      type(communication_table), intent(in) :: comm_IO(nloop)
      type(intarray_IO),  intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i_array(i)%num =   comm_IO(i)%num_neib
        if(comm_IO(i)%num_neib .gt. 0) then
          i_array(i)%i_IO                                               &
     &     => comm_IO(i)%istack_export(1:comm_IO(i)%num_neib)
        end if
      end do 
!
      end subroutine link_export_stack_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_import_item_4_mpi_IO(nloop, comm_IO, i_array)
!
      use t_comm_table
!
      integer(kind = kint), intent(in) :: nloop
      type(communication_table), intent(in) :: comm_IO(nloop)
      type(intarray_IO),  intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i_array(i)%num =   comm_IO(i)%ntot_import
        i_array(i)%i_IO => comm_IO(i)%item_import
      end do
!
      end subroutine link_import_item_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_export_item_4_mpi_IO(nloop, comm_IO, i_array)
!
      use t_comm_table
!
      integer(kind = kint), intent(in) :: nloop
      type(communication_table), intent(in) :: comm_IO(nloop)
      type(intarray_IO),  intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i_array(i)%num =   comm_IO(i)%ntot_export
        i_array(i)%i_IO => comm_IO(i)%item_export
      end do
!
      end subroutine link_export_item_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      end module link_comm_tbl_2_IO_buffer
