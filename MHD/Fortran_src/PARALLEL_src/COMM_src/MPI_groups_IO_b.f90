!>@file  MPI_groups_IO_b.f90
!!       module MPI_groups_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in July, 2007
!
!> @brief Binary output routines for group data
!!
!!@verbatim
!!      subroutine mpi_read_group_data_b                                &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, group_IO)
!!      subroutine mpi_read_surf_grp_data_b                             &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl,surf_grp_IO)
!!
!!      subroutine mpi_write_grp_data_b(id_file, ioff_gl, group_IO)
!!      subroutine mpi_write_surf_grp_data_b                            &
!!     &         (id_file, ioff_gl, surf_grp_IO)
!!        type(group_data), intent(inout) :: group_IO
!!        type(surface_group_data), intent(inout) :: surf_grp_IO
!!@endverbatim
!
      module MPI_groups_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use t_group_data
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_group_data_b                                  &
     &         (id_file, nprocs_in, id_rank, ioff_gl, group_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(group_data), intent(inout) :: group_IO
!
!
      call mpi_read_one_inthead_b                                       &
     &   (id_file, ioff_gl, group_IO%num_grp)
      call allocate_grp_type_num(group_IO)
!
      if (group_IO%num_grp .gt. 0) then
        call mpi_read_integer_stack_b(nprocs_in, id_rank, ioff_gl,      &
     &      group_IO%num_grp, group_IO%istack_grp, group_IO%num_item)
        call mpi_read_mul_charahead_b                                   &
     &     (id_file, ioff_gl, group_IO%num_grp, group_IO%grp_name)
!
        call allocate_grp_type_item(group_IO)
!
        call mpi_read_int_vector_b                                      &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      group_IO%num_item, group_IO%item_grp)
      else
        group_IO%num_item = 0
        call allocate_grp_type_item(group_IO)
      end if
!
      end subroutine mpi_read_group_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_surf_grp_data_b                               &
     &         (id_file, nprocs_in, id_rank, ioff_gl,surf_grp_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint) :: nitem
!
!
      call mpi_read_one_inthead_b                                       &
     &   (id_file, ioff_gl, surf_grp_IO%num_grp)
      call allocate_sf_grp_type_num(surf_grp_IO)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call mpi_read_integer_stack_b(nprocs_in, id_rank, ioff_gl,      &
     &      surf_grp_IO%num_grp, surf_grp_IO%istack_grp,                &
     &      surf_grp_IO%num_item)
        call mpi_read_mul_charahead_b(id_file, ioff_gl,                 &
     &      surf_grp_IO%num_grp, surf_grp_IO%grp_name)
!
        call allocate_sf_grp_type_item(surf_grp_IO)
!
        nitem = 2 * surf_grp_IO%num_item
        call mpi_read_int_vector_b                                      &
     &     (id_file, nprocs_in, id_rank, ioff_gl, nitem,                &
     &      surf_grp_IO%item_sf_grp)
      else
        call allocate_sf_grp_type_item(surf_grp_IO)
      end if
!
      end subroutine mpi_read_surf_grp_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_grp_data_b(id_file, ioff_gl, group_IO)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(group_data), intent(inout) :: group_IO
!
!
      call mpi_write_one_inthead_b                                      &
     &   (id_file, ioff_gl, group_IO%num_grp)
      call mpi_write_integer_stack_b                                    &
     &   (id_file, ioff_gl, group_IO%num_grp, group_IO%istack_grp)
      call mpi_write_mul_charahead_b                                    &
     &   (id_file, ioff_gl, group_IO%num_grp, group_IO%grp_name)
      call mpi_write_int_vector_b                                       &
     &   (id_file, ioff_gl, group_IO%num_item, group_IO%item_grp)
!
      call deallocate_grp_type(group_IO)
!
      end subroutine mpi_write_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_surf_grp_data_b                              &
     &         (id_file, ioff_gl, surf_grp_IO)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(surface_group_data), intent(inout) :: surf_grp_IO
!
      integer(kind = kint) :: nitem
!
!
      call mpi_write_one_inthead_b                                      &
     &   (id_file, ioff_gl, surf_grp_IO%num_grp)
      call mpi_write_integer_stack_b(id_file, ioff_gl,                  &
     &    surf_grp_IO%num_grp, surf_grp_IO%istack_grp)
      call mpi_write_mul_charahead_b                                    &
     &   (id_file, ioff_gl, surf_grp_IO%num_grp, surf_grp_IO%grp_name)
!
      nitem = 2 * surf_grp_IO%num_item
      call mpi_write_int_vector_b                                       &
     &   (id_file, ioff_gl, nitem, surf_grp_IO%item_sf_grp)
!
      call deallocate_sf_grp_type(surf_grp_IO)
!
      end subroutine mpi_write_surf_grp_data_b
!
!------------------------------------------------------------------
!
      end module MPI_groups_IO_b
