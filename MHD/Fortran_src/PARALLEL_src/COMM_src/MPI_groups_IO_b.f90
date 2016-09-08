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
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, surf_grp_IO)
!!
!!      subroutine mpi_write_grp_data_b                                 &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, group_IO)
!!      subroutine mpi_write_surf_grp_data_b                            &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl, surf_grp_IO)
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
      use t_calypso_mpi_IO_param
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      implicit none
!
      type(calypso_MPI_IO_params), private :: IO_param
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
        call mpi_read_mul_charahead_b                                   &
     &     (id_file, ioff_gl, group_IO%num_grp, group_IO%grp_name)
!
        call alloc_istack_merge(id_rank, nprocs_in, IO_param)
        call set_istack_4_parallell_data(group_IO%num_grp, IO_param)
!
        call mpi_read_integer_stack_b                                   &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      group_IO%num_grp, group_IO%istack_grp,                      &
     &      group_IO%num_item, IO_param%istack_merged)
!
        call allocate_grp_type_item(group_IO)
!
        call set_istack_4_parallell_data(group_IO%num_item, IO_param)
        call mpi_read_int_vector_b                                      &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      group_IO%num_item, group_IO%item_grp,                       &
     &      IO_param%istack_merged)
        call dealloc_istack_merge(IO_param)
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
     &         (id_file, nprocs_in, id_rank, ioff_gl, surf_grp_IO)
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
        call mpi_read_mul_charahead_b(id_file, ioff_gl,                 &
     &      surf_grp_IO%num_grp, surf_grp_IO%grp_name)
!
        call alloc_istack_merge(id_rank, nprocs_in, IO_param)
        call set_istack_4_parallell_data(surf_grp_IO%num_grp, IO_param)
!
        call mpi_read_integer_stack_b                                   &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      surf_grp_IO%num_grp, surf_grp_IO%istack_grp,                &
     &      surf_grp_IO%num_item, IO_param%istack_merged)
!
        call allocate_sf_grp_type_item(surf_grp_IO)
!
        nitem = 2 * surf_grp_IO%num_item
        call set_istack_4_parallell_data(nitem, IO_param)
!
        call mpi_read_int_vector_b                                      &
     &     (id_file, nprocs_in, id_rank, ioff_gl, nitem,                &
     &      surf_grp_IO%item_sf_grp, IO_param%istack_merged)
        call dealloc_istack_merge(IO_param)
      else
        call allocate_sf_grp_type_item(surf_grp_IO)
      end if
!
      end subroutine mpi_read_surf_grp_data_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_grp_data_b                                   &
     &         (id_file, nprocs_in, id_rank, ioff_gl, group_IO)
!
      integer, intent(in) ::  id_file
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(group_data), intent(inout) :: group_IO
!
!
      call mpi_write_one_inthead_b                                      &
     &   (id_file, ioff_gl, group_IO%num_grp)
!
      if (group_IO%num_grp .gt. 0) then
        call mpi_write_mul_charahead_b                                  &
     &     (id_file, ioff_gl, group_IO%num_grp, group_IO%grp_name)
!
        call alloc_istack_merge(id_rank, nprocs_in, IO_param)
        call set_istack_4_parallell_data(group_IO%num_grp, IO_param)
!
        call mpi_write_integer_stack_b                                  &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      group_IO%num_grp, group_IO%istack_grp,                      &
     &      IO_param%istack_merged)
!
        call set_istack_4_parallell_data(group_IO%num_item, IO_param)
!
        call mpi_write_int_vector_b                                     &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      group_IO%num_item, group_IO%item_grp,                       &
     &      IO_param%istack_merged)
        call dealloc_istack_merge(IO_param)
      end if
!
      call deallocate_grp_type(group_IO)
!
      end subroutine mpi_write_grp_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_surf_grp_data_b                              &
     &         (id_file, nprocs_in, id_rank, ioff_gl, surf_grp_IO)
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
      call mpi_write_one_inthead_b                                      &
     &   (id_file, ioff_gl, surf_grp_IO%num_grp)
!
      if (surf_grp_IO%num_grp .gt. 0) then
        call mpi_write_mul_charahead_b(id_file, ioff_gl,                &
     &      surf_grp_IO%num_grp, surf_grp_IO%grp_name)
!
        call alloc_istack_merge(id_rank, nprocs_in, IO_param)
        call set_istack_4_parallell_data(surf_grp_IO%num_grp, IO_param)
!
        call mpi_write_integer_stack_b                                  &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      surf_grp_IO%num_grp, surf_grp_IO%istack_grp,                &
     &      IO_param%istack_merged)
!
        nitem = 2 * surf_grp_IO%num_item
        call set_istack_4_parallell_data(nitem, IO_param)
!
        call mpi_write_int_vector_b                                     &
     &     (id_file, nprocs_in, id_rank, ioff_gl,                       &
     &      nitem, surf_grp_IO%item_sf_grp, IO_param%istack_merged)
        call dealloc_istack_merge(IO_param)
      end if
!
      call deallocate_sf_grp_type(surf_grp_IO)
!
      end subroutine mpi_write_surf_grp_data_b
!
!------------------------------------------------------------------
!
      end module MPI_groups_IO_b
