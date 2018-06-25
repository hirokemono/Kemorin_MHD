!>@file   load_mesh_data_4_merge.f90
!!@brief  module load_mesh_data_4_merge
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2011
!
!>@brief Construct spectrum data for new spectrum domain
!!
!!@verbatim
!!      subroutine load_local_mesh_4_merge                              &
!!     &         (mesh_file, nprocs_in, mesh, group, ele_mesh)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_geometry), intent(inout) :: mesh(nprocs_in)
!!        type(mesh_groups), intent(inout) :: group(nprocs_in)
!!        type(element_geometry), intent(inout) :: ele_mesh(nprocs_in)
!!      subroutine load_local_node_ele_4_merge                          &
!!     &         (mesh_file, nprocs_in, mesh)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_geometry), intent(inout) :: mesh(nprocs_in)
!!        type(mesh_geometry) :: mesh_IO_m
!!      subroutine load_local_node_4_merge                              &
!!     &         (mesh_file, nprocs_in, mesh)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_geometry), intent(inout) :: mesh(nprocs_in)
!!
!!      subroutine load_local_FEM_field_4_merge                         &
!!     &         (istep_fld, fld_IO_param, nprocs_in, t_IO, fld_IO)
!!      subroutine load_old_FEM_restart_4_merge                         &
!!     &         (istep_fld, fld_IO_param, nprocs_in, t_IO, fld_IO)
!!        type(field_IO_params), intent(in) ::  fld_IO_param
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: field_IO(nprocs_in)
!!@endverbatim
!
      module load_mesh_data_4_merge
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use t_mesh_data
      use t_geometry_data
      use t_comm_table
      use t_group_data
      use t_time_data
      use t_field_data_IO
!
      implicit none
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_local_mesh_4_merge                                &
     &         (mesh_file, nprocs_in, mesh, group, ele_mesh)
!
      use mesh_MPI_IO_select
      use load_mesh_data
!
      integer(kind = kint), intent(in) ::  nprocs_in
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(inout) :: mesh(nprocs_in)
      type(mesh_groups), intent(inout) :: group(nprocs_in)
      type(element_geometry), intent(inout) :: ele_mesh(nprocs_in)
!
      type(mesh_data) :: fem_IO_m
      integer(kind = kint) :: id_rank, iloop, ip
!
!
      do iloop = 0, (nprocs_in-1) / nprocs
        id_rank = my_rank + iloop * nprocs
        ip = id_rank + 1
        call sel_mpi_read_mesh                                          &
     &     (nprocs_in, id_rank, mesh_file, fem_IO_m)
!
        if(id_rank .lt. nprocs_in) then
          call set_mesh(fem_IO_m, mesh(ip), group(ip),                  &
     &        ele_mesh(ip)%surf%nnod_4_surf,                            &
     &        ele_mesh(ip)%edge%nnod_4_edge)
        end if
      end do
!
      end subroutine load_local_mesh_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine load_local_node_ele_4_merge                            &
     &         (mesh_file, nprocs_in, mesh)
!
      use mesh_MPI_IO_select
      use load_mesh_data
!
      integer(kind = kint), intent(in) ::  nprocs_in
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(inout) :: mesh(nprocs_in)
!
      type(mesh_geometry) :: mesh_IO_m
      integer(kind = kint) :: id_rank, iloop, ip
!
!
      do iloop = 0, (nprocs_in-1) / nprocs
        id_rank = my_rank + iloop * nprocs
        ip = id_rank + 1
        call sel_mpi_read_mesh_geometry                                 &
     &     (nprocs_in, id_rank, mesh_file, mesh_IO_m)
!
        if(id_rank .lt. nprocs_in) then
          call set_mesh_geometry_data(mesh_IO_m,                        &
     &        mesh(ip)%nod_comm, mesh(ip)%node, mesh(ip)%ele)
        end if
      end do
!
      end subroutine load_local_node_ele_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine load_local_node_4_merge                                &
     &         (mesh_file, nprocs_in, mesh)
!
      use mesh_MPI_IO_select
      use load_mesh_data
!
      integer(kind = kint), intent(in) ::  nprocs_in
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(inout) :: mesh(nprocs_in)
!
      type(mesh_geometry) :: mesh_IO_m
      integer(kind = kint) :: id_rank, iloop, ip
!
!
      do iloop = 0, (nprocs_in-1) / nprocs
        id_rank = my_rank + iloop * nprocs
        ip = id_rank + 1
        call sel_mpi_read_geometry_size                                 &
     &     (nprocs_in, id_rank, mesh_file, mesh_IO_m)
!
        if(id_rank .lt. nprocs_in) then
          call set_node_geometry_data(mesh_IO_m, mesh(ip)%node)
        end if
      end do
!
      end subroutine load_local_node_4_merge
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine load_local_FEM_field_4_merge                           &
     &         (istep_fld, fld_IO_param, nprocs_in, t_IO, fld_IO)
!
      use mesh_MPI_IO_select
      use field_IO_select
!
      integer(kind = kint), intent(in) :: istep_fld
      integer(kind = kint), intent(in) ::  nprocs_in
      type(field_IO_params), intent(in) ::  fld_IO_param
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO(nprocs_in)
!
      integer(kind = kint) :: id_rank, iloop, ip
!
!
      do iloop = 0, (nprocs_in-1) / nprocs
        id_rank = my_rank + iloop * nprocs
        ip = id_rank + 1
        call sel_read_alloc_step_FEM_file(nprocs_in, id_rank,           &
     &      istep_fld, fld_IO_param, t_IO, fld_IO(ip))
      end do
!
      end subroutine load_local_FEM_field_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine load_old_FEM_restart_4_merge                           &
     &         (istep_fld, fld_IO_param, nprocs_in, t_IO, fld_IO)
!
      use mesh_MPI_IO_select
      use input_old_file_sel_4_zlib
!
      integer(kind = kint), intent(in) :: istep_fld
      integer(kind = kint), intent(in) ::  nprocs_in
      type(field_IO_params), intent(in) ::  fld_IO_param
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO(nprocs_in)
!
      integer(kind = kint) :: id_rank, iloop, ip
!
!
      do iloop = 0, (nprocs_in-1) / nprocs
        id_rank = my_rank + iloop * nprocs
        ip = id_rank + 1
        call sel_read_rst_file                                          &
     &     (id_rank, istep_fld, fld_IO_param, t_IO, fld_IO(ip))
      end do
!
      end subroutine load_old_FEM_restart_4_merge
!
! -----------------------------------------------------------------------
!
      end module load_mesh_data_4_merge
