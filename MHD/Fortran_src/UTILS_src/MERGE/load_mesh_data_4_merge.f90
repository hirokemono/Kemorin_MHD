!>@file   load_mesh_data_4_merge.f90
!!@brief  module load_mesh_data_4_merge
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2011
!
!>@brief Construct spectrum data for new spectrum domain
!!
!!@verbatim
!!      subroutine load_local_node_4_merge(mesh_file, num_pe, mesh)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_geometry), intent(inout) :: mesh(num_pe)
!!
!!      subroutine load_local_FEM_field_4_merge                         &
!!     &         (istep_fld, fld_IO_param, num_pe, t_IO, fld_IO)
!!      subroutine load_old_FEM_restart_4_merge                         &
!!     &         (istep_fld, fld_IO_param, num_pe, t_IO, fld_IO)
!!        type(field_IO_params), intent(in) ::  fld_IO_param
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: field_IO(num_pe)
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
      subroutine load_local_node_4_merge(mesh_file, num_pe, mesh)
!
      use mesh_MPI_IO_select
      use load_mesh_data
!
      integer, intent(in) ::  num_pe
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(inout) :: mesh(num_pe)
!
      type(mesh_geometry) :: mesh_IO_m
      integer :: id_rank, ip, iloop
!
!
      do iloop = 0, (num_pe-1) / nprocs
        id_rank = my_rank + iloop * nprocs
        ip = id_rank + 1
        call sel_mpi_read_geometry_size                                 &
     &     (num_pe, id_rank, mesh_file, mesh_IO_m)
!
        if(id_rank .lt. num_pe) then
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
     &         (istep_fld, fld_IO_param, num_pe, t_IO, fld_IO)
!
      use mesh_MPI_IO_select
      use field_IO_select
!
      integer(kind = kint), intent(in) :: istep_fld
      integer, intent(in) ::  num_pe
      type(field_IO_params), intent(in) ::  fld_IO_param
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO(num_pe)
!
      integer :: id_rank, ip, iloop
!
!
      do iloop = 0, (num_pe-1) / nprocs
        id_rank = my_rank + iloop * nprocs
        ip = id_rank + 1
        call sel_read_alloc_step_FEM_file(num_pe, id_rank,              &
     &      istep_fld, fld_IO_param, t_IO, fld_IO(ip))
      end do
!
      end subroutine load_local_FEM_field_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine load_old_FEM_restart_4_merge                           &
     &         (istep_fld, fld_IO_param, num_pe, t_IO, fld_IO)
!
      use mesh_MPI_IO_select
      use input_old_file_sel_4_zlib
!
      integer(kind = kint), intent(in) :: istep_fld
      integer, intent(in) ::  num_pe
      type(field_IO_params), intent(in) ::  fld_IO_param
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO(num_pe)
!
      integer :: id_rank, ip, iloop
!
!
      do iloop = 0, (num_pe-1) / nprocs
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
