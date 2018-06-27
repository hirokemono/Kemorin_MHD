!set_parallel_mesh_in_1pe.f90
!      module set_parallel_mesh_in_1pe
!
!      Written by H. Matsui on May, 2010
!
!!      subroutine s_set_parallel_mesh_in_1pe                           &
!!     &         (mesh_file, nprocs, para_mesh)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_data), intent(inout) :: para_mesh(nprocs)
!
      module set_parallel_mesh_in_1pe
!
      use m_precision
!
      implicit    none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_parallel_mesh_in_1pe                             &
     &         (mesh_file, nprocs, para_mesh)
!
      use t_mesh_data
      use t_geometry_data
      use t_file_IO_parameter

      use mesh_IO_select
      use load_mesh_data
      use const_mesh_information
!
      integer(kind = kint), intent(in) :: nprocs
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_data), intent(inout) :: para_mesh(nprocs)
!
      integer(kind = kint) :: ip, id_rank, ierr
      type(element_geometry) :: ele_mesh
!
!
      do ip = 1, nprocs
        id_rank = ip - 1
        call input_mesh                                                 &
     &     (mesh_file, id_rank, para_mesh(ip), ele_mesh, ierr)
        call set_nod_and_ele_infos                                      &
     &     (para_mesh(ip)%mesh%node, para_mesh(ip)%mesh%ele)
!
        if(ierr .gt. 0)  stop 'Mesh data is wrong!!'
      end do
!
      end subroutine s_set_parallel_mesh_in_1pe
!
! -----------------------------------------------------------------------
!
      end module set_parallel_mesh_in_1pe
