!set_parallel_mesh_in_1pe.f90
!      module set_parallel_mesh_in_1pe
!
!      Written by H. Matsui on May, 2010
!
!!      subroutine s_set_parallel_mesh_in_1pe                           &
!!     &         (mesh_file, num_pe, para_mesh)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_data), intent(inout) :: para_mesh(num_pe)
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
     &         (mesh_file, num_pe, para_mesh)
!
      use t_mesh_data
      use t_geometry_data
      use t_file_IO_parameter

      use mesh_IO_select
      use load_mesh_data
      use const_mesh_information
!
      integer, intent(in) :: num_pe
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_data), intent(inout) :: para_mesh(num_pe)
!
      integer :: ip, id_rank
      integer(kind = kint) :: ierr
!
!
      do ip = 1, num_pe
        id_rank = ip - 1
        call input_mesh(mesh_file, id_rank,                             &
     &      para_mesh(ip)%mesh, para_mesh(ip)%group, ierr)
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
