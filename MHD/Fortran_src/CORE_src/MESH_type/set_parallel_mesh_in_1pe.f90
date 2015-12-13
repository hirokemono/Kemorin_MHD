!set_parallel_mesh_in_1pe.f90
!      module set_parallel_mesh_in_1pe
!
!      Written by H. Matsui on May, 2010
!
!      subroutine s_set_parallel_mesh_in_1pe(nprocs, para_mesh)
!      subroutine dealloc_parallel_mesh_in_1pe(nprocs, para_mesh)
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
      subroutine s_set_parallel_mesh_in_1pe(nprocs, para_mesh)
!
      use t_mesh_data
      use t_geometry_data
      use mesh_IO_select
      use load_mesh_type_data
      use const_mesh_types_info
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data), intent(inout) :: para_mesh(nprocs)
!
      integer(kind = kint) :: ip, my_rank
      integer(kind = kint) :: nnod_4_surf, nnod_4_edge
!
!
      do ip = 1, nprocs
        my_rank = ip - 1
        call input_mesh_data_type(my_rank, para_mesh(ip),               &
     &      nnod_4_surf, nnod_4_edge)
        call set_nod_and_ele_infos                                      &
     &     (para_mesh(ip)%mesh%node, para_mesh(ip)%mesh%ele)
      end do
!
      end subroutine s_set_parallel_mesh_in_1pe
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_parallel_mesh_in_1pe(nprocs, para_mesh)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data), intent(inout) :: para_mesh(nprocs)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nprocs
        call dealloc_base_mesh_type_info(para_mesh(ip))
      end do
!
      end subroutine dealloc_parallel_mesh_in_1pe
!
! -----------------------------------------------------------------------
!
      end module set_parallel_mesh_in_1pe
