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
      use load_mesh_data
      use const_mesh_information
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data_p), intent(inout) :: para_mesh(nprocs)
!
      integer(kind = kint) :: ip, my_rank, ierr
      integer(kind = kint) :: nnod_4_surf, nnod_4_edge
!
!
      do ip = 1, nprocs
        my_rank = ip - 1
        call input_mesh_p                                               &
     &     (my_rank, para_mesh(ip)%mesh, para_mesh(ip)%group,           &
     &      nnod_4_surf, nnod_4_edge, ierr)
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
      subroutine parallel_mesh_in_1pe_p(nprocs, para_mesh)
!
      use t_mesh_data
      use t_geometry_data
      use mesh_IO_select
      use load_mesh_data
      use const_mesh_information
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data_p), intent(inout) :: para_mesh(nprocs)
!
      integer(kind = kint) :: ip, my_rank, ierr
      integer(kind = kint) :: nnod_4_surf, nnod_4_edge
!
!
      do ip = 1, nprocs
        my_rank = ip - 1
        call input_mesh_p                                               &
     &     (my_rank, para_mesh(ip)%mesh, para_mesh(ip)%group,           &
     &      nnod_4_surf, nnod_4_edge, ierr)
        call set_nod_and_ele_infos                                      &
     &     (para_mesh(ip)%mesh%node, para_mesh(ip)%mesh%ele)
!
        if(ierr .gt. 0)  stop 'Mesh data is wrong!!'
      end do
!
      end subroutine parallel_mesh_in_1pe_p
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_parallel_mesh_in_1pe(nprocs, para_mesh)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data_p), intent(inout) :: para_mesh(nprocs)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nprocs
        call deallocate_ele_geometry_type(para_mesh(ip)%mesh%ele)
        call deallocate_ele_param_smp_type(para_mesh(ip)%mesh%ele)
        call deallocate_node_param_smp_type(para_mesh(ip)%mesh%node)
!
        call deallocate_grp_type(para_mesh(ip)%group%nod_grp)
        call deallocate_grp_type(para_mesh(ip)%group%ele_grp)
        call deallocate_sf_grp_type(para_mesh(ip)%group%surf_grp)
        call finalize_mesh_group_type(para_mesh(ip)%group)
!
        call dealloc_mesh_type(para_mesh(ip)%mesh)
      end do
!
      end subroutine dealloc_parallel_mesh_in_1pe
!
! -----------------------------------------------------------------------
!
      end module set_parallel_mesh_in_1pe
