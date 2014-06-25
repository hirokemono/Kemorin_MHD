!
!     module set_2nd_geometry_4_table
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine set_2nd_geometry_type_itp_tbl(nprocs_2nd)
!      subroutine link_2nd_geometry_4_itp_tbl(my_rank,                  &
!     &          newmesh, newgroup)
!      subroutine unlink_2nd_geometry_4_table(newmesh, newgroup)
!
      module set_2nd_geometry_4_table
!
      use m_precision
!
      use m_machine_parameter
      use t_mesh_data
      use m_2nd_pallalel_vector
      use m_connect_hexa_2_tetra
!
      implicit none
!
      type(mesh_data), pointer :: origin_mesh(:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_org_mesh_type_itp_para(nprocs_2nd)
!
      integer(kind = kint), intent(in) :: nprocs_2nd
!
      allocate( origin_mesh(nprocs_2nd) )
!
      end subroutine alloc_org_mesh_type_itp_para
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_org_mesh_type_itp_para
!
      deallocate( origin_mesh )
!
      end subroutine dealloc_org_mesh_type_itp_para
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_2nd_geometry_type_itp_tbl(nprocs_2nd)
!
      use m_ctl_params_4_gen_table
      use m_read_mesh_data
      use set_parallel_mesh_in_1pe
!
      integer(kind = kint), intent(in) :: nprocs_2nd
!
!
      call alloc_org_mesh_type_itp_para(nprocs_2nd)
!
      mesh_file_head = org_mesh_head
      write(*,*) 'mesh_file_head: ', trim(mesh_file_head), nprocs_2nd
      call s_set_parallel_mesh_in_1pe(nprocs_2nd, origin_mesh)
!
      end subroutine set_2nd_geometry_type_itp_tbl
!
! ----------------------------------------------------------------------
!
      subroutine link_2nd_geometry_4_itp_tbl(my_rank,                   &
     &          newmesh, newgroup)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: newgroup
      integer(kind = kint) :: jp
!
!
      jp = my_rank + 1
      call link_mesh_data_type(origin_mesh(jp)%mesh, newmesh)
      call link_groups_type(origin_mesh(jp)%group, newgroup)
!
      call link_new_ele_geometry_type                                   &
     &   (origin_mesh(jp)%mesh%ele, newmesh%ele)
!
      if (newmesh%ele%nnod_4_ele .eq. num_t_linear) then
        call set_1_hexa_2_5_tetra
      else if (newmesh%ele%nnod_4_ele .eq. num_t_quad) then
        call set_1_hexa_2_21_tetra
      else if (newmesh%ele%nnod_4_ele .eq. num_t_lag) then
        call set_1_hexa_2_40_tetra
      end if
!
      end subroutine link_2nd_geometry_4_itp_tbl
!
! ----------------------------------------------------------------------
!
      subroutine unlink_2nd_geometry_4_table(newmesh, newgroup)
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) ::   newgroup
!
!
      call unlink_ele_geometry_type(newmesh%ele)
      call deallocate_hex_2_tetra
!
      call unlink_surf_group_type(newgroup%surf_grp)
      call unlink_group_type(newgroup%ele_grp)
      call unlink_group_type(newgroup%nod_grp)
      call unlink_node_geometry_type(newmesh%node)

      call unlink_ele_connect_type(newmesh%ele)
!
      call unlink_dest_comm_tbl_type(newmesh%nod_comm)
!
      end subroutine unlink_2nd_geometry_4_table
!
! ----------------------------------------------------------------------
!
      end module set_2nd_geometry_4_table
