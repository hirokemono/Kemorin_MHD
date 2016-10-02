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
      type(mesh_data_p), allocatable :: origin_mesh(:)
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
      integer(kind = kint) :: jp
!
!
      allocate( origin_mesh(nprocs_2nd) )
      do jp = 1, nprocs_2nd
        call init_mesh_group_type(origin_mesh(jp)%group)
      end do
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
      type(mesh_geometry_p), intent(inout) :: newmesh
      type(mesh_groups_p), intent(inout) :: newgroup
      integer(kind = kint) :: jp
!
!
      jp = my_rank + 1
      call link_comm_tbl_types(origin_mesh(jp)%mesh%nod_comm, newmesh%nod_comm)
      call link_new_nod_geometry_type(origin_mesh(jp)%mesh%node, newmesh%node)
      newgroup%nod_grp =>  origin_mesh(jp)%group%nod_grp
      newgroup%ele_grp =>  origin_mesh(jp)%group%ele_grp
      newgroup%surf_grp => origin_mesh(jp)%group%surf_grp
!
      newmesh%ele => origin_mesh(jp)%mesh%ele
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
      type(mesh_geometry_p), intent(inout) :: newmesh
      type(mesh_groups_p), intent(inout) :: newgroup
!
!
      call deallocate_hex_2_tetra
!
      nullify(newgroup%surf_grp)
      nullify(newgroup%ele_grp)
      nullify(newgroup%nod_grp)

      nullify(newmesh%ele)
      call unlink_node_geometry_type(newmesh%node)

!
      call unlink_dest_comm_tbl_type(newmesh%nod_comm)
!
      end subroutine unlink_2nd_geometry_4_table
!
! ----------------------------------------------------------------------
!
      end module set_2nd_geometry_4_table
