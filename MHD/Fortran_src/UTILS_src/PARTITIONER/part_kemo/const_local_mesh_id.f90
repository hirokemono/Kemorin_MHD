!
!     module const_local_mesh_id
!
!     written by H. Matsui on Sep., 2007
!
!!      subroutine s_const_local_meshes(ip, org_node, org_ele,          &
!!     &          internals_part, nod_d_grp, ele_d_grp, newmesh)
!!        type(node_data), intent(in) :: org_node
!!        type(element_data), intent(in) :: org_ele
!!        type(internals_4_part), intent(in) :: internals_part
!!        type(domain_group_4_partition), intent(inout)  :: nod_d_grp
!!        type(domain_group_4_partition), intent(inout)  :: ele_d_grp
!!        type(mesh_geometry), intent(inout) :: newmesh
!!
!!      subroutine const_local_node_position                            &
!!     &         (ip, org_node, itl_nod_part, new_node, nod_d_grp)
!!       integer(kind = kint), intent(in) :: ip
!!       type(node_data), intent(in) :: org_node
!!       type(internal_4_partitioner), intent(in) :: itl_nod_part
!!       type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!       type(node_data), intent(inout) :: new_node
!!      subroutine const_local_element                                  &
!!     &         (ip, org_ele, itl_ele_part, new_ele, ele_d_grp)
!!        integer(kind = kint), intent(in) :: ip
!!        type(element_data), intent(in) :: org_ele
!!        type(internal_4_partitioner), intent(in) :: itl_ele_part
!!        type(domain_group_4_partition), intent(inout) :: ele_d_grp
!!        type(element_data), intent(inout) :: new_ele
!!
!!      subroutine set_local_connectivity_4_ele                         &
!!     &         (ele_org, nod_d_grp, ele_new)
!!        type(element_data), intent(in) :: ele_org
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!        type(element_data), intent(inout) :: ele_new
!
      module const_local_mesh_id
!
      use m_precision
!
      use t_internal_4_partitioner
      use t_domain_group_4_partition
      use set_local_by_subdomain_tbl
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_const_local_meshes(ip, org_node, org_ele,            &
     &          internals_part, nod_d_grp, ele_d_grp, newmesh)
!
      use t_mesh_data
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      type(internals_4_part), intent(in) :: internals_part
!
      type(domain_group_4_partition), intent(inout)  :: nod_d_grp
      type(domain_group_4_partition), intent(inout)  :: ele_d_grp
      type(mesh_geometry), intent(inout) :: newmesh
!
!
      call const_local_node_position(ip, org_node,                      &
     &    internals_part%itl_nod_part, newmesh%node, nod_d_grp)
      call const_local_element(ip, org_ele,                             &
     &    internals_part%itl_ele_part, newmesh%ele, ele_d_grp)
!
      end subroutine s_const_local_meshes
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine const_local_node_position                              &
     &         (ip, org_node, itl_nod_part, new_node, nod_d_grp)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(node_data), intent(in) :: org_node
      type(internal_4_partitioner), intent(in) :: itl_nod_part
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
      type(node_data), intent(inout) :: new_node
!
!
      new_node%numnod = itl_nod_part%num_4_subdomain(ip)
      new_node%internal_node = itl_nod_part%num_inter_sub(ip)
      call alloc_node_geometry_w_sph(new_node)
      call set_local_node                                               &
     &    (ip, org_node, itl_nod_part, new_node, nod_d_grp)
!
      end subroutine const_local_node_position
!
!   --------------------------------------------------------------------
!
      subroutine const_local_element                                    &
     &         (ip, org_ele, itl_ele_part, new_ele, ele_d_grp)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(element_data), intent(in) :: org_ele
      type(internal_4_partitioner), intent(in) :: itl_ele_part
!
      type(domain_group_4_partition), intent(inout) :: ele_d_grp
      type(element_data), intent(inout) :: new_ele
!
!
      new_ele%numele =     itl_ele_part%num_4_subdomain(ip)
      new_ele%nnod_4_ele = org_ele%nnod_4_ele
      call alloc_ele_connect(new_ele)
!
      call set_local_element                                            &
     &   (ip, org_ele, itl_ele_part, new_ele, ele_d_grp)
!
      end subroutine const_local_element
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_connectivity_4_ele                           &
     &         (ele_org, nod_d_grp, ele_new)
!
      use t_geometry_data
      use t_domain_group_4_partition
!
      type(element_data), intent(in) :: ele_org
      type(domain_group_4_partition), intent(in) :: nod_d_grp
!
      type(element_data), intent(inout) :: ele_new
!
      integer(kind = kint) :: inum, inod_g, k1
      integer(kind = kint_gl) :: iele
!
!
      do inum = 1, ele_new%numele
        iele = ele_new%iele_global(inum)
        do k1 = 1, ele_new%nodelm(inum)
          inod_g = ele_org%ie(iele,k1)
          ele_new%ie(inum,k1) = nod_d_grp%id_local_part(inod_g)
        end do
      end do
!
      end subroutine set_local_connectivity_4_ele
!
!   --------------------------------------------------------------------
!
      end module const_local_mesh_id
