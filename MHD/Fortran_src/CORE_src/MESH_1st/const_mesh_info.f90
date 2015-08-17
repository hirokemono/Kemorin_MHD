!const_mesh_info.f90
!      module const_mesh_info
!
!      Written by H. Matsui on July, 2006
!      Modified by H. Matsui on June, 2007
!
!
!      subroutine const_mesh_informations(my_rank)
!      subroutine const_nod_ele_infos(my_rank)
!      subroutine deallocate_mesh_infomations
!      subroutine deallocate_nod_ele_infos
!
!      subroutine set_local_element_info
!      subroutine set_nod_and_ele_infos
!      subroutine set_edge_and_surf_data(my_rank)
!
      module const_mesh_info
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      private :: count_num_groups_4_smp
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_mesh_informations(my_rank)
!
      use m_geometry_data
      use m_group_data
      use m_element_group_connect
      use m_surface_group_connect
      use const_surface_data
!      use check_surface_groups
!
      integer(kind = kint), intent(in) :: my_rank
!
!
       if (iflag_debug.gt.0) write(*,*) 'set_local_element_info'
      call set_local_element_info
!
      if (iflag_debug.gt.0) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos
      if (iflag_debug.gt.0) then
        call check_nod_size_smp_type(node1, my_rank)
      end if
!
       if (iflag_debug.gt.0) write(*,*) 'set_edge_and_surf_data'
      call set_edge_and_surf_data(my_rank)
!
       if (iflag_debug.gt.0) write(*,*) 'set_edge_and_surf_geometry'
      call set_edge_and_surf_geometry
!
!
       if (iflag_debug.gt.0) write(*,*) 'count_num_groups_4_smp'
      call count_num_groups_4_smp
!       if (iflag_debug.gt.0) then
!         call check_grp_4_sheard_para(my_rank, nod_grp1)
!         call check_grp_4_sheard_para(my_rank, ele_grp1)
!         call check_surf_grp_4_sheard_para(my_rank, sf_grp1)
!       end if
!
      if (iflag_debug.gt.0) write(*,*) 'set_surface_node_grp'
      call set_surface_node_grp(sf_grp1)
!       call check_surface_node_id(my_rank, sf_grp_nod1)
!
      if (iflag_debug.gt.0) write(*,*) 'const_element_list_4_surface'
      call const_element_list_4_surface
!
!       if (iflag_debug.gt.0) call check_surf_nod_4_sheard_para         &
!     &                     (my_rank, sf_grp1%num_grp, sf_grp_nod1)
!
!
       if (iflag_debug.eq.1) write(*,*) 'const_ele_group_connect'
      call const_ele_group_connect
       if (iflag_debug.eq.1) write(*,*) 'const_surf_group_connect'
      call const_surf_group_connect
!
      end subroutine const_mesh_informations
!
! ----------------------------------------------------------------------
!
      subroutine const_nod_ele_infos
!
      use m_surface_group_connect
!
!
       if (iflag_debug.gt.0) write(*,*) 'set_local_element_info'
      call set_local_element_info
!
       if (iflag_debug.gt.0) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos
!
       if (iflag_debug.gt.0) write(*,*) 'count_num_groups_4_smp'
      call count_num_groups_4_smp
!
      end subroutine const_nod_ele_infos
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_mesh_infomations
!
      use m_nod_comm_table
!
      use m_geometry_data
      use m_group_data
!
      use m_surface_geometry_data
      use m_edge_geometry_data
      use m_element_group_connect
      use m_surface_group_connect
!
!
      call deallocate_surf_group_connect
      call deallocate_ele_group_connect
!
      call deallocate_sf_grp_type_smp(sf_grp1)
      call deallocate_grp_type_smp(ele_grp1)
      call deallocate_grp_type_smp(nod_grp1)
      call deallocate_sf_grp_type(sf_grp1)
      call deallocate_grp_type(ele_grp1)
      call deallocate_grp_type(nod_grp1)
!
      call deallocate_surface_geometry
      call deallocate_edge_geometry
!
      call deallocate_edge_param_smp_type(edge1)
      call deallocate_inod_in_edge_type(edge1)
      call deallocate_surf_param_smp_type(surf1)
      call deallocate_inod_in_surf_type(surf1)
      call deallocate_ele_param_smp_type(ele1)
      call deallocate_node_param_smp_type(node1)
!
      call deallocate_edge_4_ele
      call deallocate_edge_connect
      call deallocate_surface_connect
      call deallocate_element_geometry
!
      call deallocate_ele_connect_type(ele1)
      call deallocate_node_geometry
      call deallocate_type_comm_tbl(nod_comm)
!
      end subroutine deallocate_mesh_infomations
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_nod_ele_infos
!
      use m_nod_comm_table
!
      use m_geometry_data
      use m_group_data
!
!
      call deallocate_sf_grp_type_smp(sf_grp1)
      call deallocate_grp_type_smp(ele_grp1)
      call deallocate_grp_type_smp(nod_grp1)
      call deallocate_sf_grp_type(sf_grp1)
      call deallocate_grp_type(ele_grp1)
      call deallocate_grp_type(nod_grp1)
!
      call deallocate_inod_in_edge_type(edge1)
      call deallocate_inod_in_surf_type(surf1)
      call deallocate_ele_param_smp_type(ele1)
      call deallocate_node_param_smp_type(node1)
!
      call deallocate_element_geometry
!
      call deallocate_ele_connect_type(ele1)
      call deallocate_node_geometry
      call deallocate_type_comm_tbl(nod_comm)
!
      end subroutine deallocate_nod_ele_infos
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_local_element_info
!
      use m_geometry_data
      use set_local_id_table_4_1ele
!
!
      if (iflag_debug.gt.0) write(*,*) 'allocate_inod_in_surf'
      call allocate_inod_in_surf(surf1)
      call set_inod_in_surf                                             &
     &   (surf1%nnod_4_surf, surf1%node_on_sf, surf1%node_on_sf_n)
!
      if (iflag_debug.gt.0) write(*,*) 'allocate_inod_in_edge'
      call allocate_inod_in_edge(edge1)
      call copy_inod_in_edge                                            &
     &   (edge1%nnod_4_edge, edge1%node_on_edge, edge1%node_on_edge_sf)
!
      end subroutine set_local_element_info
!
! ----------------------------------------------------------------------
!
      subroutine count_num_groups_4_smp
!
      use m_group_data
      use set_smp_4_group_types
!
      call count_grp_type_smp(nod_grp1)
      call count_grp_type_smp(ele_grp1)
      call count_surf_grp_type_smp(sf_grp1)
!
      end subroutine count_num_groups_4_smp
!
!-----------------------------------------------------------------------
!
      subroutine set_nod_and_ele_infos
!
      use set_size_4_smp
      use cal_mesh_position
!
!
      call count_size_4_sheard_para
!
       if (iflag_debug.gt.0) write(*,*) 'set_spherical_position'
      call set_spherical_position
!
       if (iflag_debug.gt.0) write(*,*) 'set_center_of_element'
      call set_center_of_element
!
       if (iflag_debug.gt.0) write(*,*) 'count_overlap_element'
      call count_overlap_element
!
      end subroutine set_nod_and_ele_infos
!
! ----------------------------------------------------------------------
!
      subroutine set_edge_and_surf_data(my_rank)
!
      use m_geometry_data
      use const_surface_data
      use const_edge_data
      use set_size_4_smp
!
      integer(kind = kint), intent(in) :: my_rank
      logical :: read_surface, read_edge
!
!
      read_surface = allocated(isurf_global)
      read_edge =    allocated(iedge_global)
!
      if(read_surface .eqv. .false.) then
        if (iflag_debug.gt.0) write(*,*) 'construct_surface_data'
        call construct_surface_data
!
        call count_surf_size_4_smp
        if (iflag_debug.gt.0) write(*,*) 'count_overlap_surface'
        call count_overlap_surface
      else
!
        call count_surf_size_4_smp
        if (iflag_debug.gt.0) write(*,*) 'count_overlap_surface'
        call count_overlap_surface
      end if
!
!
!
      if(read_edge .eqv. .false.) then
        if (iflag_debug.gt.0) write(*,*) 'construct_edge_data'
        call construct_edge_data(my_rank)
!
        if (iflag_debug.gt.0) write(*,*) 'count_overlap_edge'
        call count_edge_size_4_smp
        call count_overlap_edge
      else
!
        if (iflag_debug.gt.0) write(*,*) 'count_overlap_edge'
        call count_edge_size_4_smp
        call count_overlap_edge
!
      end if
!
      end subroutine set_edge_and_surf_data
!
! ----------------------------------------------------------------------
!
      subroutine set_edge_and_surf_geometry
!
      use cal_mesh_position
      use m_surface_geometry_data
      use m_edge_geometry_data
!
!
      call allocate_surface_geometry
      call set_center_of_surface
!
      call allocate_edge_geometry
      call set_center_of_edge
!
      end subroutine set_edge_and_surf_geometry
!
! ----------------------------------------------------------------------
!
      end module const_mesh_info
