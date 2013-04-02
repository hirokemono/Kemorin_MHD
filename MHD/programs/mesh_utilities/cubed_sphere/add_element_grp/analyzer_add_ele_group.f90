!analyzer_add_ele_group.f90
!      module analyzer_add_ele_group
!
      module  analyzer_add_ele_group
!
!     Written by H. Matsui on Mar., 2008
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      integer(kind = kint), parameter, private :: my_rank = izero
      integer(kind = kint), parameter, private :: ifile_type = izero
!
!      subroutine initialize_add_egrp
!      subroutine analyze_add_egrp
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine  initialize_add_egrp
!
      use m_control_data_add_ele_grp
      use m_add_ele_grp_parameter
      use m_geometry_data
      use m_read_mesh_data
      use load_mesh_data
      use set_control_add_2d_egrp
      use const_mesh_info
      use cal_mesh_position
!
!
      call read_control_add_elegrp
      call s_set_control_add_2d_egrp
!
!  read global mesh
!
      mesh_file_head = original_mesh_head
      iflag_mesh_file_fmt = ifile_type
      call input_mesh(my_rank)
!
      call deallocate_element_geometry
!
       if (iflag_debug.eq.1) write(*,*) 'set_local_element_info'
      call set_local_element_info
      call set_spherical_position
!
       if (iflag_debug.eq.1) write(*,*) 'set_center_of_element'
      call allocate_element_geometry
      call set_center_of_element
!
      end subroutine  initialize_add_egrp
!
!   --------------------------------------------------------------------
!
      subroutine analyze_add_egrp
!
      use m_nod_comm_table
      use m_2nd_geometry_data
      use m_2nd_nod_comm_table
      use m_2nd_group_data
      use m_add_ele_grp_parameter
      use m_read_mesh_data
      use copy_comm_tables_to_2nd
      use link_geometry_to_1st_mesh
      use link_group_to_1st_mesh
      use set_new_2d_element_group
      use load_2nd_mesh_data
!
!
       if (iflag_debug.eq.1) write(*,*) 'copy_nod_comm_table_2_2nd'
      call copy_nod_comm_table_2_2nd
      call deallocate_nod_import_item
      call deallocate_nod_export_item
      call deallocate_neib_id
!
      call link_node_data
      call link_element_data
!
      call link_node_group
      call link_surface_group
!
      call alloc_r_ele_cubed_sph
      call set_rele_cubed_sph
       if (iflag_debug.eq.1) write(*,*) 's_set_new_2d_element_group'
      call s_set_new_2d_element_group
!
       if (iflag_debug.eq.1) write(*,*) 'output_2nd_mesh'
      iflag_mesh_file_fmt = ifile_type
      mesh_file_head = modified_mesh_head
      call output_2nd_mesh(my_rank)
!
      write(*,*) 'deallocate_2nd_element_group'
      call deallocate_2nd_element_group
      call dealloc_r_ele_cubed_sph
!
      end subroutine analyze_add_egrp
!
!   --------------------------------------------------------------------
!
      end module analyzer_add_ele_group
