!analyzer_add_ele_group.f90
!      module analyzer_add_ele_group
!     Written by H. Matsui on Mar., 2008
!     Modified by H. Matsui on June., 2013
!
!      subroutine initialize_add_egrp
!
!
      module  analyzer_add_ele_group
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_mesh_data
!
      implicit none
!
      type(mesh_geometry), save :: mesh_add
      type(mesh_groups), save :: group_add
!
      integer(kind = kint) :: nnod_4_surf, nnod_4_edge
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine  initialize_add_egrp
!
      use calypso_mpi
      use m_control_data_add_ele_grp
      use m_add_ele_grp_parameter
      use m_read_mesh_data
      use m_work_4_add_egrp_sph
      use mpi_load_mesh_data
      use const_mesh_information
!
      use set_control_add_2d_egrp
      use set_ele_grp2_by_2d
      use set_new_2d_element_group
!
!
!
      call read_control_add_elegrp
      call s_set_control_add_2d_egrp
!
!  read global mesh
!
      mesh_file_head = original_mesh_head
      call mpi_input_mesh                                               &
     &   (mesh_add, group_add, nnod_4_surf, nnod_4_edge)
      call const_nod_ele_infos(my_rank, mesh_add, group_add)
!
      call alloc_r_ele_cubed_sph(mesh_add%ele%numele)
      call set_rele_cubed_sph                                           &
     &   (mesh_add%node%numnod, mesh_add%ele%numele, mesh_add%ele%ie,   &
     &    mesh_add%node%rr, mesh_add%ele%r_ele)
!
      call allocate_work_4_add_egrp_sph(mesh_add%ele%numele)
      call count_new_2d_element_group(mesh_add%ele)
!
      if (iflag_debug.eq.1) write(*,*) 'set_new_2d_ele_group'
      call set_new_2d_ele_group(mesh_add%ele, group_add%ele_grp)
!
      call deallocate_work_4_add_egrp_sph
!
       if (iflag_debug.eq.1) write(*,*) 'output_mesh_1st'
      mesh_file_head = modified_mesh_head
      call mpi_output_mesh(mesh_add, group_add)
      call dealloc_mesh_infos(mesh_add, group_add)
!
      call dealloc_r_ele_cubed_sph
!
      end subroutine initialize_add_egrp
!
!   --------------------------------------------------------------------
!
      end module analyzer_add_ele_group
