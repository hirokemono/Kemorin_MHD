!analyzer_cutshell.f90
!     Written by H. Matsui on Oct., 2007
!
!      subroutine initialize_cutshell
!      subroutine analyze_cutshell
!
      module  analyzer_cutshell
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_mesh_data
      use t_phys_data
!
      implicit none
!
      integer(kind = kint), parameter, private :: my_rank = izero
!
      type(mesh_data), save :: cutted_fem
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------

      subroutine  initialize_cutshell
!
      use m_control_data_4_cutshell
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_read_mesh_data
      use load_mesh_data
      use set_control_cut_shell
      use cal_mesh_position
      use const_cutshell_mesh
!
!
      call read_control_data_4_cutshell
      call s_set_control_4_cutshell
!
!  read global mesh
!
      mesh_file_head = original_mesh_head
      call input_mesh                                                   &
     &   (my_rank, nod_comm, node1, ele1, nod_grp1, ele_grp1, sf_grp1,  &
     &    surf1%nnod_4_surf, edge1%nnod_4_edge)
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_element_info'
      call set_spherical_position(node1)
!
      end subroutine  initialize_cutshell
!
!   --------------------------------------------------------------------
!
      subroutine analyze_cutshell
!
      use m_read_mesh_data
      use const_cutshell_mesh
      use load_mesh_data
!
!
      call s_const_reduced_geometry(cutted_fem%mesh, cutted_fem%group)
!
      cutted_fem%mesh%nod_comm%num_neib = 0
      call allocate_type_comm_tbl_num(cutted_fem%mesh%nod_comm)
      call allocate_type_comm_tbl_item(cutted_fem%mesh%nod_comm)
!
      mesh_file_head = modified_mesh_head
      call output_mesh_type(my_rank, cutted_fem)
!
      end subroutine analyze_cutshell
!
!   --------------------------------------------------------------------
!
      end module analyzer_cutshell
