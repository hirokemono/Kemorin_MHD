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
      type(mesh_data), save :: original_fem
      type(mesh_data), save :: cutted_fem
      integer(kind = kint) :: nnod_4_surf, nnod_4_edge
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------

      subroutine  initialize_cutshell
!
      use m_control_data_4_cutshell
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
      call input_mesh(my_rank, original_fem%mesh, original_fem%group,   &
     &    nnod_4_surf, nnod_4_edge)
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_element_info'
      call set_spherical_position(original_fem%mesh%node)
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
      call s_const_reduced_geometry                                     &
     &   (original_fem%mesh, original_fem%group,                        &
     &    cutted_fem%mesh, cutted_fem%group)
!
      cutted_fem%mesh%nod_comm%num_neib = 0
      call allocate_type_comm_tbl_num(cutted_fem%mesh%nod_comm)
      call allocate_type_comm_tbl_item(cutted_fem%mesh%nod_comm)
!
      mesh_file_head = modified_mesh_head
      call output_mesh(my_rank, cutted_fem%mesh, cutted_fem%group)
      call dealloc_mesh_infos(cutted_fem%mesh, cutted_fem%group)
!
      end subroutine analyze_cutshell
!
!   --------------------------------------------------------------------
!
      end module analyzer_cutshell
