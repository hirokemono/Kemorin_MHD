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
      type(mesh_data), save :: fem_add
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
      call mpi_input_mesh(original_mesh_file, nprocs,                   &
     &    fem_add%mesh, fem_add%group, nnod_4_surf, nnod_4_edge)
      call const_nod_ele_infos                                          &
     &   (my_rank, fem_add%mesh%node, fem_add%mesh%ele,                 &
     &    fem_add%group%nod_grp, fem_add%group%ele_grp,                 &
     &    fem_add%group%surf_grp)
!
      call alloc_r_ele_cubed_sph(fem_add%mesh%ele%numele)
      call set_rele_cubed_sph                                           &
     &   (fem_add%mesh%node%numnod, fem_add%mesh%ele%numele,            &
     &    fem_add%mesh%ele%ie, fem_add%mesh%node%rr,                    &
     &    fem_add%mesh%ele%r_ele)
!
      call allocate_work_4_add_egrp_sph(fem_add%mesh%ele%numele)
      call count_new_2d_element_group(fem_add%mesh%ele)
!
      if (iflag_debug.eq.1) write(*,*) 'set_new_2d_ele_group'
      call set_new_2d_ele_group                                         &
     &   (fem_add%mesh%ele, fem_add%group%ele_grp)
!
      call deallocate_work_4_add_egrp_sph
!
       if (iflag_debug.eq.1) write(*,*) 'mpi_output_mesh'
      call mpi_output_mesh                                              &
     &   (modified_mesh_file, fem_add%mesh, fem_add%group)
      call dealloc_mesh_infos(fem_add%mesh, fem_add%group)
!
      call dealloc_r_ele_cubed_sph
!
      end subroutine initialize_add_egrp
!
!   --------------------------------------------------------------------
!
      end module analyzer_add_ele_group
