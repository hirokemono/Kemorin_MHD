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
      use t_control_data_add_ele_grp
      use t_add_ele_grp_parameter
!
      implicit none
!
      type(mesh_data), save :: fem_add
      type(control_data_add_ele_grp), save :: addgrp_c1
      type(add_ele_grp_param), save :: add_egrp_param1
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
      use m_work_4_add_egrp_sph
      use input_control_add_elegrp
      use mpi_load_mesh_data
      use const_mesh_information
!
      use set_ele_grp2_by_2d
      use set_new_2d_element_group
!
!
!  read control
      call s_input_control_add_elegrp(add_egrp_param1)
!
!  read global mesh
!
      call mpi_input_mesh                                               &
     &   (add_egrp_param1%original_mesh_file, nprocs, fem_add)
      call const_nod_ele_infos(my_rank, fem_add%mesh, fem_add%group)
!
      call alloc_r_ele_cubed_sph(fem_add%mesh%ele%numele)
      call set_rele_cubed_sph                                           &
     &   (fem_add%mesh%node%numnod, fem_add%mesh%ele%numele,            &
     &    fem_add%mesh%ele%ie, fem_add%mesh%node%rr,                    &
     &    fem_add%mesh%ele%r_ele)
!
      call allocate_work_4_add_egrp_sph(fem_add%mesh%ele%numele)
      call count_new_2d_element_group(add_egrp_param1,                  &
     &                                fem_add%mesh%ele)
!
      if (iflag_debug.eq.1) write(*,*) 'set_new_2d_ele_group'
      call set_new_2d_ele_group                                         &
     &   (add_egrp_param1, fem_add%mesh%ele, fem_add%group%ele_grp)
!
      call deallocate_work_4_add_egrp_sph
!
       if (iflag_debug.eq.1) write(*,*) 'mpi_output_mesh'
      call mpi_output_mesh(add_egrp_param1%modified_mesh_file,          &
     &                     fem_add%mesh, fem_add%group)
      call dealloc_mesh_data(fem_add%mesh, fem_add%group)
!
      call dealloc_r_ele_cubed_sph
!
      end subroutine initialize_add_egrp
!
!   --------------------------------------------------------------------
!
      end module analyzer_add_ele_group
