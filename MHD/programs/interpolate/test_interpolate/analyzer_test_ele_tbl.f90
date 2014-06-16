!analyzer_test_ele_tbl.f90
!      module analyzer_test_ele_tbl
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_analyzer
!      subroutine analyze
!
!..................................................
!
      module analyzer_test_ele_tbl
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_mesh_data
!
      implicit none
!
      type(mesh_data), save :: new_femmesh
      type(surface_geometry), save :: new_surf_mesh
      type(edge_geometry), save ::  new_edge_mesh
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use m_ctl_params_4_gen_table
!
      use input_control_itp_mesh
      use const_mesh_info
      use set_size_4_smp_types
      use element_posi_2_nodal_array
!
      integer(kind = kint) :: ierr
!
!     ---------------------
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_itp_mesh'
      call s_input_control_itp_mesh(new_femmesh,                        &
     &    new_surf_mesh, new_edge_mesh, ierr)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_element_info'
      call set_local_element_info
!
!     --------------------- 
!
      if (my_rank .lt. ndomain_org) then
        if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
        call set_nod_and_ele_infos
      end if
!
      call s_2nd_ele_posi_2_nodal_array(new_femmesh%mesh)
!
!     --------------------- 
!
      if (my_rank .lt. ndomain_dest) then
        call count_size_4_smp_mesh_type                                 &
     &     (new_femmesh%mesh%node, new_femmesh%mesh%ele)
        if(i_debug.eq.iflag_full_msg) then
          call check_smp_size_type(my_rank, new_femmesh%mesh)
        end if
      end if
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_interpolated_geometry
      use mesh_interpolation
!
!
       if (iflag_debug.eq.1) write(*,*) 'allocate_interpolate_geometry'
      call allocate_interpolate_geometry                                &
     &   (new_femmesh%mesh%node%numnod)
!
       if (iflag_debug.eq.1) write(*,*) 'interpolation_4_mesh_test'
      call interpolation_4_mesh_test(new_femmesh%mesh)
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_ele_tbl
