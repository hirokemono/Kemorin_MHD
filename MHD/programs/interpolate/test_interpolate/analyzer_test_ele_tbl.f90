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
      use t_step_parameter
      use t_mesh_data
      use t_interpolate_table
      use t_IO_step_parameter
      use t_ctl_data_gen_table
!
      implicit none
!
      type(ctl_data_gen_table), save :: gtbl_ctl1
!
      type(mesh_data), save :: org_femmesh
      type(element_geometry), save :: org_ele_mesh
!
      type(mesh_data), save :: new_femmesh
      type(element_geometry), save :: new_ele_mesh
!
      type(interpolate_table), save :: itp_ele_t
!
      type(time_step_param), save :: t_ITP
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
      use input_control_interpolate
      use const_mesh_information
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
      call s_input_control_interpolate(gtbl_ctl1,                       &
     &    org_femmesh, org_ele_mesh, new_femmesh, new_ele_mesh,         &
     &    itp_ele_t, t_ITP, ierr)
!
!     --------------------- 
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
      use mesh_interpolation
!
!
      if (iflag_debug.eq.1) write(*,*) 'interpolation_4_mesh_test'
      call interpolation_4_mesh_test                                    &
     &   (org_femmesh%mesh, new_femmesh%mesh, itp_ele_t)
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_ele_tbl
