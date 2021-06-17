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
      use t_IO_step_parameter
      use t_structure_4_interolation
      use t_mesh_SR
      use m_solver_SR
!
      implicit none
!
      type(structure_4_interolation), save:: itp_etst
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
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
      call s_input_control_interpolate(itp_etst, ierr)
!
!     --------------------- 
!
      call s_2nd_ele_posi_2_nodal_array(itp_etst%new_fem%mesh)
!
!     --------------------- 
!
      if (my_rank .lt. itp_etst%gen_itp_p%ndomain_dest) then
        call count_size_4_smp_mesh                                      &
     &     (itp_etst%new_fem%mesh%node, itp_etst%new_fem%mesh%ele)
        if(i_debug.eq.iflag_full_msg) then
          call check_mesh_smp_size(my_rank, itp_etst%new_fem%mesh)
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
      use set_size_4_smp_types
!
!
      if (iflag_debug.eq.1) write(*,*) 'interpolation_4_mesh_test'
      call interpolation_4_mesh_test(itp_etst%nprocs_2nd,               &
     &    itp_etst%org_fem%mesh, itp_etst%new_fem%mesh,                 &
     &    itp_etst%itp_tbl, itp_etst%v_1st_sol, itp_etst%v_2nd_sol,     &
     &    SR_sig1, SR_r1, SR_il1)
!
      if (my_rank .lt. itp_etst%gen_itp_p%ndomain_dest) then
        call finalize_size_4_smp_mesh                                   &
     &     (itp_etst%new_fem%mesh%node, itp_etst%new_fem%mesh%ele)
      end if
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_ele_tbl
