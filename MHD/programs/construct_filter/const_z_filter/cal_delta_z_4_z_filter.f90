!cal_delta_z_4_z_filter.f90
!      module cal_delta_z_4_z_filter
!
!      Written by H. Matsui
!
!      subroutine cal_delta_z(nod_comm, node, ele, edge, jac_1d)
!
      module cal_delta_z_4_z_filter
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_edge_data
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine cal_delta_z(nod_comm, node, ele, edge, jac_1d)
!
      use m_crs_matrix
      use m_int_edge_vart_width
      use m_int_edge_data
      use m_commute_filter_z
!
      use t_jacobian_1d
!
      use calcs_by_LUsolver
      use solve_by_mass_z
      use int_edge_mass_mat_z_filter
      use set_matrices_4_z_filter
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
!
      type(jacobians_1d), intent(in) :: jac_1d
      integer(kind = kint) :: num_int
!
!
      num_int = i_int_z_filter
!
      call int_edge_mass_matrix(node%numnod, ele%numele, edge,          &
     &                          num_int, jac_1d)
!      iflag_mass = 0
!
      if ( iflag_mass .eq. 1) then
       call allocate_consist_mass_crs(node%numnod)
       call set_consist_mass_mat(node%numnod)
!
       call allocate_delta_z(node%numnod, ele%numele)
       call int_edge_vart_width(ele%numele, edge, num_int, jac_1d)
!
       call set_rhs_vart_width(node%numnod)
       call set_consist_mass_mat(node%numnod)
!
       write(*,*) mat1_crs%METHOD_crs
       if ( mat1_crs%METHOD_crs .eq. 'LU' ) then
!
         call solve_delta_z_LU(node%numnod)
!
       else
!
         write(*,*) 'solve_crs_by_mass_z'
         call solve_crs_by_mass_z(nod_comm, node)
!$omp workshare
         delta_z(1:node%numnod) = sol_mk_crs(1:node%numnod)
!$omp end workshare
       end if
!
       write(*,*) 'int_edge_diff_vart_w'
       call int_edge_diff_vart_w(ele, edge, num_int, jac_1d)
       write(*,*) 'set_rhs_vart_width'
       call set_rhs_vart_width(node%numnod)

       if ( mat1_crs%METHOD_crs .eq. 'LU' ) then
         call solve_delta_dz_LU(node%numnod)
       else
         write(*,*) 'solve_crs_by_mass_z2'
         call  solve_crs_by_mass_z2(nod_comm, node)
!$omp workshare
         delta_dz(1:node%numnod) = sol_mk_crs(1:node%numnod)
!$omp end workshare
        end if
!
       call int_edge_d2_vart_w(node, ele, edge, num_int, jac_1d)
!       call int_edge_d2_vart_w2(ele, edge, num_int, jac_1d)
       call set_rhs_vart_width(node%numnod)

       if ( mat1_crs%METHOD_crs .eq. 'LU' ) then
          call solve_delta_d2z_LU(node%numnod)
       else
         write(*,*) 'solve_crs_by_mass_z2'
         call  solve_crs_by_mass_z2(nod_comm, node)
!$omp workshare
         d2_dz(1:node%numnod) = sol_mk_crs(1:node%numnod)
!$omp end workshare
       end if
!
       call cal_sol_d2_vart_width(node%numnod)
!
      else
        call allocate_delta_z(node%numnod, ele%numele)
        call int_edge_vart_width(ele%numele, edge, num_int, jac_1d)
        call cal_sol_vart_width(node%numnod)
!
        call int_edge_diff_vart_w(ele, edge, num_int, jac_1d)
        call cal_sol_diff_vart_width(node%numnod)
!
        call int_edge_d2_vart_w(node, ele, edge, num_int, jac_1d)
        call cal_sol_d2_vart_width(node%numnod)
!
      end if
!
!
      end subroutine cal_delta_z
!
!   --------------------------------------------------------------------
!
      end module cal_delta_z_4_z_filter
