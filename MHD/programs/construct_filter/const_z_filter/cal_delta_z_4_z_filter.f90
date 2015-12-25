!cal_delta_z_4_z_filter.f90
!      module cal_delta_z_4_z_filter
!
!      Written by H. Matsui
!
!      subroutine cal_delta_z(jac_1d)
!
      module cal_delta_z_4_z_filter
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine cal_delta_z(jac_1d)
!
      use m_geometry_data
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
      type(jacobians_1d), intent(in) :: jac_1d
      integer(kind = kint) :: num_int
!
!
      num_int = i_int_z_filter
!
      call int_edge_mass_matrix(num_int, jac_1d)
!      iflag_mass = 0
!
      if ( iflag_mass .eq. 1) then
       call allocate_consist_mass_crs(node1%numnod)
       call set_consist_mass_mat(node1%numnod)
!
       call allocate_delta_z
       call int_edge_vart_width(num_int, jac_1d)
!
       call set_rhs_vart_width
       call set_consist_mass_mat(node1%numnod)
!
       write(*,*) mat1_crs%METHOD_crs
       if ( mat1_crs%METHOD_crs .eq. 'LU' ) then
!
         call solve_delta_z_LU(node1%numnod)
!
       else
!
         write(*,*) 'solve_crs_by_mass_z'
         call solve_crs_by_mass_z
!$omp workshare
         delta_z(1:node1%numnod) = sol_mk_crs(1:node1%numnod)
!$omp end workshare
       end if
!
       write(*,*) 'int_edge_diff_vart_w'
       call int_edge_diff_vart_w(num_int, jac_1d)
       write(*,*) 'set_rhs_vart_width'
       call set_rhs_vart_width

       if ( mat1_crs%METHOD_crs .eq. 'LU' ) then
         call solve_delta_dz_LU(node1%numnod)
       else
         write(*,*) 'solve_crs_by_mass_z2'
         call  solve_crs_by_mass_z2
!$omp workshare
         delta_dz(1:node1%numnod) = sol_mk_crs(1:node1%numnod)
!$omp end workshare
        end if
!
       call int_edge_d2_vart_w(num_int, jac_1d)
!       call int_edge_d2_vart_w2(num_int, jac_1d)
       call set_rhs_vart_width

       if ( mat1_crs%METHOD_crs .eq. 'LU' ) then
          call solve_delta_d2z_LU(node1%numnod)
       else
         write(*,*) 'solve_crs_by_mass_z2'
         call  solve_crs_by_mass_z2
!$omp workshare
         d2_dz(1:node1%numnod) = sol_mk_crs(1:node1%numnod)
!$omp end workshare
       end if
!
       call cal_sol_d2_vart_width
!
      else
        call allocate_delta_z
        call int_edge_vart_width(num_int, jac_1d)
        call cal_sol_vart_width
!
        call int_edge_diff_vart_w(num_int, jac_1d)
        call cal_sol_diff_vart_width
!
        call int_edge_d2_vart_w(num_int, jac_1d)
        call cal_sol_d2_vart_width
!
      end if
!
!
      end subroutine cal_delta_z
!
!   --------------------------------------------------------------------
!
      end module cal_delta_z_4_z_filter
