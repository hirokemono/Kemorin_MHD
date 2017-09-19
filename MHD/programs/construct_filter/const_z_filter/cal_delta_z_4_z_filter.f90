!cal_delta_z_4_z_filter.f90
!      module cal_delta_z_4_z_filter
!
!      Written by H. Matsui
!
!!      subroutine cal_delta_z                                          &
!!     &         (CG_param, DJDS_param, nod_comm, node, ele, edge,      &
!!     &          spf_1d, g_FEM, jac_1d, tbl_crs, mat_crs)
!
      module cal_delta_z_4_z_filter
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_edge_data
      use t_iccg_parameter
      use t_crs_connect
      use t_crs_matrix
!
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use t_jacobian_1d
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine cal_delta_z                                            &
     &         (CG_param, DJDS_param, nod_comm, node, ele, edge,        &
     &          spf_1d, g_FEM, jac_1d, tbl_crs, mat_crs)
!
      use m_int_edge_vart_width
      use m_int_edge_data
      use m_commute_filter_z
!
      use calcs_by_LUsolver
      use solve_by_mass_z
      use int_edge_mass_mat_z_filter
      use set_matrices_4_z_filter
!
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_poarameter), intent(in) :: DJDS_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(edge_shape_function), intent(in) :: spf_1d
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
!
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
!
      integer(kind = kint) :: num_int
!
!
      num_int = i_int_z_filter
!
      call int_edge_mass_matrix(node%numnod, ele%numele, edge,          &
     &                          num_int, g_FEM, jac_1d)
!      iflag_mass = 0
!
      if ( iflag_mass .eq. 1) then
       call allocate_consist_mass_crs(node%numnod, tbl_crs)
       call set_consist_mass_mat(node%numnod)
!
       call allocate_delta_z(node%numnod, ele%numele)
       call int_edge_vart_width                                         &
     &    (ele%numele, edge, num_int, g_FEM, jac_1d)
!
       call set_rhs_vart_width(node%numnod)
       call set_consist_mass_mat(node%numnod)
!
       write(*,*) mat_crs%METHOD_crs
       if ( mat_crs%METHOD_crs .eq. 'LU' ) then
!
         call solve_delta_z_LU(node%numnod)
!
       else
!
         write(*,*) 'solve_crs_by_mass_z'
         call solve_crs_by_mass_z                                       &
     &      (CG_param, DJDS_param, nod_comm, node, tbl_crs, mat_crs)
!$omp workshare
         delta_z(1:node%numnod) = sol_mk_crs(1:node%numnod)
!$omp end workshare
       end if
!
       write(*,*) 'int_edge_diff_vart_w'
       call int_edge_diff_vart_w                                        &
     &    (ele, edge, num_int, spf_1d, g_FEM, jac_1d)
       write(*,*) 'set_rhs_vart_width'
       call set_rhs_vart_width(node%numnod)

       if ( mat_crs%METHOD_crs .eq. 'LU' ) then
         call solve_delta_dz_LU(node%numnod)
       else
         write(*,*) 'solve_crs_by_mass_z2'
         call  solve_crs_by_mass_z2                                     &
     &      (CG_param, DJDS_param, nod_comm, node, tbl_crs, mat_crs)
!$omp workshare
         delta_dz(1:node%numnod) = sol_mk_crs(1:node%numnod)
!$omp end workshare
        end if
!
       call int_edge_d2_vart_w                                          &
     &    (node, ele, edge, num_int, spf_1d, g_FEM, jac_1d)
!       call int_edge_d2_vart_w2                                        &
!     &     (ele, edge, num_int, spf_1d, g_FEM, jac_1d)
       call set_rhs_vart_width(node%numnod)

       if ( mat_crs%METHOD_crs .eq. 'LU' ) then
          call solve_delta_d2z_LU(node%numnod)
       else
         write(*,*) 'solve_crs_by_mass_z2'
         call  solve_crs_by_mass_z2                                     &
     &      (CG_param, DJDS_param, nod_comm, node, tbl_crs, mat_crs)
!$omp workshare
         d2_dz(1:node%numnod) = sol_mk_crs(1:node%numnod)
!$omp end workshare
       end if
!
       call cal_sol_d2_vart_width(node%numnod)
!
      else
        call allocate_delta_z(node%numnod, ele%numele)
        call int_edge_vart_width                                        &
     &     (ele%numele, edge, num_int, g_FEM, jac_1d)
        call cal_sol_vart_width(node%numnod)
!
        call int_edge_diff_vart_w                                       &
     &     (ele, edge, num_int, spf_1d, g_FEM, jac_1d)
        call cal_sol_diff_vart_width(node%numnod)
!
        call int_edge_d2_vart_w                                         &
     &     (node, ele, edge, num_int, spf_1d, g_FEM, jac_1d)
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
