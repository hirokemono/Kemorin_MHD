!analyzer_test_dz.f90
!      module analyzer_test_dz
!..................................................
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_analyzer
!
      module analyzer_test_dz
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_surface_data
      use t_jacobians
      use t_edge_data
      use t_iccg_parameter
      use t_crs_connect
      use t_crs_matrix
      use t_shape_functions
      use t_solver_SR
!
      implicit none
!
      type(CRS_matrix_connect), save :: tbl_crs_z
      type(CRS_matrix), save :: mat_crs_z
!
!>     structure for node data (position)
      type(mesh_geometry), save :: z_filter_mesh2
!>     Structure for edge data
      type(edge_data), save :: edge_z_filter2
!>      structure of surface data (geometry and connectivity)
      type(surface_data), save :: surf_z_filter2
!>     Stracture for Jacobians
      type(jacobians_type), save :: jacs_z2
!
      type(CG_poarameter), save :: CG_param_z
      type(DJDS_poarameter), save :: DJDS_param_z
      type(edge_shape_function), save :: spf_1d_z
!
!>      Structure of communication flags
      type(send_recv_status), save :: SR_sig_f
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), save :: SR_r_f
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use calypso_mpi
!
      use m_commute_filter_z
      use m_int_edge_data
      use m_int_edge_vart_width

      use input_control_gen_z_filter
      use const_crs_connect_commute_z
      use cal_jacobian_linear_1d
      use cal_delta_z_4_z_filter
!
      integer (kind= kint), parameter :: id_delta_z = 15
      integer (kind= kint) :: i, n_int
!
      nprocs = 1
!
!      call elapsed_label_4_Zfilter
!C
!C
!C-- CNTL DATA
      call s_input_control_4_z_commute(z_filter_mesh2%nod_comm,         &
     &    z_filter_mesh2%node, z_filter_mesh2%ele,                      &
     &    surf_z_filter2, edge_z_filter2, mat_crs_z,                    &
     &    CG_param_z, DJDS_param_z)
!C
!C     set gauss points
!C===
!
      n_int = i_int_z_filter
      if (my_rank.eq.0) write(*,*) 'const_jacobian_linear_1d'
      call const_jacobian_linear_1d(n_int, z_filter_mesh2%node,         &
     &    surf_z_filter2, edge_z_filter2, spf_1d_z, jacs_z2)
!
      if (my_rank.eq.0) write(*,*) 'set_crs_connect_commute_z'
      call set_crs_connect_commute_z(z_filter_mesh2%node, tbl_crs_z)
!
      if (my_rank.eq.0) write(*,*) 'allocate_int_edge_data'
      call allocate_int_edge_data                                       &
     &   (z_filter_mesh2%node%numnod, z_filter_mesh2%ele%numele)
      call set_spatial_difference(z_filter_mesh2%ele%numele,            &
     &    n_int, jacs_z2%g_FEM, jacs_z2%jac_1d_l)
!
!
      call cal_delta_z(CG_param_z, DJDS_param_z,                        &
     &   z_filter_mesh2%nod_comm, z_filter_mesh2%node,                  &
     &   z_filter_mesh2%ele, edge_z_filter2, spf_1d_z,                  &
     &   jacs_z2%g_FEM, jacs_z2%jac_1d_l, tbl_crs_z,                    &
     &   mat_crs_z, SR_sig_f, SR_r_f)
      call dealloc_edge_shape_func(spf_1d_z)
!
!C===
!
!       call dealloc_crs_mat_data(mat_crs_z)
!       call dealloc_crs_connect(tbl_crs_z)
!
       open (id_delta_z,file='delta_z.0.dat')
!
      write(id_delta_z,*) 'inod, z, delta z, diff.'
      do i = 1, z_filter_mesh2%node%numnod
        write(id_delta_z,'(i15,1p20E25.15e3)')                          &
     &        i, z_filter_mesh2%node%xx(i,3),                           &
     &        delta_z(i), delta_dz(i), d2_dz(i)
      end do
!
      close(id_delta_z)
!
      if (my_rank.eq.0) write (*,*) mat_crs_z%ITERactual, "  iters"

      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_dz
