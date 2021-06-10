!
!      module analyzer_gen_z_filter
!..................................................
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_gen_z_filter
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
      use t_gauss_points
      use t_shape_functions
!
      implicit none
!
      type(CRS_matrix_connect), save :: tbl_crs_z
      type(CRS_matrix), save :: mat_crs_z
!
!>  structure for node data (position)
      type(mesh_geometry), save :: z_filter_mesh1
!>     Structure for edge data
      type(edge_data), save :: edge_z_filter1
!
!>      structure of surface data (geometry and connectivity)
      type(surface_data), save :: surf_z_filter1
!>     Stracture for Jacobians
      type(jacobians_type), save :: jacs_z1
!
      type(gauss_points), private :: gauss_z
      type(gauss_integrations), save, private :: g_z_int
!
      type(CG_poarameter), save :: CG_param_z
      type(DJDS_poarameter), save :: DJDS_param_z
      type(edge_shape_function), save :: spf_1d_z
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
      use m_neibor_data_z
      use m_z_filter_values
      use m_work_4_integration
      use m_matrix_4_z_commute
      use m_int_commtative_filter
      use m_int_edge_data
      use m_matrix_4_LU
      use m_solver_SR
      use const_delta_z_analytical

      use const_crs_connect_commute_z
      use solve_precond_DJDS

      use set_diff_position_z_filter
      use int_edge_norm_nod_z_filter
      use int_edge_moment_z_filter
      use int_edge_horiz_filter_peri
      use int_edge_commute_z_filter

      use int_gaussian_moments
      use int_linear_moments
      use int_tophat_moments

      use input_control_gen_z_filter
      use calcs_by_LUsolver
      use const_z_commute_matrix
      use copy_1darray_2_2darray
      use switch_crs_matrix
      use cal_jacobian_linear_1d
      use set_neib_nod_z
      use set_neib_ele_z
      use set_neib_connect_z
      use set_matrices_4_z_filter
      use copy_matrix_2_djds_array
      use write_z_filter_4_nod
!
      use t_crs_connect
      use t_crs_matrix
      use t_solver_djds
!
      type(CRS_matrix_connect), save :: tbl_crs_z
      type(CRS_matrix), save :: mat_crs_z
!
      type(DJDS_ordering_table) :: djds_tbl_z
      type(DJDS_MATRIX) :: djds_mat_z
!
      integer(kind=kint) :: itr_res, ierr
!
!C
!C-- read CNTL DATA
      call s_input_control_4_z_commute                                  &
     &  (z_filter_mesh1%nod_comm, z_filter_mesh1%node,                  &
     &   z_filter_mesh1%ele, surf_z_filter1, edge_z_filter1, mat_crs_z, &
     &   CG_param_z, DJDS_param_z)
!
!C
!C     set gauss points
!C===
!
!    set shape functions for 1 dimensional
!
      if (my_rank.eq.0) write(*,*) 'const_jacobian_linear_1d'
      call const_jacobian_linear_1d                                     &
     &   (i_int_z_filter, z_filter_mesh1%node,                          &
     &    surf_z_filter1, edge_z_filter1, spf_1d_z, jacs_z1)
!
!   construct FEM mesh for x direction
!
      mat_crs_z%NB_crs = nfilter2_3
      if (my_rank.eq.0) write(*,*) 'set_crs_connect_commute_z'
      call set_crs_connect_commute_z(z_filter_mesh1%node, tbl_crs_z)
!
!
!
      if (my_rank.eq.0) write(*,*) 'allocate_int_edge_data'
      call allocate_int_edge_data                                       &
     &   (z_filter_mesh1%node%numnod, z_filter_mesh1%ele%numele)
      call set_spatial_difference(z_filter_mesh1%ele%numele,            &
     &    i_int_z_filter, jacs_z1%g_FEM, jacs_z1%jac_1d_l)
!
      if (my_rank.eq.0) write(*,*) 'cal_delta_z_analytical'
       call cal_delta_z_analytical                                      &
     &    (z_filter_mesh1%node, z_filter_mesh1%ele,                     &
     &     edge_z_filter1, jacs_z1%g_FEM, jacs_z1%jac_1d_l)
!      call cal_delta_z(CG_param_z, DJDS_param_z,                       &
!     &  z_filter_mesh1%nod_comm, z_filter_mesh1%node,                  &
!     &  z_filter_mesh1%ele, edge_z_filter1, spf_1d_z,                  &
!     &  jacs_z1%g_FEM, jacs_z1%jac_1d_l,tbl_crs_z, mat_crs_z)
!
!      call check_crs_connect                                           &
!     &   (my_rank, z_filter_mesh1%node%numnod, tbl_crs_z)
!      call check_communication_data
!
!    set information for filtering for node
!
      call allocate_neib_nod(z_filter_mesh1%node%numnod,                &
     &                       z_filter_mesh1%node%internal_node)
      if (my_rank.eq.0) write(*,*) 's_set_neib_nod_z'
      call s_set_neib_nod_z(z_filter_mesh1%node%internal_node,          &
     &    nfilter2_2, numfilter+1, nneib_nod, ineib_nod)
      if (my_rank.eq.0) write(*,*) 'set_connect_2_n_filter'
      call set_connect_2_n_filter(z_filter_mesh1%node)
!      call check_neib_nod(my_rank, z_filter_mesh1%node%numnod,         &
!     &                             z_filter_mesh1%node%internal_node)
!
!    set information for filtering for element
!
      call allocate_neib_ele
      if (my_rank.eq.0) write(*,*) 's_set_neib_ele_z'
      call s_set_neib_ele_z(totalele, nfilter2_1, numfilter, nneib_ele, &
     &    ineib_ele)
      if (my_rank.eq.0) write(*,*) 's_set_neib_connect_z'
      call s_set_neib_connect_z
!      call check_neib_ele(my_rank)
!
!     det dz / dxi
!
      if (my_rank.eq.0) write(*,*) 'set_difference_of_position'
      call set_difference_of_position                                   &
     &   (z_filter_mesh1%node, edge_z_filter1)
!      call check_difference_of_position(my_rank)
!
!   set moments of filter
!
      if (my_rank.eq.0) write(*,*) 'allocate_filter_values'
      call allocate_filter_values(numfilter)
      write(*,*) 'allocate_filter',                                     &
     &            nfilter6_1, nfilter2_1, i_int_z_filter
!
      if ( iflag_filter .eq. 0) then
        call int_tophat_moment_infty(nfilter6_1,f_mom_full,f_width)
      else if (iflag_filter .eq. 1) then
        call int_linear_moment_infty(nfilter6_1,f_mom_full,f_width)
      else
        call int_gaussian_moment_infty(nfilter6_1,f_mom_full,f_width)
      end if
!
      if (my_rank.eq.0) write(*,*) 'construct_gauss_coefs'
      call construct_gauss_coefs(i_int_z_filter, gauss_z)
      call alloc_work_4_integration                                     &
     &  ((nfilter6_1 + 1), gauss_z%n_point, g_z_int)
      call allocate_work_4_commute
!
      call allocate_matrix_4_commutation(z_filter_mesh1%node%numnod)
!
      if (my_rank.eq.0) write(*,*) 'int_edge_norm_nod'
       call int_edge_norm_nod                                           &
     &    (z_filter_mesh1%node, edge_z_filter1, gauss_z, g_z_int)
!       call check_nod_normalize_matrix                                 &
!     &     (my_rank, z_filter_mesh1%node%numnod)
!
       write(*,*) 'alloc_crs_mat_data'
       mat_crs_z%NB_crs = ncomp_mat
       call alloc_crs_mat_data(tbl_crs_z, mat_crs_z)
!
       call set_matrix_4_border(z_filter_mesh1%node%numnod, mat_crs_z)
       write(*,*) 's_const_commute_matrix'
       call s_const_commute_matrix                                      &
     &    (z_filter_mesh1%node%numnod, mat_crs_z)
       write(*,*) 's_switch_crs_matrix'
       call s_switch_crs_matrix(tbl_crs_z, mat_crs_z)
       write(*,*) 'check_crs_matrix_comps'
       call check_crs_matrix_comps(my_rank, tbl_crs_z, mat_crs_z)
!
!      goto 999
!
!C===
!
!C
!C-- solve matrix
      write(*,*) 'METHOD_crs: ', mat_crs_z%METHOD_crs
      if ( mat_crs_z%METHOD_crs .eq. 'LU' ) then
        call solve_z_commute_LU(z_filter_mesh1%node%numnod, mat_crs_z)
      else
        call transfer_crs_2_djds_matrix                                 &
     &     (z_filter_mesh1%node, z_filter_mesh1%nod_comm,               &
     &      tbl_crs_z, mat_crs_z, CG_param_z, DJDS_param_z,             &
     &      djds_tbl_z, djds_mat_z)
!
        if   (mat_crs_z%SOLVER_crs.eq.'block33'                         &
     &    .or. mat_crs_z%SOLVER_crs.eq.'BLOCK33') then
          write(*,*) 'solve_by_djds_solver33'
          call solve_by_djds_solver33                                   &
     &       (z_filter_mesh1%node, z_filter_mesh1%nod_comm, CG_param_z, &
     &        mat_crs_z, djds_tbl_z, djds_mat_z, SR_sig1, SR_r1,        &
     &        itr_res, ierr)
        else if (mat_crs_z%SOLVER_crs.eq.'blockNN'                      &
     &    .or. mat_crs_z%SOLVER_crs.eq.'BLOCKNN') then
          write(*,*) 'solve_by_djds_solverNN'
          call solve_by_djds_solverNN                                   &
     &       (z_filter_mesh1%node, z_filter_mesh1%nod_comm, CG_param_z, &
     &        mat_crs_z, djds_tbl_z, djds_mat_z, SR_sig1, SR_r1,        &
     &        itr_res, ierr)
        end if
      end if
!
!    construct commutative filter
!
!
       ndep_filter = ncomp_mat
       call allocate_int_commute_filter                                 &
      &   (z_filter_mesh1%node%numnod, z_filter_mesh1%ele%numele)
!
       write(*,*) 's_copy_1darray_2_2darray'
       call s_copy_1darray_2_2darray                                    &
     &    (ncomp_mat, z_filter_mesh1%node%numnod,                       &
     &     c_filter, mat_crs_z%X_crs)
       call dealloc_crs_mat_data(mat_crs_z)
!
       write(*,*) 's_set_neib_nod_z'
       call s_set_neib_nod_z(z_filter_mesh1%node%numnod,                &
     &     ncomp_mat, nside, nneib_nod2, ineib_nod2)
!       call check_neib_nod_2nd(my_rank)
       write(*,*) 's_set_neib_ele_z'
       call s_set_neib_ele_z(z_filter_mesh1%ele%numele,                 &
     &     ncomp_mat, nside, nneib_ele2, ineib_ele2)
!       call check_neib_ele_2nd(my_rank, z_filter_mesh1%ele%numele)
!
       call int_edge_filter_peri(ndep_filter, totalnod_x, xsize,        &
     &      xmom_h_x, xmom_ht_x, gauss_z, g_z_int)
       call int_edge_filter_peri(ndep_filter, totalnod_y, ysize,        &
     &      xmom_h_y, xmom_ht_y, gauss_z, g_z_int)
!
       if(my_rank.eq.0) write(*,*) 'int_edge_commutative_filter'
       call int_edge_commutative_filter                                 &
     &    (z_filter_mesh1%node%numnod, z_filter_mesh1%ele%numele,       &
     &     z_filter_mesh1%node%xx(1:z_filter_mesh1%node%numnod,3),      &
     &     edge_z_filter1%ie_edge, gauss_z, g_z_int)
!       call check_int_commutative_filter                               &
!     &    (my_rank, z_filter_mesh1%node%numnod)
!
       if(my_rank.eq.0) write(*,*) 'int_edge_moment'
       call int_edge_moment                                             &
     &    (z_filter_mesh1%node%numnod, z_filter_mesh1%ele%numele,       &
     &     edge_z_filter1, i_int_z_filter, spf_1d_z,                    &
     &     jacs_z1%g_FEM, jacs_z1%jac_1d_l)
       call dealloc_edge_shape_func(spf_1d_z)
!
!    output results
!
       call write_filter_4_nod(z_filter_mesh1%node, z_filter_mesh1%ele, &
     &     edge_z_filter1)
!
       call deallocate_filter_values
       call dealloc_work_4_integration(g_z_int)
       call dealloc_gauss_points(gauss_z)
!
!    finerizing
!
       if (my_rank.eq.0) write (*,*) itr_res, "  iters"
!
       end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use calypso_mpi
!
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_z_filter
