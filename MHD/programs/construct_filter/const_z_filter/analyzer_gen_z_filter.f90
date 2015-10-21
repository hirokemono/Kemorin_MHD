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
        implicit none
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
      use m_geometry_data
      use m_nod_comm_table
      use m_iccg_parameter
      use m_crs_matrix
!
      use m_gauss_points
      use m_fem_gauss_int_coefs
      use m_commute_filter_z
      use m_neibor_data_z
      use m_z_filter_values
      use m_work_4_integration
      use m_matrix_4_z_commute
      use m_int_commtative_filter
      use m_gauss_integration
      use m_int_edge_data
      use m_jacobians_4_edge
      use m_matrix_4_LU
      use const_delta_z_analytical

      use const_crs_connect_commute_z
      use solve_precond_DJDS

      use set_diff_position_z_filter
      use set_vert_diff_z_filter
      use int_edge_norm_nod_z_filter
      use int_edge_moment_z_filter
      use int_edge_horiz_filter_peri
      use int_edge_commute_z_filter

      use int_gaussian_moments
      use int_linear_moments
      use int_tophat_moments

      use input_control_gen_z_filter
      use calcs_by_LUsolver
      use const_connect_2_n_filter
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
      use t_solver_djds
!
      type(DJDS_ordering_table) :: djds_tbl1
      type(DJDS_MATRIX) :: djds_mat1
!
      integer(kind=kint) :: n_int, ierr
      integer (kind = kint), parameter :: n_int_points = 200
!
!C
!C-- read CNTL DATA

      call s_input_control_4_z_commute
!
!C
!C     set gauss points
!C===
!
!    set shape functions for 1 dimensional
!
      n_int = i_int_z_filter
      n_point = i_int_z_filter
      if (my_rank.eq.0) write(*,*) 's_cal_jacobian_linear_1d'
      call s_cal_jacobian_linear_1d(i_int_z_filter)
!
!   construct FEM mesh for x direction
!
      mat1_crs%NB_crs = nfilter2_3
      if (my_rank.eq.0) write(*,*) 'set_crs_connect_commute_z'
      call set_crs_connect_commute_z
!
!
!
      if (my_rank.eq.0) write(*,*) 'allocate_int_edge_data'
      call allocate_int_edge_data(node1%numnod, ele1%numele)
      call set_spatial_difference(n_int)
!
      if (my_rank.eq.0) write(*,*) 'cal_delta_z_analytical'
       call cal_delta_z_analytical
!      call cal_delta_z
!
!      call check_crs_connect(my_rank, node1%numnod, tbl1_crs)
!      call check_communication_data
!
!    set information for filtering for node
!
      call allocate_neib_nod(node1%numnod, node1%internal_node)
      if (my_rank.eq.0) write(*,*) 's_set_neib_nod_z'
      call s_set_neib_nod_z(node1%internal_node,                        &
     &    nfilter2_2, numfilter+1,   nneib_nod, ineib_nod)
      if (my_rank.eq.0) write(*,*) 'set_connect_2_n_filter'
      call set_connect_2_n_filter
!      call check_neib_nod(my_rank, node1%numnod, node1%internal_node)
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
      call set_difference_of_position
!      call check_difference_of_position(my_rank)
!
!   set moments of filter
!
      if (my_rank.eq.0) write(*,*) 'allocate_filter_values'
      call allocate_filter_values(numfilter)
      write(*,*) 'allocate_filter', nfilter6_1, nfilter2_1, n_int
!
      if ( iflag_filter .eq. 0) then
        call int_tophat_moment_infty(nfilter6_1,f_mom_full,f_width)
      else if (iflag_filter .eq. 1) then
        call int_linear_moment_infty(nfilter6_1,f_mom_full,f_width)
      else
        call int_gaussian_moment_infty(nfilter6_1,f_mom_full,f_width)
      end if
!
       num_inte = nfilter6_1 + 1
       call allocate_gauss_points(n_int_points)
      if (my_rank.eq.0) write(*,*) 'construct_gauss_coefs'
       call construct_gauss_coefs
       call allocate_work_4_integration
       call allocate_work_4_commute
!
       call allocate_matrix_4_commutation(node1%numnod)
!
      if (my_rank.eq.0) write(*,*) 'int_edge_norm_nod'
       call int_edge_norm_nod
!       call check_nod_normalize_matrix(my_rank, node1%numnod)
!
       write(*,*) 'alloc_crs_mat_data'
       mat1_crs%NB_crs = ncomp_mat
       call alloc_crs_mat_data(tbl1_crs, mat1_crs)
!
       call set_matrix_4_border(node1%numnod)
       write(*,*) 's_const_commute_matrix'
       call s_const_commute_matrix
       write(*,*) 's_switch_crs_matrix'
       call s_switch_crs_matrix(tbl1_crs, mat1_crs)
       write(*,*) 'check_crs_matrix_comps'
       call check_crs_matrix_comps(my_rank, tbl1_crs, mat1_crs)
!
!      goto 999
!
!C===
!
!C
!C-- solve matrix
      write(*,*) 'METHOD_crs: ', mat1_crs%METHOD_crs
      if ( mat1_crs%METHOD_crs .eq. 'LU' ) then
        call solve_z_commute_LU(node1%numnod)
      else
        call transfer_crs_2_djds_matrix(node1, nod_comm,                &
     &      tbl1_crs, mat1_crs, djds_tbl1, djds_mat1)
!
        if   (mat1_crs%SOLVER_crs.eq.'block33'                          &
     &    .or. mat1_crs%SOLVER_crs.eq.'BLOCK33') then
          write(*,*) 'solve_by_djds_solver33'
          call solve_by_djds_solver33                                   &
     &       (node1, nod_comm, mat1_crs, djds_tbl1, djds_mat1, ierr)
        else if (mat1_crs%SOLVER_crs.eq.'blockNN'                       &
     &    .or. mat1_crs%SOLVER_crs.eq.'BLOCKNN') then
          write(*,*) 'solve_by_djds_solverNN'
          call solve_by_djds_solverNN                                   &
     &       (node1, nod_comm, mat1_crs, djds_tbl1, djds_mat1, ierr)
        end if
      end if
!
!    construct commutative filter
!
!
       ndep_filter = ncomp_mat
       call allocate_int_commute_filter(node1%numnod, ele1%numele)
!
       write(*,*) 's_copy_1darray_2_2darray'
       call s_copy_1darray_2_2darray                                    &
     &    (ncomp_mat, node1%numnod, c_filter, mat1_crs%X_crs)
       call dealloc_crs_mat_data(mat1_crs)
!
       write(*,*) 's_set_neib_nod_z'
       call s_set_neib_nod_z(node1%numnod, ncomp_mat, nside,            &
     &     nneib_nod2, ineib_nod2)
!       call check_neib_nod_2nd(my_rank)
       write(*,*) 's_set_neib_ele_z'
       call s_set_neib_ele_z(ele1%numele, ncomp_mat, nside, nneib_ele2, &
     &    ineib_ele2)
!       call check_neib_ele_2nd(my_rank, ele1%numele)
!
       call int_edge_filter_peri(ndep_filter, totalnod_x, xsize,        &
     &      xmom_h_x, xmom_ht_x)
       call int_edge_filter_peri(ndep_filter, totalnod_y, ysize,        &
     &      xmom_h_y, xmom_ht_y)
       write(*,*) 'xmom_ht_x', xmom_ht_x
       write(*,*) 'xmom_ht_x', xmom_ht_y
       if(my_rank.eq.0) write(*,*) 'int_edge_commutative_filter'
       call int_edge_commutative_filter(node1%numnod, ele1%numele,      &
     &     node1%xx(1:node1%numnod,3), edge1%ie_edge)
!       call check_int_commutative_filter(my_rank, node1%numnod)
!
       if(my_rank.eq.0) write(*,*) 'int_edge_moment'
       call int_edge_moment(n_int)
!
!    output results
!
       call write_filter_4_nod
!
       call deallocate_filter_values
       call deallocate_gauss_points
       call deallocate_work_4_integration
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
