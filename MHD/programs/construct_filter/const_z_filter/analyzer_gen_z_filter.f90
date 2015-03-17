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
      use m_geometry_parameter
      use m_geometry_data
      use m_iccg_parameter
      use m_crs_connect
      use m_crs_matrix
      use m_matrix_data_4_djds
!
      use m_gauss_points
      use m_fem_gauss_int_coefs
      use m_commute_filter_z
      use m_neibor_data_z
      use m_filter_values
      use m_work_4_integration
      use m_matrix_4_commutation
      use m_int_commtative_filter
      use m_gauss_integration
      use m_int_edge_data
      use m_jacobians_4_edge
      use m_matrix_4_LU
      use const_delta_z_analytical

      use const_crs_connect_commute_z
      use DJDS_precond_solve33
      use DJDS_precond_solveNN

!      use cal_delta_z_4_z_filter
      use set_diff_position_z_filter
      use set_vert_diff_z_filter
      use int_edge_norm_nod_z_filter
      use int_edge_moment_z_filter
      use int_edge_horiz_filter_peri
      use int_edge_commute_z_filter

      use input_control_gen_z_filter
      use choose_filter_moments
      use calcs_by_LUsolver
      use const_connect_2_n_filter
      use construct_commute_matrix
      use copy_1darray_2_2darray
      use switch_crs_matrix
      use cal_jacobian_linear_1d
      use set_neib_nod_z
      use set_neib_ele_z
      use set_neib_connect_z
      use set_matrices_4_z_filter
      use write_z_filter_4_nod
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
      NB_crs = nfilter2_3
      if (my_rank.eq.0) write(*,*) 'set_crs_connect_commute_z'
      call set_crs_connect_commute_z
!
!
!
      if (my_rank.eq.0) write(*,*) 'allocate_int_edge_data'
      call allocate_int_edge_data
      call set_spatial_difference(n_int)
!
      if (my_rank.eq.0) write(*,*) 'cal_delta_z_analytical'
       call cal_delta_z_analytical
!      call cal_delta_z
!
!      call check_crs_connect(my_rank, numnod)
!      call check_communication_data
!
!    set information for filtering for node
!
      call allocate_neib_nod
      if (my_rank.eq.0) write(*,*) 's_set_neib_nod_z'
      call s_set_neib_nod_z(internal_node, nfilter2_2, numfilter+1,     &
     &    nneib_nod, ineib_nod)
      if (my_rank.eq.0) write(*,*) 'set_connect_2_n_filter'
      call set_connect_2_n_filter
!      call check_neib_nod(my_rank)
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
      call s_choose_filter_moments
!
       num_inte = nfilter6_1 + 1
       call allocate_gauss_points(n_int_points)
      if (my_rank.eq.0) write(*,*) 'construct_gauss_coefs'
       call construct_gauss_coefs
       call allocate_work_4_integration
       call allocate_work_4_commute
!
       call allocate_matrix_4_commutation
!
      if (my_rank.eq.0) write(*,*) 'int_edge_norm_nod'
       call int_edge_norm_nod
!       call check_nod_normalize_matrix(my_rank)
!
       write(*,*) 'allocate_crs_mat_data'
       NB_crs = ncomp_mat
       call allocate_crs_mat_data
!
       call set_matrix_4_border
       write(*,*) 's_const_commute_matrix'
       call s_const_commute_matrix
       write(*,*) 's_switch_crs_matrix'
       call s_switch_crs_matrix
       write(*,*) 'check_crs_matrix_components'
       call check_crs_matrix_components(my_rank)
!
!      goto 999
!
!C===
!
!C
!C-- solve matrix
      write(*,*) 'METHOD_crs: ', METHOD_crs
      if ( METHOD_crs .eq. 'LU' ) then
        call solve_z_commute_LU
      else
        if   (SOLVER_crs.eq.'block33'                                   &
     &    .or. SOLVER_crs.eq.'BLOCK33') then
          write(*,*) 'solve_by_djds_solver33'
          call solve_by_djds_solver33(ierr)
        else if (SOLVER_crs.eq.'blockNN'                                &
     &    .or. SOLVER_crs.eq.'BLOCKNN') then
          write(*,*) 'solve_by_djds_solverNN'
          call solve_by_djds_solverNN(ierr)
        end if
      end if
!
!    construct commutative filter
!
!
       ndep_filter = ncomp_mat
       call allocate_int_commute_filter(numnod, numele)
!
       write(*,*) 's_copy_1darray_2_2darray'
       call s_copy_1darray_2_2darray(ncomp_mat,numnod,c_filter,X_crs)
       call deallocate_crs_mat_data
!
       write(*,*) 's_set_neib_nod_z'
       call s_set_neib_nod_z(numnod, ncomp_mat, nside, nneib_nod2,      &
     &    ineib_nod2)
!       call check_neib_nod_2nd(my_rank)
       write(*,*) 's_set_neib_ele_z'
       call s_set_neib_ele_z(numele, ncomp_mat, nside, nneib_ele2,      &
     &    ineib_ele2)
!       call check_neib_ele_2nd(my_rank, numele)
!
       call int_edge_filter_peri(ndep_filter, totalnod_x, xsize,        &
     &      xmom_h_x, xmom_ht_x)
       call int_edge_filter_peri(ndep_filter, totalnod_y, ysize,        &
     &      xmom_h_y, xmom_ht_y)
       write(*,*) 'xmom_ht_x', xmom_ht_x
       write(*,*) 'xmom_ht_x', xmom_ht_y
       if(my_rank.eq.0) write(*,*) 'int_edge_commutative_filter'
       call int_edge_commutative_filter(numnod, numele,                 &
     &     xx(1,3), ie_edge)
!       call check_int_commutative_filter(my_rank, numnod)
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
