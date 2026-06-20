!
!      module analyzer_gen_z_filter
!..................................................
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_analyzer_gen_z_filter
!      subroutine analyze_gen_z_filter
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
      use t_solver_SR
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
!>      Structure of communication flags
      type(send_recv_status), save :: SR_sig_f
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), save :: SR_r_f
!
      logical :: flag_analytical = .TRUE.
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer_gen_z_filter
!
      use calypso_mpi
!
      use m_commute_filter_z
      use m_z_filter_values
      use m_int_commtative_filter
!
      use t_neighbour_data_z
      use t_neighbour_index_z
      use t_vart_edge_width
      use t_z_int_edge_data
!
      use const_delta_z_analytical

      use const_crs_connect_commute_z
      use solve_precond_DJDS

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
      use set_matrices_4_z_filter
      use copy_matrix_2_djds_array
      use write_z_filter_4_nod
      use cal_delta_z_4_z_filter
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
      type(neighbour_data_z), save :: neib_z1
      type(z_int_edge_data), save :: z_int_edge1
      type(z_filter_work), save :: zfilter_wk1
      type(edge_z_width), save :: dz_plane1
!
      type(neighbour_data_z), save :: neib_z2
!
      real(kind = kreal), allocatable:: sk_norm_n(:)
      real(kind = kreal), allocatable:: d_norm_nod(:,:,:)
!
      real(kind = kreal) :: INITtime, PRECtime
      real(kind = kreal) :: COMPtime, COMMtime
      integer(kind=kint) :: itr_res, ierr
!
!
      call elapsed_label_4_Zfilter
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
      if(my_rank .eq. 0) write(*,*) 'init_int_z_edge_data'
      call init_int_z_edge_data                                         &
     &   (z_filter_mesh1%node, z_filter_mesh1%ele, edge_z_filter1,      &
     &    i_int_z_filter, jacs_z1%g_FEM, jacs_z1%jac_1d_l, z_int_edge1)
!
      if(my_rank.eq.0) write(*,*) 'cal_delta_z_analytical'
      if(flag_analytical) then
        call cal_delta_z_analytical(z_filter_mesh1%ele, edge_z_filter1, &
     &                              z_filter_mesh1%node, dz_plane1)
      else
        call cal_delta_z(CG_param_z, DJDS_param_z,                      &
     &      z_filter_mesh1%nod_comm, z_filter_mesh1%node,               &
     &      z_filter_mesh1%ele, edge_z_filter1, spf_1d_z,               &
     &      jacs_z1%g_FEM, jacs_z1%jac_1d_l, z_int_edge1, dz_plane1,    &
     &      tbl_crs_z, mat_crs_z, SR_sig_f, SR_r_f)
!
        call check_crs_connect                                           &
     &     (my_rank, z_filter_mesh1%node%numnod, tbl_crs_z)
      end if
!
!    set information for filtering for node
!
      call init_z_neighbour                                             &
     &   (z_filter_mesh1%node%internal_node, totalele,                  &
     &    nfilter2_2, nfilter2_1, (numfilter+1), numfilter, neib_z1)
!      write(50+my_rank,*) 'neib_z1'
!      call check_z_neighbour(my_rank,                                  &
!     &    z_filter_mesh1%node%internal_node, totalele, neib_z1)
!
!    set information for filtering for element
!
      call alloc_z_neib_index(z_filter_mesh1%node%numnod, nfilter2_1,   &
     &                        zfilter_wk1)
      if(my_rank .eq. 0) write(*,*) 'set_connect_2_n_filter'
      call set_connect_2_n_filter(z_filter_mesh1%node,                  &
     &    neib_z1%nneib_nod, zfilter_wk1%ncomp_z_st)
      if (my_rank.eq.0) write(*,*) 's_set_neib_connect_z'
      call s_set_neib_connect_z(totalele, nfilter2_1,                   &
     &                          neib_z1%nneib_ele, zfilter_wk1%jdx_z)
!      call check_z_neib_index(my_rank, z_filter_mesh1%node%numnod,       &
!     &                        totalele, zfilter_wk1)
!
!     det dz / dxi
!
      if (my_rank.eq.0) write(*,*) 'set_difference_of_position'
      call set_difference_of_position                                   &
     &   (z_filter_mesh1%node, edge_z_filter1,                          &
     &    neib_z1%nneib_ele, neib_z1%ineib_ele, zfilter_wk1%alpha)
!      call check_difference_of_position(my_rank, totalele, neib_z1,    &
!     &                                  zfilter_wk1)
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
!
      allocate(sk_norm_n(0:nfilter6_1))
      sk_norm_n = 0.0d0
!
      allocate(d_norm_nod(z_filter_mesh1%node%numnod,                   &
     &                    nfilter2_3, 0:nfilter2_3))
      d_norm_nod(1:z_filter_mesh1%node%numnod,                          &
     &           1:nfilter2_3,0:nfilter2_3) =   0.0d0
!
      if (my_rank.eq.0) write(*,*) 'int_edge_norm_nod'
       call int_edge_norm_nod(z_filter_mesh1%node, z_filter_mesh1%ele,  &
      &    edge_z_filter1, gauss_z, neib_z1, z_int_edge1%dz_ele,        &
      &    g_z_int, sk_norm_n, d_norm_nod)
!       call check_nod_normalize_matrix                                 &
!     &    (my_rank, z_filter_mesh1%node%numnod, d_norm_nod)
!
       write(*,*) 'alloc_crs_mat_data'
       mat_crs_z%NB_crs = ncomp_mat
       call alloc_crs_mat_data(tbl_crs_z, mat_crs_z)
!
       call set_matrix_4_border(z_filter_mesh1%node%numnod,             &
     &                          neib_z1, mat_crs_z)
       write(*,*) 's_const_commute_matrix'
       call s_const_commute_matrix(z_filter_mesh1%node%numnod,          &
     &     neib_z1, zfilter_wk1, dz_plane1%delta_z_n, d_norm_nod,       &
     &     mat_crs_z)
       deallocate(d_norm_nod)
       call dealloc_z_int_edge_data(z_int_edge1)
!
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
     &        mat_crs_z, djds_tbl_z, djds_mat_z, SR_sig_f, SR_r_f,      &
     &        itr_res, ierr, INITtime, PRECtime, COMPtime, COMMtime)
        else if (mat_crs_z%SOLVER_crs.eq.'blockNN'                      &
     &    .or. mat_crs_z%SOLVER_crs.eq.'BLOCKNN') then
          write(*,*) 'solve_by_djds_solverNN'
          call solve_by_djds_solverNN                                   &
     &       (z_filter_mesh1%node, z_filter_mesh1%nod_comm, CG_param_z, &
     &        mat_crs_z, djds_tbl_z, djds_mat_z, SR_sig_f, SR_r_f,      &
     &        itr_res, ierr, INITtime, PRECtime, COMPtime, COMMtime)
        end if
      end if
!
      if(flag_Zfilte_time) then
        elps1%elapsed(ist_elapsed_Zfilter+1)                            &
     &        = elps1%elapsed(ist_elapsed_ZFILTER+1) + INITtime
        elps1%elapsed(ist_elapsed_ZFILTER+2)                            &
     &        = elps1%elapsed(ist_elapsed_ZFILTER+2) + COMPtime
        elps1%elapsed(ist_elapsed_ZFILTER+3)                            &
     &        = elps1%elapsed(ist_elapsed_ZFILTER+3) + COMMtime
      end if
!
!    construct commutative filter
!
!
       ndep_filter = ncomp_mat
      call allocate_int_commute_filter(z_filter_mesh1%node%numnod)
      call init_z_neighbour                                             &
     &   (z_filter_mesh1%node%numnod, z_filter_mesh1%ele%numele,        &
     &    ncomp_mat, ncomp_mat, nside, nside, neib_z2)
!       write(50+my_rank,*) 'neib_z2'
!       call check_z_neighbour(my_rank, z_filter_mesh1%node%numnod,     &
!     &                        z_filter_mesh1%ele%numele, neib_z2)
!
       write(*,*) 's_copy_1darray_2_2darray'
       call s_copy_1darray_2_2darray                                    &
     &    (ncomp_mat, z_filter_mesh1%node%numnod,                       &
     &     c_filter, mat_crs_z%X_crs)
       call dealloc_crs_mat_data(mat_crs_z)
!
!
       call int_edge_filter_peri(ndep_filter, totalnod_x, xsize,        &
     &     gauss_z, xmom_h_x, xmom_ht_x, sk_norm_n, g_z_int)
       call int_edge_filter_peri(ndep_filter, totalnod_y, ysize,        &
     &     gauss_z, xmom_h_y, xmom_ht_y, sk_norm_n, g_z_int)
!
       if(my_rank.eq.0) write(*,*) 'int_edge_commutative_filter'
       call int_edge_commutative_filter                                 &
     &    (z_filter_mesh1%node%numnod, z_filter_mesh1%node%xx(1,3),     &
     &     z_filter_mesh1%ele%numele, edge_z_filter1%ie_edge,           &
     &     z_int_edge1%dz_ele, gauss_z, neib_z2, sk_norm_n, g_z_int)
!       call check_int_commutative_filter                               &
!     &    (my_rank, z_filter_mesh1%node%numnod)
!
       if(my_rank.eq.0) write(*,*) 'int_edge_moment'
       call int_edge_moment                                             &
     &    (z_filter_mesh1%node%numnod, z_filter_mesh1%ele%numele,       &
     &     edge_z_filter1, i_int_z_filter, spf_1d_z,                    &
     &     jacs_z1%g_FEM, jacs_z1%jac_1d_l, z_int_edge1%mk_z)
       call dealloc_edge_shape_func(spf_1d_z)
!
!    output results
!
       call write_filter_4_nod(z_filter_mesh1%node, z_filter_mesh1%ele, &
     &                         edge_z_filter1, dz_plane1, neib_z2)
!
       deallocate(sk_norm_n)
!
       call deallocate_filter_values
       call dealloc_work_4_integration(g_z_int)
       call dealloc_gauss_points(gauss_z)
!
       call deallocate_int_commute_filter
       call dealloc_z_neighbour(neib_z2)
       call dealloc_z_neighbour(neib_z1)
!
!    finerizing
!
       if (my_rank.eq.0) write (*,*) itr_res, "  iters"
!
       end subroutine init_analyzer_gen_z_filter
!
! ----------------------------------------------------------------------
!
      subroutine analyze_gen_z_filter
!
      use calypso_mpi
!
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze_gen_z_filter'
!
        end subroutine analyze_gen_z_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_nod_normalize_matrix(id_rank,                    &
     &                                      numnod, d_norm_nod)
!
      use m_commute_filter_z
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in)                                    &
     &           :: d_norm_nod(numnod,nfilter2_3,0:nfilter2_3)
      integer(kind = kint) :: i, k
!
!
      do k = 0, nfilter2_3
        do i = 1, numnod
        write(id_rank+60,*) 'd_norm_nod (node_id,order) = ', i, k
        write(id_rank+60,'(1p5e16.8)') d_norm_nod(i,1:nfilter2_3,k)
        end do
      end do
!
      end subroutine check_nod_normalize_matrix
!
! -----------------------------------------------------------------------
!
      end module analyzer_gen_z_filter
