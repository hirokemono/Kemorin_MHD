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
      use m_vertical_filter_utils
!
      use m_iccg_parameter
      use t_crs_connect
      use t_crs_matrix
!
      implicit none
!
      type(CRS_matrix_connect), save :: tbl_crs_z
      type(CRS_matrix), save :: mat_crs_z
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
      use m_fem_gauss_int_coefs
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
!C
!C
!C-- CNTL DATA
      call s_input_control_4_z_commute(z_filter_mesh%nod_comm,          &
     &    z_filter_mesh%node, z_filter_mesh%ele,                        &
     &    surf_z_filter, edge_z_filter, mat_crs_z, DJDS_param1)
!C
!C     set gauss points
!C===
!
      n_int = i_int_z_filter
      if (my_rank.eq.0) write(*,*) 's_cal_jacobian_linear_1d'
      call s_cal_jacobian_linear_1d                                     &
     &   (n_int, z_filter_mesh%node, z_filter_mesh%ele,                 &
     &    surf_z_filter, edge_z_filter, jac_z_l, jac_z_q)
!
      if (my_rank.eq.0) write(*,*) 'set_crs_connect_commute_z'
      call set_crs_connect_commute_z(z_filter_mesh%node, tbl_crs_z)
!
      if (my_rank.eq.0) write(*,*) 'allocate_int_edge_data'
      call allocate_int_edge_data                                       &
     &   (z_filter_mesh%node%numnod, z_filter_mesh%ele%numele)
      call set_spatial_difference(z_filter_mesh%ele%numele,             &
     &                            n_int, jac_z_l)
!
!
      call cal_delta_z                                                  &
     &   (DJDS_param1, z_filter_mesh%nod_comm, z_filter_mesh%node,      &
     &    z_filter_mesh%ele, edge_z_filter, jac_z_l,                    &
     &    tbl_crs_z, mat_crs_z)
!
!C===
!
!       call dealloc_crs_mat_data(mat_crs_z)
!       call dealloc_crs_connect(tbl_crs_z)
!
       open (id_delta_z,file='delta_z.0.dat')
!
      write(id_delta_z,*) 'inod, z, delta z, diff.'
      do i = 1, z_filter_mesh%node%numnod
        write(id_delta_z,'(i15,1p20E25.15e3)')                          &
     &        i, z_filter_mesh%node%xx(i,3),                            &
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
