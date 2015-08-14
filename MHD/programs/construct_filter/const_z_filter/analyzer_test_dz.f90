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
      use calypso_mpi
      use m_geometry_data
      use m_crs_connect
      use m_crs_matrix
!
      use m_gauss_points
      use m_fem_gauss_int_coefs
      use m_commute_filter_z
      use m_int_edge_data
      use m_int_edge_vart_width

      use input_control_gen_z_filter
      use const_crs_connect_commute_z
      use cal_jacobian_linear_1d
      use cal_delta_z_4_z_filter
      use set_vert_diff_z_filter
!
      integer (kind= kint), parameter :: id_delta_z = 15
      integer (kind= kint) :: i, n_int
!
      nprocs = 1
!C
!C
!C-- CNTL DATA

      call s_input_control_4_z_commute
!C
!C     set gauss points
!C===
!
      n_int = i_int_z_filter
      n_point = n_int
      if (my_rank.eq.0) write(*,*) 's_cal_jacobian_linear_1d'
      call s_cal_jacobian_linear_1d(n_int)
!
      if (my_rank.eq.0) write(*,*) 'set_crs_connect_commute_z'
      call set_crs_connect_commute_z
!
      if (my_rank.eq.0) write(*,*) 'allocate_int_edge_data'
      call allocate_int_edge_data(node1%numnod, ele1%numele)
      call set_spatial_difference(n_int)
!
!
!
       call cal_delta_z
!
!C===
!
!       call deallocate_crs_mat_data
!       call deallocate_crs_connect
!
       open (id_delta_z,file='delta_z.0.dat')
!
      write(id_delta_z,*) 'inod, z, delta z, diff.'
      do i = 1, node1%numnod
        write(id_delta_z,'(i15,1p20E25.15e3)')                          &
     &        i, node1%xx(i,3), delta_z(i), delta_dz(i), d2_dz(i)
      end do
!
      close(id_delta_z)
!
      if (my_rank.eq.0) write (*,*) ITERactual, "  iters"

      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_dz
