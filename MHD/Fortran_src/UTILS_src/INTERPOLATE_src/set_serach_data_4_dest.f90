!
!      module set_serach_data_4_dest
!..................................................
!
      module set_serach_data_4_dest
!
!      modified by H. Matsui on Aug., 2006 
!
      use m_precision
!
      implicit none
!
!      subroutine s_set_serach_data_4_dest
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_serach_data_4_dest
!
      use calypso_mpi
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_pallalel_vector
      use m_ctl_params_4_gen_table
      use m_sphere_bin_4_table
      use m_interpolate_table_dest
      use m_interpolate_coefs_dest
      use m_work_const_itp_table
      use m_search_bolck_4_itp
!
      use set_2nd_geometry_4_table
      use set_bin_id_4_destination
      use order_dest_table_by_type
!
!
      if (iflag_debug.eq.1) write(*,*)  'set_sph_grid_4_bin'
      call set_all_block_points_4_itp                                   &
     &   (numnod, numele, xx, nprocs_2nd, origin_mesh)
!      call check_block_points_4_itp(50+my_rank, nprocs_2nd)
!
      if (iflag_debug.eq.1) write(*,*)  'set_sph_grid_4_bin'
      call set_sph_grid_4_bin
!
!  -------------------------------
!
      if (iflag_debug.eq.1)                                             &
     &     write(*,*)  'allocate_interpolate_table'
!
      ntot_table_dest = internal_node
      call allocate_itp_num_dest(nprocs_2nd)
      call allocate_itp_table_dest
      call allocate_itp_coef_dest
      call allocate_itp_work_dest(nprocs_2nd)
      call allocate_work_const_itp_tbl(numnod)
!
      if (iflag_debug.eq.1)                                             &
     &     write(*,*)  's_set_bin_id_4_destination'
      call s_set_bin_id_4_destination(id_search_area)
!
!      call check_search_ID(12)
!
      end subroutine s_set_serach_data_4_dest
!
! ----------------------------------------------------------------------
!
      end module set_serach_data_4_dest
