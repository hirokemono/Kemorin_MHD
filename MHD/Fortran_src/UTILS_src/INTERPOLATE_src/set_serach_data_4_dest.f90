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
      use m_machine_parameter
      use m_geometry_parameter
      use m_2nd_pallalel_vector
      use m_ctl_params_4_gen_table
      use m_sphere_bin_4_table
      use m_interpolate_table_dest
      use m_work_const_itp_table
!
      use set_bin_id_4_destination
!
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
      call allocate_work_const_itp_tbl(nprocs_2nd)
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
