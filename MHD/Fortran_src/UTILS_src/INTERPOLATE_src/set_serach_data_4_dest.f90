!
!      module set_serach_data_4_dest
!..................................................
!
!      modified by H. Matsui on Aug., 2006 
!
!!      subroutine s_set_serach_data_4_dest                             &
!!     &         (dest_node, itp_dest, itp_coef)
!!        type(node_data), intent(in) :: dest_node
!!        type(interpolate_table_dest), intent(in) :: itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: itp_coef
!
      module set_serach_data_4_dest
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
      subroutine s_set_serach_data_4_dest                               &
     &         (dest_node, itp_dest, itp_coef)
!
      use t_geometry_data
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      use m_2nd_pallalel_vector
      use m_ctl_params_4_gen_table
      use m_work_const_itp_table
      use m_search_bolck_4_itp
!
      use set_2nd_geometry_4_table
      use order_dest_table_by_type
!
      type(node_data), intent(in) :: dest_node
      type(interpolate_table_dest), intent(inout) :: itp_dest
      type(interpolate_coefs_dest), intent(inout) :: itp_coef
!
!
      call set_all_block_points_4_itp                                   &
     &   (num_sph_grid, dest_node%numnod, dest_node%xx,                 &
     &   nprocs_2nd, origin_mesh)
!      call check_block_points_4_itp(50+my_rank, nprocs_2nd)
!
!  -------------------------------
!
      if (iflag_debug.eq.1)                                             &
     &     write(*,*)  'allocate_interpolate_table'
!
      itp_dest%ntot_table_dest = dest_node%internal_node
      call set_num_org_domain(nprocs_2nd, itp_dest)
      call alloc_itp_num_dest(itp_dest)
      call alloc_itp_table_dest(itp_dest)
      call alloc_itp_coef_dest(itp_dest, itp_coef)
      call allocate_itp_work_dest(nprocs_2nd)
      call allocate_work_const_itp_tbl(dest_node%numnod, itp_dest)
!
      end subroutine s_set_serach_data_4_dest
!
! ----------------------------------------------------------------------
!
      end module set_serach_data_4_dest
