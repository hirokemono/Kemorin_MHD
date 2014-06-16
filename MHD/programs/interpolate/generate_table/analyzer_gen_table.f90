!analyzer_gen_table.f90
!      module analyzer_gen_table
!..................................................
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_make_interpolate_table
!      subroutine analyze_make_interpolate_table
!
      module analyzer_gen_table
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use t_mesh_data
!
      implicit none
!
      type(mesh_geometry), save :: newmesh
      type(mesh_groups), save ::   newgroup
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_make_interpolate_table
!
!
      use m_2nd_pallalel_vector
      use cal_jacobian
!
      use input_control_gen_table
      use const_mesh_info
      use set_serach_data_4_dest
      use set_2nd_geometry_4_table
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_generate_table'
      call s_input_control_generate_table
!
!     ----- construct mesh informations for target mesh
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
!     ----- construct mesh informations for original mesh
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'set_2nd_geometry_type_itp_tbl', nprocs_2nd
      call set_2nd_geometry_type_itp_tbl(nprocs_2nd)
!
!  -------------------------------
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_element'
      call set_max_int_point_by_etype
      call cal_jacobian_element
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 's_set_serach_data_4_dest'
      call s_set_serach_data_4_dest
!
      end subroutine init_make_interpolate_table
!
! ----------------------------------------------------------------------
!
      subroutine analyze_make_interpolate_table
!
      use calypso_mpi
      use m_interpolate_coefs_dest
      use construct_interpolate_table
      use const_interpolate_4_org
      use order_dest_table_by_domain
      use order_dest_table_by_type
      use copy_interpolate_dest_IO
      use itp_table_IO_select_4_zlib
      use delete_data_files
!
      integer(kind = kint) :: ierr_missing
!
!
      if (iflag_debug.eq.1) write(*,*) 's_construct_interpolate_table'
      call s_construct_interpolate_table(newmesh, newgroup, ierr_missing)
!
!   ordering destination table by domain
!
      if (iflag_debug.eq.1) write(*,*) 's_order_dest_table_by_domain'
      call s_order_dest_table_by_domain(ierr_missing)
!
!      call check_table_in_org_2(13)
!
!   ordering destination table by interpolation type
!
      if (iflag_debug.eq.1) write(*,*) 's_order_dest_table_by_type'
      call s_order_dest_table_by_type
!
      if (iflag_debug.eq.1) write(*,*) 'copy_itp_table_dest_to_IO'
      call copy_itp_table_dest_to_IO
!
      table_file_header = work_header
      call sel_write_itp_coefs_dest(my_rank)
!
!   construct table for originate domain
!
      if (iflag_debug.eq.1)                                             &
     &   write(*,*) 'const_interpolate_table_4_orgin'
      call const_interpolate_table_4_orgin
!
!
      if (my_rank .eq. 0) then
        call delete_parallel_files(ione, nprocs, work_header)
      end if
!
      if (iflag_debug.eq.1)                                             &
     &       write(*,*) 'exit analyze_make_interpolate_table'
!
      end subroutine analyze_make_interpolate_table
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_table
