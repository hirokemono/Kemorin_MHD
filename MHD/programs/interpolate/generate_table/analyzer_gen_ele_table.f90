!analyzer_gen_ele_table.f90
!      module analyzer_gen_ele_table
!..................................................
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_gen_ele_table
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use t_mesh_data
      use t_next_node_ele_4_node
!
      implicit none
!
      type(mesh_geometry), save :: newmesh
      type(mesh_groups), save ::   newgroup
!
      type(next_nod_ele_table), save :: next_tbl_i
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
!
      use m_geometry_data
      use m_jacobians
!
      use input_control_gen_table
      use const_mesh_types_info
      use set_table_type_RHS_assemble
      use set_serach_data_4_dest
      use element_posi_2_nodal_array
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
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos(node1, ele1)
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
      if (iflag_debug.eq.1) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (node1, ele1, next_tbl_i%neib_ele, next_tbl_i%neib_nod)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_jacobian_element'
      call set_max_int_point_by_etype
      call cal_jacobian_element
!
!  -------------------------------
!
      call s_element_posi_2_nodal_array(ele1, node1)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 's_set_serach_data_4_dest'
      call s_set_serach_data_4_dest(node1)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use calypso_mpi
      use m_ctl_params_4_gen_table
      use m_interpolate_coefs_dest
      use construct_interpolate_table
      use const_interpolate_4_org
      use const_rev_ele_itp_table
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
      call s_construct_interpolate_table                                &
     &   (next_tbl_i%neib_nod, newmesh, newgroup, ierr_missing)
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
      call copy_itp_table_dest_to_IO
!
      table_file_header = work_header
      call sel_write_itp_coefs_dest(my_rank)
!
!   construct table for originate domain
!
      if(iflag_reverse_itp_tbl .eq. 1) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'const_rev_ele_interpolate_table'
        call const_rev_ele_interpolate_table
      else
        if (iflag_debug.eq.1)                                          &
     &     write(*,*) 'const_interpolate_table_4_orgin'
        call const_interpolate_table_4_orgin
      end if
!
!
      if (my_rank .eq. 0) then
        call delete_parallel_files(ione, nprocs, work_header)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_ele_table
