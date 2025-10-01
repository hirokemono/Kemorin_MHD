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
      use t_interpolate_table
      use t_interpolate_coefs_dest
      use t_ctl_data_gen_table
      use t_work_const_itp_table
      use t_search_block_4_itp
      use t_ctl_params_4_gen_table
      use set_2nd_geometry_4_table
!
      implicit none
!
      integer, save :: nprocs_2nd
      type(ctl_data_gen_table), save :: gtbl_ctl1
      type(ctl_params_4_gen_table), save :: gen_itp_p1
!
      type(mesh_data), save :: org_femmesh
      type(next_nod_ele_table), save :: next_tbl_i
!
      type(interpolate_table), save :: itp_ele
      type(interpolate_coefs_dest), save :: itp_e_coef
!
      type(para_block_4_interpolate) :: itp_blks1
      type(work_const_itp_table), save :: cst_itp_wke
!
      type(parallel_mesh_for_itp),save  :: itp_org
!
      character(len=kchara), parameter, private :: work_header = 'work'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use t_shape_functions
!
      use nod_and_ele_derived_info
      use const_mesh_information
      use set_element_id_4_node
      use element_posi_2_nodal_array
      use set_2nd_geometry_4_table
      use const_jacobians_3d
      use mpi_load_mesh_data
      use read_ctl_data_gen_table
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_gen_itp_table'
      call read_control_4_gen_itp_table(gtbl_ctl1)
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_gen_table'
      call set_ctl_params_4_gen_table                                   &
     &   (gtbl_ctl1, gen_itp_p1, itp_blks1, nprocs_2nd)
      
      call dealloc_ctl_data_gen_table(gtbl_ctl1)
!
!  --  read geometry
!
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(gen_itp_p1%itp_dest_mesh_file,                &
     &    nprocs, org_femmesh)
!
      if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos                                        &
     &   (org_femmesh%mesh%node, org_femmesh%mesh%ele)
!
!     ----- construct mesh informations for original mesh
!
      if (iflag_debug.eq.1)                                             &
     &  write(*,*) 'set_2nd_geometry_type_itp_tbl', nprocs_2nd
      call set_2nd_geometry_type_itp_tbl                                &
     &   (gen_itp_p1%itp_org_mesh_file, nprocs_2nd, itp_org)
!
!  -------------------------------
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (org_femmesh%mesh, next_tbl_i%neib_ele, next_tbl_i%neib_nod)
!
!  -------------------------------
!
      call s_element_posi_2_nodal_array                                 &
     &   (org_femmesh%mesh%ele, org_femmesh%mesh%node)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 's_set_serach_data_4_dest'
      call s_set_serach_data_4_dest(itp_org, org_femmesh%mesh%node,     &
     &    itp_ele%tbl_dest, itp_e_coef, cst_itp_wke, itp_blks1)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use calypso_mpi
      use m_interpolate_table_IO
!
      use construct_interpolate_table
      use const_interpolate_4_org
      use const_rev_ele_itp_table
      use order_dest_table_by_domain
      use order_dest_table_by_type
      use itp_work_file_IO_select
      use copy_interpolate_types
      use delete_data_files
!
      integer(kind = kint) :: ierr_missing, num_pe
      type(field_IO_params) :: tmp_tbl_IO
      type(interpolate_table_dest) :: IO_itp_dest
!
!
      if (iflag_debug.eq.1) write(*,*) 's_construct_interpolate_table'
      call s_construct_interpolate_table(itp_org, gen_itp_p1,           &
     &    org_femmesh%mesh%node%numnod,                                 &
     &    org_femmesh%mesh%node%internal_node,                          &
     &    org_femmesh%mesh%node%xx, next_tbl_i%neib_nod,                &
     &    itp_blks1%org_blocks, itp_e_coef,                             &
     &    cst_itp_wke%iflag_org_domain, ierr_missing)
!
!   ordering destination table by domain
!
      if (iflag_debug.eq.1) write(*,*) 's_order_dest_table_by_domain'
      call s_order_dest_table_by_domain(org_femmesh%mesh%node,          &
     &    nprocs_2nd, cst_itp_wke%iflag_org_domain, ierr_missing,       &
     &    itp_ele%tbl_dest, itp_e_coef, cst_itp_wke%orderd)
!
!      call check_table_in_org_2(13, itp_ele%tbl_dest, itp_e_coef)
!
!   ordering destination table by interpolation type
!
      if (iflag_debug.eq.1) write(*,*) 's_order_dest_table_by_type'
      call s_order_dest_table_by_type                                   &
     &   (org_femmesh%mesh%node, org_femmesh%mesh%ele,                  &
     &    itp_ele%tbl_dest, itp_e_coef, cst_itp_wke%orderd)
!
      call copy_itp_coefs_dest(my_rank,                                 &
     &    itp_ele%tbl_dest, itp_e_coef, IO_itp_dest, IO_itp_c_dest)
!
      tmp_tbl_IO%file_prefix = work_header
      call sel_write_itp_coefs_dest                                     &
     &   (my_rank, tmp_tbl_IO, IO_itp_dest, IO_itp_c_dest)
      call dealloc_itp_dest_after_write(IO_itp_dest, IO_itp_c_dest)
!
!   construct table for originate domain
!
      if(gen_itp_p1%iflag_reverse_itp_tbl .eq. 1) then
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'const_rev_ele_interpolate_table'
        call const_rev_ele_interpolate_table                            &
     &     (nprocs_2nd, gen_itp_p1, cst_itp_wke)
      else
        if (iflag_debug.eq.1)                                           &
     &     write(*,*) 'const_interpolate_table_4_orgin'
        call const_interpolate_table_4_orgin                            &
     &     (nprocs_2nd, gen_itp_p1, cst_itp_wke)
      end if
!
!
      if (my_rank .eq. 0) then
        num_pe = int(nprocs, KIND(num_pe))
        call delete_parallel_files(ione, num_pe, work_header)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_ele_table
