!analyzer_gen_filter.f90
!      module analyzer_gen_filter
!..................................................
!
!      modified by H. Matsui on Mar., 2008 
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_gen_filter
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_ctl_params_4_gen_filter
      use t_filter_dxdxi
      use t_filter_moments
!
      implicit none
!
      type(dxdxi_data_type), save :: filter_dxi1
      type(dxidx_data_type), save :: dxidxs1
      type(gradient_filter_mom_type), save :: FEM_momenet1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use m_fem_gauss_int_coefs
      use m_filter_file_names
      use const_mesh_info
      use input_control_gen_filter
      use cal_1d_moments_4_fliter
!
      use m_geometry_data
      use m_group_data
      use m_jacobians
      use m_jacobians_4_surface
      use m_jacobians_4_edge
      use m_jacobian_sf_grp
      use set_element_data_4_IO
      use set_surface_data_4_IO
      use set_edge_data_4_IO
      use check_jacobians
      use int_volume_of_domain
      use int_element_length
      use set_surf_grp_vectors
      use check_surface_groups
      use set_normal_vectors
      use set_edge_vectors
      use m_read_mesh_data
      use set_element_list_4_filter
      use sum_normal_4_surf_group
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 'input_control_3d_commute'
      call input_control_3d_commute
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_1d_moments'
      call s_cal_1d_moments
!
      call s_set_element_list_4_filter(ele_grp1)
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
!  -------------------------------
!
      if (iflag_debug.eq.1 .and. my_rank.eq.0 )                         &
     &   write(*,*) 'alloc_vectors_surf_grp_type'
      call alloc_vectors_surf_grp_type                                  &
     &   (sf_grp1%num_grp, sf_grp1%num_item, sf_grp_v1)
!
      if (iflag_debug.eq.1 .and. my_rank.eq.0 )                         &
     &   write(*,*) 'pick_surface_group_geometry'
      call pick_surface_group_geometry                                  &
     &   (surf1, sf_grp1, sf_grp_tbl1, sf_grp_v1)
!
!  -------------------------------
!  -------------------------------
!
      if (iflag_debug.eq.1)  write(*,*)  'cal_jacobian_element'
      call maximum_integration_points(num_int_points)
      call cal_jacobian_element
!
      if (iflag_debug.eq.1)  write(*,*)  'cal_jacobian_surface'
      call cal_jacobian_surface
!
      if (iflag_debug.eq.1)  write(*,*)  'cal_jacobian_edge'
      call cal_jacobian_edge
!
!      call check_jacobians_trilinear(my_rank, ele1, jac_3d_l)
!
      if (iflag_debug.eq.1)  write(*,*)  'cal_jacobian_surf_grp'
      call cal_jacobian_surf_grp(sf_grp1)
!
!  -------------------------------
!
      if (iflag_debug.eq.1)  write(*,*)  's_int_whole_volume_only'
      call s_int_whole_volume_only
      if (my_rank.eq.0) write(*,*)  'Volume of Domain: ', ele1%volume
!
      if (iflag_debug.eq.1)  write(*,*)  'int_element_length_1st'
      FEM1_elen%nnod_filter_mom = node1%numnod
      FEM1_elen%nele_filter_mom = ele1%numele
      FEM_momenet1%num_filter_moms = 2
      call alloc_jacobians_ele(FEM1_elen%nele_filter_mom, filter_dxi1)
      call alloc_elen_ele_type                                          &
     &   (FEM1_elen%nele_filter_mom, FEM1_elen%elen_ele)
!
      call s_int_element_length(FEM1_elen%nele_filter_mom,              &
     &    filter_dxi1%dxi_ele, FEM1_elen%elen_ele%moms)
!
       end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_array_for_send_recv
      use m_geometry_data
      use m_matrix_4_filter
      use m_crs_matrix_4_filter
      use m_filter_elength
      use m_filter_coefs
      use m_filter_coef_combained
      use m_comm_data_IO
      use m_filter_file_names
      use m_file_format_switch
!
      use cal_element_size
      use set_parallel_file_name
      use filter_moment_IO_select
      use filter_coef_IO
      use construct_filters
      use copy_nod_comm_tbl_4_filter
      use set_filter_geometry_4_IO
      use set_filter_comm_tbl_4_IO
      use filter_geometry_IO
      use check_num_fail_nod_commute
      use nodal_vector_send_recv
!
      use cal_filter_func_node
!
      character(len=kchara) :: file_name
!
!  ---------------------------------------------------
!       set element size for each node
!  ---------------------------------------------------
!
      if(iflag_debug.eq.1)  write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(ithree, node1%numnod)
!
      call init_send_recv
!
      if(iflag_debug.eq.1)  write(*,*) 's_cal_element_size'
      call s_cal_element_size(filter_dxi1, dxidxs1)
      call dealloc_jacobians_ele(filter_dxi1)
!
!  ---------------------------------------------------
!       output filter length and coefs
!  ---------------------------------------------------
!
      ifmt_filter_file = ifmt_filter_elen
      filter_file_head = filter_elen_head
      call sel_write_filter_elen_file(my_rank, FEM1_elen)
!
!  ---------------------------------------------------
!       copy node and communication table
!  ---------------------------------------------------
!
      if (iflag_tgt_filter_type .gt. -10)  then
        call copy_node_data_to_filter
        call copy_comm_table_to_filter
        call copy_filtering_geometry_to_IO
        call copy_filter_comm_tbl_to_IO(my_rank)
!
        call add_int_suffix(my_rank, filter_coef_head, file_name)
!
        if (ifmt_3d_filter .eq. id_binary_file_fmt) then
          open(filter_coef_code, file=file_name, form='unformatted')
          call write_filter_geometry_b(filter_coef_code)
        else
          open(filter_coef_code, file=file_name, form='formatted')
          call write_filter_geometry(filter_coef_code)
        end if
!
        num_failed_whole = 0
        num_failed_fluid = 0
!
        call select_const_filter(dxidxs1, FEM_momenet1)
        call dealloc_jacobians_node(filter_dxi1)
!
        close(filter_coef_code)
!
        call s_check_num_fail_nod_commute
!
!  ---------------------------------------------------
!       output filter moments
!  ---------------------------------------------------
!
        ifmt_filter_file = ifmt_filter_moms
        filter_file_head = filter_moms_head
        call sel_write_filter_moms_file                                 &
     &     (my_rank, FEM1_elen, FEM_momenet1)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_filter
