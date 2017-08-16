!expand_filter_area_4_1node.f90
!      module expand_filter_area_4_1node
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine init_4_cal_fileters(mesh, ele_4_nod, neib_nod)
!!      subroutine init_4_cal_fluid_fileters(mesh, ele_4_nod, neib_nod)
!!        type(mesh_geometry),       intent(in) :: mesh
!!      subroutine finalize_4_cal_fileters
!!      subroutine resize_matrix_size_gen_filter(nnod_4_ele)
!!      subroutine s_expand_filter_area_4_1node                         &
!!     &         (inod, node, ele, ele_4_nod, FEM_elen)
!!      subroutine copy_next_nod_ele_4_each                             &
!!     &         (inod, numnod, ele_4_nod, neib_nod)
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!
      module expand_filter_area_4_1node
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
!
      implicit none
!
      integer(kind = kint), private :: nnod_filetering
!
      private :: allocate_work_4_fileters
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_4_cal_fileters(mesh, ele_4_nod, neib_nod)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use set_table_4_RHS_assemble
!
      type(mesh_geometry),       intent(in) :: mesh
      type(element_around_node), intent(inout) :: ele_4_nod
      type(next_nod_id_4_nod),   intent(inout) :: neib_nod
!
!
      call allocate_work_4_fileters(mesh%node, mesh%ele)
!
!  ---------------------------------------------------
!       set belonged node and element for each node
!  ---------------------------------------------------
!
       if(iflag_debug.eq.1) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod(mesh, ele_4_nod, neib_nod)
!
      end subroutine init_4_cal_fileters
!
! -----------------------------------------------------------------------
!
      subroutine init_4_cal_fluid_fileters(mesh, ele_4_nod, neib_nod)
!
      use t_mesh_data
      use t_next_node_ele_4_node
!
      use m_filter_file_names
      use m_field_file_format
      use set_ele_id_4_node_type
      use set_element_list_4_filter
!
      type(mesh_geometry),       intent(in) :: mesh
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
      type(element_around_node), intent(inout) :: ele_4_nod
!
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        write(filter_coef_code,'(a)') '!'
        write(filter_coef_code,'(a)') '! filter coefficients for fluid'
        write(filter_coef_code,'(a)') '!'
      end if
!
      call set_grouped_ele_id_4_node(nele_4_filter, iele_4_filter,      &
     &    mesh%node, mesh%ele, ele_4_nod)
!
      call const_next_nod_id_4_node                                     &
     &   (mesh%node, mesh%ele, ele_4_nod, neib_nod)
!
      end subroutine init_4_cal_fluid_fileters
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_work_4_fileters(node, ele)
!
      use t_geometry_data
      use t_next_node_ele_4_node
      use m_reference_moments
      use m_matrix_4_filter
      use m_filter_file_names
      use m_field_file_format
      use m_crs_matrix_4_filter
      use fem_const_filter_matrix
      use add_nodes_elems_4_each_nod
      use ordering_by_filtering_size
      use delete_small_weighting
!
      type(node_data),           intent(in) :: node
      type(element_data),        intent(in) :: ele
!
!
      if (inod_end_filter .eq. -1) then
        inod_end_filter = node%internal_node
      end if
      nnod_filetering = inod_end_filter - inod_start_filter + 1
!
      call allocate_nod_ele_near_1nod(node%numnod, ele%numele)
      call allocate_nod_ele_1nod_tmp(node%numnod, ele%numele)
      call allocate_wk_exp_ele_nod_each(node%numnod, ele%numele)
!
      max_mat_size =      0
      nmax_num_ele_1nod = 0
      call allocate_mat_num_weight(node%numnod)
      call allocate_matrix_4_filter
      call allocate_sk_filter(ele%nnod_4_ele)
!
      nmax_crs = max_mat_size
      imax_l = nmax_crs * (nmax_crs - 1) / 2
      imax_u = nmax_crs * (nmax_crs - 1) / 2
      call allocate_array_4_crs_stack
      call allocate_array_4_crs_item
!
      if (iflag_ordering_list .gt. 0) then
        call allocate_dist_ratio(node%numnod)
      end if
!
      call allocate_tmp_4_filter_sort(node%numnod)
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        write(filter_coef_code,'(a)') '!'
        write(filter_coef_code,'(a)') '! filter coefficients'
        write(filter_coef_code,'(a)') '!'
      end if
!
      end subroutine allocate_work_4_fileters
!
! -----------------------------------------------------------------------
!
      subroutine finalize_4_cal_fileters
!
      use m_filter_file_names
      use m_matrix_4_filter
      use m_reference_moments
      use m_crs_matrix_4_filter
      use add_nodes_elems_4_each_nod
      use ordering_by_filtering_size
      use fem_const_filter_matrix
      use delete_small_weighting
!
!

      call deallocate_tmp_4_filter_sort
!

      if (iflag_ordering_list .gt. 0) call deallocate_dist_ratio
!
      call deallocate_array_4_crs_item
      call deallocate_reference_moments
      call deallocate_matrix_4_filter
      call deallocate_mat_num_weight
      call deallocate_sk_filter
!
      call deallocate_wk_exp_ele_nod_each
!
      call deallocate_nod_ele_near_1nod
!
      end subroutine finalize_4_cal_fileters
!
! -----------------------------------------------------------------------
!
      subroutine resize_matrix_size_gen_filter(nnod_4_ele)
!
      use m_reference_moments
      use m_matrix_4_filter
      use m_crs_matrix_4_filter
      use fem_const_filter_matrix
!
      integer(kind = kint), intent(in) :: nnod_4_ele
!
!
      if (max_mat_size .lt. nnod_near_1nod_weight) then
!
        write(*,*) 'Raise order of reference moments'
        write(*,*) 'current matrix size: ', max_mat_size
        write(*,*) 'num. of neib for weight: ', nnod_near_1nod_weight
!
        max_mat_size = nnod_near_1nod_weight
        call deallocate_matrix_4_filter
        call allocate_matrix_4_filter
        nmax_crs = max_mat_size
        imax_l = nmax_crs * (nmax_crs - 1) / 2
        imax_u = nmax_crs * (nmax_crs - 1) / 2
        call deallocate_array_4_crs_item
        call allocate_array_4_crs_stack
        call allocate_array_4_crs_item
      end if
!
      if (nmax_num_ele_1nod .lt. nele_near_1nod_weight) then
!
        write(*,*) 'Raise work area for integration'
        write(*,*) 'current matrix size: ', nmax_num_ele_1nod
        write(*,*) 'num_ele for integration: ', nele_near_1nod_weight
!
        nmax_num_ele_1nod = nele_near_1nod_weight
        call deallocate_sk_filter
        call allocate_sk_filter(nnod_4_ele)
      end if
!
      end subroutine resize_matrix_size_gen_filter
!
! -----------------------------------------------------------------------
!
      subroutine s_expand_filter_area_4_1node                           &
     &         (inod, node, ele, ele_4_nod, FEM_elen)
!
      use t_filter_elength
      use t_geometry_data
      use t_next_node_ele_4_node
      use add_nodes_elems_4_each_nod
      use ordering_by_filtering_size
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: ele_4_nod
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      integer(kind = kint), intent(in) :: inod
!
!
      nnod_near_1nod_filter = nnod_near_1nod_weight
      nele_near_1nod_filter = nele_near_1nod_weight
!
      call expand_near_ele_4_each_nod                                   &
     &   (node%numnod, ele%numele, ele_4_nod%ntot,                      &
     &    ele_4_nod%istack_4_node, ele_4_nod%iele_4_node,               &
     &    nnod_near_1nod_filter, inod_near_1nod_weight,                 &
     &    nele_near_1nod_filter, nele_near_1nod_weight,                 &
     &    iele_near_1nod_weight)
!
      call add_nod_4_grp_each_nod                                       &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    nele_near_1nod_weight, iele_near_1nod_weight,                 &
     &    nnod_near_1nod_filter, nnod_near_1nod_weight,                 &
     &    inod_near_1nod_weight, iweight_1nod_weight,                   &
     &    idist_from_center_1nod)
!
      if     (iflag_ordering_list .eq. 0) then
        call sort_added_nod_4_each_nod(node%numnod,                     &
     &      nnod_near_1nod_filter, nnod_near_1nod_weight,               &
     &      inod_near_1nod_weight, iweight_1nod_weight)
      else if(iflag_ordering_list .eq. 1) then
        call filter_ordering_by_distance(node, inod)
      else if(iflag_ordering_list .eq. 2) then
        call filter_ordering_by_dist_ratio(node, FEM_elen, inod)
      end if
!
      end subroutine s_expand_filter_area_4_1node
!
! -----------------------------------------------------------------------
!
      subroutine copy_next_nod_ele_4_each                               &
     &         (inod, numnod, ele_4_nod, neib_nod)
!
      use t_next_node_ele_4_node
!
      type(element_around_node), intent(in) :: ele_4_nod
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      integer(kind = kint), intent(in) :: inod, numnod
      integer(kind = kint) :: inum, jnum
!
      nele_near_1nod_filter = ele_4_nod%nele_4_node(inod)
      nele_near_1nod_weight = ele_4_nod%nele_4_node(inod)
      do inum = 1, nele_near_1nod_weight
        jnum = ele_4_nod%istack_4_node(inod-1) + inum
        iele_near_1nod_weight(inum) = ele_4_nod%iele_4_node(jnum)
      end do
!
      nnod_near_1nod_filter = neib_nod%nnod_next(inod)
      nnod_near_1nod_weight = neib_nod%nnod_next(inod)
      idist_from_center_1nod(inum) = 1
      do inum = 1, nnod_near_1nod_weight
        jnum = neib_nod%istack_next(inod-1) + inum
        inod_near_1nod_weight(inum) = neib_nod%inod_next(jnum)
        iweight_1nod_weight(inum) =   neib_nod%iweight_next(inum)
        idist_from_center_1nod(inum) = 1
      end do
      do inum = (nnod_near_1nod_weight+1), numnod
        idist_from_center_1nod(inum) = -1
      end do
      idist_from_center_1nod(1) = 0
!
      end subroutine copy_next_nod_ele_4_each
!
! -----------------------------------------------------------------------
!
      end module expand_filter_area_4_1node
