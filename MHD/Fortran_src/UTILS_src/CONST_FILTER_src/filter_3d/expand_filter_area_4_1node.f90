!expand_filter_area_4_1node.f90
!      module expand_filter_area_4_1node
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine init_4_cal_fileters
!      subroutine finalize_4_cal_fileters
!      subroutine resize_matrix_size_gen_filter
!      subroutine s_expand_filter_area_4_1node(inod)
!      subroutine copy_next_nod_ele_4_each(inod, numnod)
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_4_cal_fileters
!
      use m_geometry_data
      use m_reference_moments
      use m_matrix_4_filter
      use m_filter_file_names
      use m_field_file_format
      use m_crs_matrix_4_filter
      use fem_const_filter_matrix
      use add_nodes_elems_4_each_nod
      use ordering_by_filtering_size
      use const_RHS_assemble_list
      use delete_small_weighting
!
!
      if (inod_end_filter .eq. -1) then
        inod_end_filter = node1%internal_node
      end if
      nnod_filetering = inod_end_filter - inod_start_filter + 1
!
      call allocate_nod_ele_near_1nod(node1%numnod, ele1%numele)
      call allocate_nod_ele_1nod_tmp(node1%numnod, ele1%numele)
      call allocate_wk_exp_ele_nod_each(node1%numnod, ele1%numele)
!
      max_mat_size =      0
      nmax_num_ele_1nod = 0
      call allocate_mat_num_weight(node1%numnod)
      call allocate_matrix_4_filter
      call allocate_sk_filter
!
      nmax_crs = max_mat_size
      imax_l = nmax_crs * (nmax_crs - 1) / 2
      imax_u = nmax_crs * (nmax_crs - 1) / 2
      call allocate_array_4_crs_stack
      call allocate_array_4_crs_item
!
      if (iflag_ordering_list .gt. 0) then
        call allocate_dist_ratio(node1%numnod)
      end if
!
      call allocate_tmp_4_filter_sort(node1%numnod)
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        write(filter_coef_code,'(a)') '!'
        write(filter_coef_code,'(a)') '! filter coefficients'
        write(filter_coef_code,'(a)') '!'
      end if
!
!  ---------------------------------------------------
!       set belonged node and element for each node
!  ---------------------------------------------------
!
       if(iflag_debug.eq.1) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod
!
      end subroutine init_4_cal_fileters
!
! -----------------------------------------------------------------------
!
      subroutine init_4_cal_fluid_fileters
!
      use m_filter_file_names
      use m_field_file_format
      use m_next_node_id_4_node
      use set_element_list_4_filter
!
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        write(filter_coef_code,'(a)') '!'
        write(filter_coef_code,'(a)') '! filter coefficients for fluid'
        write(filter_coef_code,'(a)') '!'
      end if
!
      call set_ele_id_4_filter_grp
!
      call const_next_nod_id_4_node
!
!
      end subroutine init_4_cal_fluid_fileters
!
! -----------------------------------------------------------------------
!
      subroutine finalize_4_cal_fileters
!
      use m_filter_file_names
      use m_filter_elength
      use m_matrix_4_filter
      use m_reference_moments
      use m_crs_matrix_4_filter
      use m_element_id_4_node
      use m_next_node_id_4_node
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
      subroutine resize_matrix_size_gen_filter
!
      use m_next_node_id_4_node
      use m_reference_moments
      use m_matrix_4_filter
      use m_crs_matrix_4_filter
      use fem_const_filter_matrix
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
        call allocate_sk_filter
      end if
!
      end subroutine resize_matrix_size_gen_filter
!
! -----------------------------------------------------------------------
!
      subroutine s_expand_filter_area_4_1node(inod)
!
      use m_geometry_data
      use m_element_id_4_node
      use add_nodes_elems_4_each_nod
      use ordering_by_filtering_size
!
      integer(kind = kint) :: inod
!
!
      nnod_near_1nod_filter = nnod_near_1nod_weight
      nele_near_1nod_filter = nele_near_1nod_weight
!
      call expand_near_ele_4_each_nod                                   &
     &   (node1%numnod, ele1%numele, ele_4_nod1%ntot,                   &
     &    ele_4_nod1%istack_4_node, ele_4_nod1%iele_4_node,             &
     &    nnod_near_1nod_filter, inod_near_1nod_weight,                 &
     &    nele_near_1nod_filter, nele_near_1nod_weight,                 &
     &    iele_near_1nod_weight)
!
      call add_nod_4_grp_each_nod                                       &
     &   (node1%numnod, ele1%numele, ele1%nnod_4_ele, ie,               &
     &    nele_near_1nod_weight, iele_near_1nod_weight,                 &
     &    nnod_near_1nod_filter, nnod_near_1nod_weight,                 &
     &    inod_near_1nod_weight, iweight_1nod_weight,                   &
     &    idist_from_center_1nod)
!
      if     (iflag_ordering_list .eq. 0) then
        call sort_added_nod_4_each_nod(node1%numnod,                    &
     &      nnod_near_1nod_filter, nnod_near_1nod_weight,               &
     &      inod_near_1nod_weight, iweight_1nod_weight)
      else if(iflag_ordering_list .eq. 1) then
        call filter_ordering_by_distance(inod)
      else if(iflag_ordering_list .eq. 2) then
        call filter_ordering_by_dist_ratio(inod)
      end if
!
      end subroutine s_expand_filter_area_4_1node
!
! -----------------------------------------------------------------------
!
      subroutine copy_next_nod_ele_4_each(inod, numnod)
!
      use m_element_id_4_node
      use m_next_node_id_4_node
!
      integer(kind = kint), intent(in) :: inod, numnod
      integer(kind = kint) :: inum, jnum
!
      nele_near_1nod_filter = ele_4_nod1%nele_4_node(inod)
      nele_near_1nod_weight = ele_4_nod1%nele_4_node(inod)
      do inum = 1, nele_near_1nod_weight
        jnum = ele_4_nod1%istack_4_node(inod-1) + inum
        iele_near_1nod_weight(inum) = ele_4_nod1%iele_4_node(jnum)
      end do
!
      nnod_near_1nod_filter = neib_nod1%nnod_next(inod)
      nnod_near_1nod_weight = neib_nod1%nnod_next(inod)
      idist_from_center_1nod(inum) = 1
      do inum = 1, nnod_near_1nod_weight
        jnum = neib_nod1%istack_next(inod-1) + inum
        inod_near_1nod_weight(inum) = neib_nod1%inod_next(jnum)
        iweight_1nod_weight(inum) =   neib_nod1%iweight_next(inum)
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
