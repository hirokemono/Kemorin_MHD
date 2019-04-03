!expand_filter_area_4_1node.f90
!      module expand_filter_area_4_1node
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine init_4_cal_fileters                                  &
!!     &         (mesh, gfil_p, ele_4_nod, neib_nod, fil_coef, tmp_coef,&
!!     &          fil_tbl_crs, fil_mat_crs, fil_mat)
!!        type(mesh_geometry),       intent(in) :: mesh
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!!        type(element_around_node), intent(inout) :: ele_4_nod
!!        type(next_nod_id_4_nod),   intent(inout) :: neib_nod
!!        type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
!!        type(CRS_matrix_connect), intent(inout) :: fil_tbl_crs
!!        type(CRS_matrix), intent(inout) :: fil_mat_crs
!!        type(matrix_4_filter), intent(inout) :: fil_mat
!!      subroutine init_4_cal_fluid_fileters                            &
!!     &         (mesh, fil_elist, ele_4_nod, neib_nod)
!!        type(mesh_geometry),       intent(in) :: mesh
!!        type(element_list_4_filter), intent(in) :: fil_elist
!!
!!      subroutine finalize_4_cal_fileters(iflag_ordering_list,         &
!!     &         (fil_coef, fil_tbl_crs, fil_mat_crs, fil_mat, ref_m)
!!      subroutine resize_matrix_size_gen_filter(nnod_4_ele,            &
!!     &          fil_coef, fil_tbl_crs, fil_mat_crs, fil_mat)
!!        type(each_filter_coef), intent(in) :: fil_coef
!!        type(CRS_matrix_connect), intent(inout) :: fil_tbl_crs
!!        type(CRS_matrix), intent(inout) :: fil_mat_crs
!!        type(matrix_4_filter), intent(inout) :: fil_mat
!!
!!      subroutine s_expand_filter_area_4_1node(inod, gfil_p, node, ele,&
!!     &          ele_4_nod, FEM_elen, fil_coef)
!!      subroutine copy_next_nod_ele_4_each                             &
!!     &         (inod, numnod, ele_4_nod, neib_nod, fil_coef)
!!        type(ctl_params_4_gen_filter), intent(in) :: gfil_p
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(each_filter_coef), intent(inout) :: fil_coef
!
      module expand_filter_area_4_1node
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_params_4_gen_filter
      use t_filter_coefs
      use t_crs_connect
      use t_crs_matrix
      use t_matrix_4_filter
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
      subroutine init_4_cal_fileters                                    &
     &         (mesh, gfil_p, ele_4_nod, neib_nod, fil_coef, tmp_coef,  &
     &          fil_tbl_crs, fil_mat_crs, fil_mat)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use set_table_4_RHS_assemble
!
      type(mesh_geometry),       intent(in) :: mesh
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(element_around_node), intent(inout) :: ele_4_nod
      type(next_nod_id_4_nod),   intent(inout) :: neib_nod
!
      type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
      type(CRS_matrix_connect), intent(inout) :: fil_tbl_crs
      type(CRS_matrix), intent(inout) :: fil_mat_crs
      type(matrix_4_filter), intent(inout) :: fil_mat
!
!
      call allocate_work_4_fileters(mesh%node, mesh%ele, gfil_p,        &
     &    fil_coef, tmp_coef, fil_tbl_crs, fil_mat_crs, fil_mat)
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
      subroutine init_4_cal_fluid_fileters                              &
     &         (mesh, fil_elist, ele_4_nod, neib_nod)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_element_list_4_filter
!
      use m_filter_file_names
      use m_field_file_format
      use set_ele_id_4_node_type
!
      type(mesh_geometry),       intent(in) :: mesh
      type(element_list_4_filter), intent(in) :: fil_elist
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
      call set_grouped_ele_id_4_node                                    &
     &   (fil_elist%nele_4_filter, fil_elist%iele_4_filter,             &
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
      subroutine allocate_work_4_fileters(node, ele, gfil_p,            &
     &          fil_coef, tmp_coef, fil_tbl_crs, fil_mat_crs, fil_mat)
!
      use t_geometry_data
      use t_next_node_ele_4_node
      use m_filter_file_names
      use m_field_file_format
      use fem_const_filter_matrix
      use add_nodes_elems_4_each_nod
      use ordering_by_filtering_size
      use delete_small_weighting
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
      type(CRS_matrix_connect), intent(inout) :: fil_tbl_crs
      type(CRS_matrix), intent(inout) :: fil_mat_crs
      type(matrix_4_filter), intent(inout) :: fil_mat
!
!
      if(gfil_p%inod_end_filter .eq. -1) then
        gfil_p%inod_end_filter = node%internal_node
      end if
      nnod_filetering                                                   &
     &     = gfil_p%inod_end_filter - gfil_p%inod_start_filter + 1
!
      call alloc_each_filter_coef(node%numnod, fil_coef)
      call alloc_each_filter_coef(node%numnod, tmp_coef)
      call alloc_each_ele_filter_coef(ele%numele, fil_coef)
      call alloc_each_ele_filter_coef(ele%numele, tmp_coef)
      call allocate_wk_exp_ele_nod_each(node%numnod, ele%numele)
!
      fil_mat%max_mat_size =      0
      nmax_num_ele_1nod = 0
      call allocate_mat_num_weight(node%numnod)
      call alloc_matrix_4_filter(fil_mat)
      call allocate_sk_filter(ele%nnod_4_ele)
!
      call alloc_crs_stack(fil_mat%max_mat_size, fil_tbl_crs)
!
      fil_tbl_crs%ntot_l = fil_mat%max_mat_size                         &
     &                    * (fil_mat%max_mat_size - 1) / 2
      fil_tbl_crs%ntot_u = fil_mat%max_mat_size                         &
     &                    * (fil_mat%max_mat_size - 1) / 2
      call alloc_crs_connect(fil_tbl_crs)
      call alloc_crs_mat_data(fil_tbl_crs, fil_mat_crs)
!
      if(gfil_p%iflag_ordering_list .gt. 0) then
        call allocate_dist_ratio(node%numnod)
      end if
!
      call allocate_tmp_4_filter_sort(fil_coef%nnod)
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
      subroutine finalize_4_cal_fileters(iflag_ordering_list,           &
     &          fil_coef, fil_tbl_crs, fil_mat_crs, fil_mat, ref_m)
!
      use t_reference_moments
      use m_filter_file_names
      use add_nodes_elems_4_each_nod
      use ordering_by_filtering_size
      use fem_const_filter_matrix
      use delete_small_weighting
!
      integer(kind = kint), intent(in) :: iflag_ordering_list
      type(each_filter_coef), intent(inout) :: fil_coef
      type(CRS_matrix_connect), intent(inout) :: fil_tbl_crs
      type(CRS_matrix), intent(inout) :: fil_mat_crs
      type(reference_moments), intent(inout) :: ref_m
      type(matrix_4_filter), intent(inout) :: fil_mat
!
!
      call deallocate_tmp_4_filter_sort
!
      if (iflag_ordering_list .gt. 0) call deallocate_dist_ratio
!
      call dealloc_crs_mat_data(fil_mat_crs)
      call dealloc_crs_connect(fil_tbl_crs)
!
      call dealloc_reference_moments(ref_m)
      call dealloc_coef_4_filter_moms(ref_m)
!
      call dealloc_matrix_4_filter(fil_mat)
      call deallocate_mat_num_weight
      call deallocate_sk_filter
!
      call deallocate_wk_exp_ele_nod_each
!
      call dealloc_each_ele_filter_coef(fil_coef)
      call dealloc_each_filter_coef(fil_coef)
!
      end subroutine finalize_4_cal_fileters
!
! -----------------------------------------------------------------------
!
      subroutine resize_matrix_size_gen_filter(nnod_4_ele,              &
     &          fil_coef, fil_tbl_crs, fil_mat_crs, fil_mat)
!
      use fem_const_filter_matrix
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      type(each_filter_coef), intent(in) :: fil_coef
!
      type(CRS_matrix_connect), intent(inout) :: fil_tbl_crs
      type(CRS_matrix), intent(inout) :: fil_mat_crs
      type(matrix_4_filter), intent(inout) :: fil_mat
!
!
      if (fil_mat%max_mat_size .lt. fil_coef%nnod_4_1nod_w) then
!
        write(*,*) 'Raise order of reference moments'
        write(*,*) 'current matrix size: ', fil_mat%max_mat_size
        write(*,*) 'num. of neib for weight: ', fil_coef%nnod_4_1nod_w
!
        fil_mat%max_mat_size = fil_coef%nnod_4_1nod_w
        call dealloc_matrix_4_filter(fil_mat)
        call alloc_matrix_4_filter(fil_mat)
!
      call dealloc_crs_mat_data(fil_mat_crs)
      call dealloc_crs_connect(fil_tbl_crs)
!
        call alloc_crs_stack(fil_mat%max_mat_size, fil_tbl_crs)
!
        fil_tbl_crs%ntot_l = fil_mat%max_mat_size                       &
     &                      * (fil_mat%max_mat_size - 1) / 2
        fil_tbl_crs%ntot_u = fil_mat%max_mat_size                       &
     &                      * (fil_mat%max_mat_size - 1) / 2
        call alloc_crs_connect(fil_tbl_crs)
        call alloc_crs_mat_data(fil_tbl_crs, fil_mat_crs)
      end if
!
      if (nmax_num_ele_1nod .lt. fil_coef%nele_4_1nod_w) then
!
        write(*,*) 'Raise work area for integration'
        write(*,*) 'current matrix size: ', nmax_num_ele_1nod
        write(*,*) 'num_ele for integration: ', fil_coef%nele_4_1nod_w
!
        nmax_num_ele_1nod = fil_coef%nele_4_1nod_w
        call deallocate_sk_filter
        call allocate_sk_filter(nnod_4_ele)
      end if
!
      end subroutine resize_matrix_size_gen_filter
!
! -----------------------------------------------------------------------
!
      subroutine s_expand_filter_area_4_1node(inod, gfil_p, node, ele,  &
     &          ele_4_nod, FEM_elen, fil_coef)
!
      use t_filter_elength
      use t_geometry_data
      use t_next_node_ele_4_node
      use add_nodes_elems_4_each_nod
      use ordering_by_filtering_size
!
      type(ctl_params_4_gen_filter), intent(in) :: gfil_p
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: ele_4_nod
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      integer(kind = kint), intent(in) :: inod
!
      type(each_filter_coef), intent(inout) :: fil_coef
!
!
      fil_coef%nnod_4_1nod_f = fil_coef%nnod_4_1nod_w
      fil_coef%nele_4_1nod_f = fil_coef%nele_4_1nod_w
!
      call expand_near_ele_4_each_nod                                   &
     &   (node%numnod, ele%numele, ele_4_nod%ntot,                      &
     &    ele_4_nod%istack_4_node, ele_4_nod%iele_4_node,               &
     &    fil_coef%nnod_4_1nod_f, fil_coef%inod_4_1nod_w,               &
     &    fil_coef%nele_4_1nod_f, fil_coef%nele_4_1nod_w,               &
     &    fil_coef%iele_4_1nod_w)
!
      call add_nod_4_grp_each_nod                                       &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    fil_coef%nele_4_1nod_w, fil_coef%iele_4_1nod_w,               &
     &    fil_coef%nnod_4_1nod_f, fil_coef%nnod_4_1nod_w,               &
     &    fil_coef%inod_4_1nod_w, fil_coef%iweight_for_1nod,            &
     &    fil_coef%idist_from_1nod)
!
      if     (gfil_p%iflag_ordering_list .eq. 0) then
        call sort_added_nod_4_each_nod(node%numnod,                     &
     &      fil_coef%nnod_4_1nod_f, fil_coef%nnod_4_1nod_w,             &
     &      fil_coef%inod_4_1nod_w, fil_coef%iweight_for_1nod)
      else if(gfil_p%iflag_ordering_list .eq. 1) then
        call filter_ordering_by_distance(node, inod, fil_coef)
      else if(gfil_p%iflag_ordering_list .eq. 2) then
        call filter_ordering_by_dist_ratio                              &
     &     (node, FEM_elen, inod, fil_coef)
      end if
!
      end subroutine s_expand_filter_area_4_1node
!
! -----------------------------------------------------------------------
!
      subroutine copy_next_nod_ele_4_each                               &
     &         (inod, numnod, ele_4_nod, neib_nod, fil_coef)
!
      use t_next_node_ele_4_node
!
      type(element_around_node), intent(in) :: ele_4_nod
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      integer(kind = kint), intent(in) :: inod, numnod
!
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint) :: inum, jnum
!
!
      fil_coef%nele_4_1nod_f = ele_4_nod%nele_4_node(inod)
      fil_coef%nele_4_1nod_w = ele_4_nod%nele_4_node(inod)
      do inum = 1, fil_coef%nele_4_1nod_w
        jnum = ele_4_nod%istack_4_node(inod-1) + inum
        fil_coef%iele_4_1nod_w(inum) = ele_4_nod%iele_4_node(jnum)
      end do
!
      fil_coef%nnod_4_1nod_f = neib_nod%nnod_next(inod)
      fil_coef%nnod_4_1nod_w = neib_nod%nnod_next(inod)
      fil_coef%idist_from_1nod(inum) = 1
      do inum = 1, fil_coef%nnod_4_1nod_w
        jnum = neib_nod%istack_next(inod-1) + inum
        fil_coef%inod_4_1nod_w(inum) =    neib_nod%inod_next(jnum)
        fil_coef%iweight_for_1nod(inum) = neib_nod%iweight_next(inum)
        fil_coef%idist_from_1nod(inum) = 1
      end do
      do inum = (fil_coef%nnod_4_1nod_w+1), numnod
        fil_coef%idist_from_1nod(inum) = -1
      end do
      fil_coef%idist_from_1nod(1) = 0
!
      end subroutine copy_next_nod_ele_4_each
!
! -----------------------------------------------------------------------
!
      end module expand_filter_area_4_1node
