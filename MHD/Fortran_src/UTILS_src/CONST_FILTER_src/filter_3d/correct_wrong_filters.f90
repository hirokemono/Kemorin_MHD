!
!      module correct_wrong_filters
!
!     Written by H. Matsui on Nov., 2008
!
!!      subroutine s_correct_wrong_filters                              &
!!     &         (id_org_filter, fixed_file_name, mesh, g_FEM, jac_3d,  &
!!     &          FEM_elen, ref_m, gfil_p, dxidxs, mom_nod, fil_coef,   &
!!     &          tmp_coef, whole_area, fluid_area, f_matrices,         &
!!     &          bbuf_org)
!!      subroutine correct_wrong_fluid_filters                          &
!!     &         (id_org_filter, fixed_file_name, gfil_p, mesh, g_FEM,  &
!!     &          jac_3d, FEM_elen, ref_m, fil_elist, dxidxs, mom_nod,  &
!!     &          fil_coef, tmp_coef, fluid_area, f_matrices, bbuf_org)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(reference_moments), intent(in) :: ref_m
!!        type(element_list_4_filter), intent(in) :: fil_elist
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!!        type(dxidx_data_type), intent(inout) :: dxidxs
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
!!        type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
!!        type(matrices_4_filter), intent(inout) :: f_matrices
!!        type(binary_IO_buffer), intent(inout) :: bbuf_org
!
      module correct_wrong_filters
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_next_node_ele_4_node
      use t_filter_elength
      use t_filter_dxdxi
      use t_filter_moments
      use t_reference_moments
      use t_filter_coefs
      use t_matrix_4_filter
      use t_element_list_4_filter
      use t_ctl_params_4_gen_filter
      use t_binary_IO_buffer
!
      use binary_IO
      use expand_filter_area_4_1node
      use copy_moments_2_matrix
      use cal_filter_func_each_node
      use cal_simple_filter_each_node
      use cal_3d_filter_4_each_node
      use cal_filter_moments_again
      use write_filters_4_each_node
!
      implicit none
!
      type(element_around_node), save :: ele_4_nod_f
      type(next_nod_id_4_nod), save :: neib_nod_f
!
      private :: ele_4_nod_f, neib_nod_f
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_correct_wrong_filters                                &
     &         (id_org_filter, fixed_file_name, mesh, g_FEM, jac_3d,    &
     &          FEM_elen, ref_m, gfil_p, dxidxs, mom_nod, fil_coef,     &
     &          tmp_coef, whole_area, fluid_area, f_matrices,           &
     &          bbuf_org)
!
      use set_simple_filters
!
      character(len = kchara), intent(in) :: fixed_file_name
      integer(kind = kint), intent(in) :: id_org_filter
!
      type(mesh_geometry), intent(in) :: mesh
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
      type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
      type(filter_area_flag), intent(inout) :: whole_area, fluid_area
      type(matrices_4_filter), intent(inout) :: f_matrices
!
      type(binary_IO_buffer), intent(inout) :: bbuf_org
!
      integer(kind = kint) :: inod, ierr2, ierr
!
!
      if(gfil_p%inod_end_filter .eq. -1) then
        gfil_p%inod_end_filter = mesh%node%internal_node
      end if
!
      call init_4_cal_fileters(mesh, gfil_p, ele_4_nod_f, neib_nod_f,   &
     &    fil_coef, tmp_coef, f_matrices%fil_tbl_crs,                   &
     &    f_matrices%fil_mat_crs, f_matrices%fil_mat)
!
      write(70+my_rank,*) ' Best condition for filter'
!
      do inod = gfil_p%inod_start_filter, gfil_p%inod_end_filter
        call read_each_filter_stack_coef                                &
     &     (id_org_filter, fil_coef, bbuf_org, ierr)
        if(ierr .gt. 0) return
!
        call cal_rms_filter_coefs(fil_coef, min_rms_weight, ierr2)
!
        if (min_rms_weight .gt. gfil_p%max_rms_weight_limit) then
          whole_area%iflag_make_filter(inod) = 1
          fluid_area%iflag_make_filter(inod) = 1
        end if
!
        if (ierr2 .eq. -1) then
          whole_area%iflag_make_filter(inod) = 1
          fluid_area%iflag_make_filter(inod) = 1
        end if
!
        if(whole_area%iflag_make_filter(inod) .eq. 0) then
          call write_each_filter_stack_coef                             &
     &       (fixed_file_name, inod, fil_coef, ierr)
!
          if(gfil_p%iflag_tgt_filter_type .ge. -iflag_gaussian          &
     &      .and. gfil_p%iflag_tgt_filter_type.le. -iflag_tophat) then
            call s_cal_filter_moments_again                             &
     &         (gfil_p, mesh%node, mesh%ele, g_FEM, jac_3d, FEM_elen,   &
     &          ref_m, inod, ele_4_nod_f, neib_nod_f, mom_nod,          &
     &          fil_coef, f_matrices%fil_tbl_crs,                       &
     &          f_matrices%fil_mat_crs, f_matrices%fil_mat)
          end if
        else
!
          if(gfil_p%iflag_tgt_filter_type .eq. -iflag_commutative) then
            call copy_each_filter_coefs(fil_coef, tmp_coef)
            call const_filter_func_nod_by_nod(fixed_file_name, inod,    &
     &          gfil_p, mesh%node, mesh%ele, g_FEM, jac_3d, FEM_elen,   &
     &          ref_m, ele_4_nod_f, neib_nod_f, fil_coef, tmp_coef,     &
     &          whole_area, f_matrices, ierr)
          else if(gfil_p%iflag_tgt_filter_type .ge. -iflag_gaussian     &
     &      .and. gfil_p%iflag_tgt_filter_type.le. -iflag_tophat) then
            call set_simple_filter_nod_by_nod(fixed_file_name,          &
     &          gfil_p, mesh%node, mesh%ele, g_FEM, jac_3d, FEM_elen,   &
     &          dxidxs%dx_nod, ref_m, inod, ele_4_nod_f, neib_nod_f,    &
     &          fil_coef, f_matrices%fil_tbl_crs,                       &
     &          f_matrices%fil_mat_crs, f_matrices%fil_mat)
          end if
!
          fil_coef%nnod_near_nod_w(inod) = fil_coef%nnod_4_1nod_w
          call cal_filter_moms_each_nod_type                            &
     &       (inod, ref_m, fil_coef, f_matrices%fil_mat, mom_nod)
        end if
!
      end do
!
      call dealloc_iele_belonged(ele_4_nod_f)
      call dealloc_inod_next_node(neib_nod_f)
!
      end subroutine s_correct_wrong_filters
!
! -----------------------------------------------------------------------
!
      subroutine correct_wrong_fluid_filters                            &
     &         (id_org_filter, fixed_file_name, gfil_p, mesh, g_FEM,    &
     &          jac_3d, FEM_elen, ref_m, fil_elist, dxidxs, mom_nod,    &
     &          fil_coef, tmp_coef, fluid_area, f_matrices, bbuf_org)
!
      character(len = kchara), intent(in) :: fixed_file_name
      integer(kind = kint), intent(in) :: id_org_filter
!
      type(ctl_params_4_gen_filter), intent(in) :: gfil_p
      type(mesh_geometry), intent(in) :: mesh
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
      type(element_list_4_filter), intent(in) :: fil_elist
!
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
      type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
      type(filter_area_flag), intent(inout) :: fluid_area
      type(matrices_4_filter), intent(inout) :: f_matrices
!
      type(binary_IO_buffer), intent(inout) :: bbuf_org
!
      integer(kind = kint) :: inod, ierr2, ierr
!
!
      call init_4_cal_fluid_fileters                                    &
     &   (mesh, fil_elist, ele_4_nod_f, neib_nod_f)
!
      write(70+my_rank,*) ' Best condition for fluid filter'
!
      do inod = gfil_p%inod_start_filter, gfil_p%inod_end_filter
        call read_each_filter_stack_coef                                &
     &     (id_org_filter, fil_coef, bbuf_org, ierr)
        if(ierr .gt. 0) return
!
        if ( fil_coef%nnod_4_1nod_w .gt. 0) then
          call cal_rms_filter_coefs(fil_coef, min_rms_weight, ierr2)
!
          if (min_rms_weight .gt. gfil_p%max_rms_weight_limit) then
            fluid_area%iflag_make_filter(inod) = 1
          end if
        end if
!
!     no correction
!
        if( fluid_area%iflag_make_filter(inod) .eq. 0) then
          if (fil_coef%nnod_4_1nod_w .eq. 0) then
            call write_each_no_filter_coef                              &
     &         (fixed_file_name, inod, fil_coef, ierr)
          else if (fil_coef%nnod_4_1nod_w .lt. 0) then
            fil_coef%nnod_4_1nod_w = -fil_coef%nnod_4_1nod_w
            call write_each_same_filter_coef                            &
     &         (fixed_file_name, inod, fil_coef, ierr)
          else
            call write_each_filter_stack_coef                           &
     &         (fixed_file_name, inod, fil_coef, ierr)
          end if
!
!       correct fluid filter
!
        else
!
          if(gfil_p%iflag_tgt_filter_type .eq. -iflag_commutative) then
            call copy_each_filter_coefs(fil_coef, tmp_coef)
            call const_fluid_filter_nod_by_nod(fixed_file_name, inod,   &
     &          gfil_p, mesh%node, mesh%ele, g_FEM, jac_3d, FEM_elen,   &
     &          ref_m, ele_4_nod_f, neib_nod_f, fil_coef, tmp_coef,     &
     &          fluid_area, f_matrices, ierr)
          else if(gfil_p%iflag_tgt_filter_type .ge. -iflag_gaussian     &
     &      .and. gfil_p%iflag_tgt_filter_type.le. -iflag_tophat) then
            call set_simple_fl_filter_nod_by_nod(fixed_file_name,       &
     &          gfil_p, mesh%node, mesh%ele, g_FEM, jac_3d, FEM_elen,   &
     &          dxidxs%dx_nod, ref_m, inod, ele_4_nod_f, neib_nod_f,    &
     &          mom_nod, fil_coef, f_matrices%fil_mat)
          end if
!
        end if
      end do
!
      call dealloc_iele_belonged(ele_4_nod_f)
      call dealloc_inod_next_node(neib_nod_f)
!
      end subroutine correct_wrong_fluid_filters
!
! -----------------------------------------------------------------------
!
      end module correct_wrong_filters
