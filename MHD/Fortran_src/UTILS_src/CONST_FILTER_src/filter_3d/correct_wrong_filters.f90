!
!      module correct_wrong_filters
!
!     Written by H. Matsui on Nov., 2008
!
!!      subroutine s_correct_wrong_filters                              &
!!     &         (id_org_filter, fixed_file_name, mesh,                 &
!!     &          g_FEM, jac_3d, FEM_elen, dxidxs, mom_nod)
!!      subroutine correct_wrong_fluid_filters                          &
!!     &         (id_org_filter, fixed_file_name, mesh,                 &
!!     &          g_FEM, jac_3d, FEM_elen, dxidxs, mom_nod)
!!       type(mesh_geometry), intent(in) :: mesh
!!       type(jacobians_3d), intent(in) :: jac_3d
!!       type(gradient_model_data_type), intent(in) :: FEM_elen
!!       type(dxidx_data_type), intent(inout) :: dxidxs
!!       type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
!
      module correct_wrong_filters
!
      use m_precision
!
      use m_constants
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_crs_matrix_4_filter
!
      use t_mesh_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_next_node_ele_4_node
      use t_filter_elength
      use t_filter_dxdxi
      use t_filter_moments
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
     &         (id_org_filter, fixed_file_name, mesh,                   &
     &          g_FEM, jac_3d, FEM_elen, dxidxs, mom_nod)
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
!
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
      integer(kind = kint) :: inod, ierr2, ierr
!
!
      if (inod_end_filter .eq. -1) then
        inod_end_filter = mesh%node%internal_node
      end if
!
      call init_4_cal_fileters(mesh, ele_4_nod_f, neib_nod_f,           &
     &    fil_tbl_crs, fil_mat_crs)
!
      write(70+my_rank,*) ' Best condition for filter'
!
      do inod = inod_start_filter, inod_end_filter
        call read_each_filter_stack_coef(id_org_filter, ierr)
!
        call cal_rms_filter_coefs(min_rms_weight, ierr2)
!
        if (min_rms_weight .gt. max_rms_weight_limit) then
          iflag_make_whole_filter(inod) = 1
          iflag_make_fluid_filter(inod) = 1
        end if
!
        if (ierr2 .eq. -1) then
          iflag_make_whole_filter(inod) = 1
          iflag_make_fluid_filter(inod) = 1
        end if
!
        if( iflag_make_whole_filter(inod) .eq. 0) then
          call write_each_filter_stack_coef                             &
     &       (fixed_file_name, inod, ierr)
!
          if(iflag_tgt_filter_type .ge. -4                              &
     &      .and. iflag_tgt_filter_type.le. -2) then
            call s_cal_filter_moments_again                             &
     &         (mesh%node, mesh%ele, g_FEM, jac_3d, FEM_elen,           &
     &          inod, ele_4_nod_f, neib_nod_f, mom_nod)
          end if
        else
!
          if (iflag_tgt_filter_type .eq. -1) then
            call copy_filter_coefs_to_tmp
            call const_filter_func_nod_by_nod                           &
     &         (fixed_file_name, inod, mesh%node, mesh%ele,             &
     &          ele_4_nod_f, neib_nod_f, g_FEM, jac_3d, FEM_elen, ierr)
          else if(iflag_tgt_filter_type .ge. -4                         &
     &      .and. iflag_tgt_filter_type.le. -2) then
            call set_simple_filter_nod_by_nod(fixed_file_name,          &
     &          mesh%node, mesh%ele, g_FEM, jac_3d, FEM_elen,           &
     &          dxidxs%dx_nod, inod, ele_4_nod_f, neib_nod_f)
          end if
!
          nnod_near_nod_weight(inod) = nnod_near_1nod_weight
          call cal_filter_moms_each_nod_type(inod, mom_nod)
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
     &         (id_org_filter, fixed_file_name, mesh,                   &
     &          g_FEM, jac_3d, FEM_elen, dxidxs, mom_nod)
!
      character(len = kchara), intent(in) :: fixed_file_name
      integer(kind = kint), intent(in) :: id_org_filter
!
      type(mesh_geometry), intent(in) :: mesh
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
!
      integer(kind = kint) :: inod, ierr2, ierr
!
!
!
      call init_4_cal_fluid_fileters(mesh, ele_4_nod_f, neib_nod_f)
!
      write(70+my_rank,*) ' Best condition for fluid filter'
!
      do inod = inod_start_filter, inod_end_filter
        call read_each_filter_stack_coef(id_org_filter, ierr)
!
        if ( nnod_near_1nod_weight .gt. 0) then
          call cal_rms_filter_coefs(min_rms_weight, ierr2)
!
          if (min_rms_weight .gt. max_rms_weight_limit) then
            iflag_make_fluid_filter(inod) = 1
          end if
        end if
!
!     no correction
!
        if( iflag_make_fluid_filter(inod) .eq. 0) then
!
          if (nnod_near_1nod_weight .eq. 0) then
            call write_each_no_filter_coef                              &
     &         (fixed_file_name, inod, ierr)
          else if (nnod_near_1nod_weight .lt. 0) then
            nnod_near_1nod_weight = -nnod_near_1nod_weight
            call write_each_same_filter_coef                            &
     &         (fixed_file_name, inod, ierr)
          else
            call write_each_filter_stack_coef                           &
     &         (fixed_file_name, inod, ierr)
          end if
!
!       correct fluid filter
!
        else
!
          if (iflag_tgt_filter_type .eq. -1) then
            call copy_filter_coefs_to_tmp
            call const_fluid_filter_nod_by_nod                          &
     &         (fixed_file_name, inod, mesh%node, mesh%ele,             &
     &          ele_4_nod_f, neib_nod_f, g_FEM, jac_3d, FEM_elen, ierr)
          else if(iflag_tgt_filter_type .ge. -4                         &
     &      .and. iflag_tgt_filter_type.le. -2) then
            call set_simple_fl_filter_nod_by_nod(fixed_file_name,       &
     &          mesh%node, mesh%ele, g_FEM, jac_3d, FEM_elen,           &
     &          dxidxs%dx_nod, inod, ele_4_nod_f, neib_nod_f, mom_nod)
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
