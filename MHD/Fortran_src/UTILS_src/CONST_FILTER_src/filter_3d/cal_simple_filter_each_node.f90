!cal_simple_filter_each_node.f90
!      module cal_simple_filter_each_node
!
!     Written by H. Matsui on Nov., 2008
!
!!      subroutine set_simple_filter_nod_by_nod                         &
!!     &         (file_name, node, ele, g_FEM, jac_3d, FEM_elen, dx_nod,&
!!     &          inod, ele_4_nod, neib_nod)
!!      subroutine set_simple_fl_filter_nod_by_nod                      &
!!     &         (file_name, node, ele, g_FEM, jac_3d, FEM_elen, dx_nod,&
!!     &          inod, ele_4_nod, neib_nod, mom_nod)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(dxidx_direction_type), intent(in) :: dx_nod
!!        type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
!
!
      module cal_simple_filter_each_node
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_filter_elength
      use t_filter_dxdxi
      use t_next_node_ele_4_node
!
      implicit none
!
      integer(kind = kint), parameter :: num_fixed_point = 0
      private :: num_fixed_point
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_simple_filter_nod_by_nod                           &
     &         (file_name, node, ele, g_FEM, jac_3d, FEM_elen, dx_nod,  &
     &          inod, ele_4_nod, neib_nod)
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_matrix_4_filter
!
      use expand_filter_area_4_1node
      use set_simple_filters
      use cal_3d_filter_4_each_node
      use int_filter_functions
      use fem_const_filter_matrix
      use delete_small_weighting
      use write_filters_4_each_node
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: inod
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(dxidx_direction_type), intent(in) :: dx_nod
!
      type(element_around_node), intent(inout) :: ele_4_nod
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
!
      integer(kind = kint) :: i
!
!
      call copy_next_nod_ele_4_each                                     &
     &     (inod, node%numnod, ele_4_nod, neib_nod)
      call resize_matrix_size_gen_filter(ele%nnod_4_ele)
!
!   set filter area for tophat filter
      if ( abs(iflag_tgt_filter_type) .eq. 2) then
        do i = 2, maximum_neighbour
          call s_expand_filter_area_4_1node                             &
     &       (inod, node, ele, ele_4_nod, FEM_elen)
          nnod_near_1nod_filter = nnod_near_1nod_weight
          nele_near_1nod_filter = nele_near_1nod_weight
          call resize_matrix_size_gen_filter(ele%nnod_4_ele)
        end do
!
!   set filter area for other filters
      else
        do i = 1, maximum_neighbour
          call s_expand_filter_area_4_1node                             &
     &       (inod, node, ele, ele_4_nod, FEM_elen)
          call resize_matrix_size_gen_filter(ele%nnod_4_ele)
        end do
      end if
      mat_size = nnod_near_1nod_weight
!
!    set nxn matrix
!
      call int_node_filter_matrix                                       &
     &   (node, ele, g_FEM, jac_3d, inod, num_int_points,               &
     &    nele_near_1nod_weight, iele_near_1nod_weight(1),              &
     &    nnod_near_1nod_weight, inod_near_1nod_weight(1),              &
     &    nnod_near_1nod_filter)
!
      call copy_2_filter_matrix(num_fixed_point)
!
!      set filter function without normalization
!
      if      ( abs(iflag_tgt_filter_type) .eq. 2) then
        call set_tophat_filter_4_each_nod(nnod_near_1nod_filter)
      else if ( abs(iflag_tgt_filter_type) .eq. 3) then
        call set_linear_filter_4_each_nod                               &
     &     (nnod_near_1nod_filter, idist_from_center_1nod,              &
     &      maximum_neighbour)
      else if ( abs(iflag_tgt_filter_type) .eq. 4) then
        call set_gaussian_filter_each_nod                               &
     &     (node%numnod, node%xx, inod, node%numnod,                    &
     &      dx_nod%dxi%df_dx, dx_nod%dxi%df_dy, dx_nod%dxi%df_dz,       &
     &      dx_nod%dei%df_dx, dx_nod%dei%df_dy, dx_nod%dei%df_dz,       &
     &      dx_nod%dzi%df_dx, dx_nod%dzi%df_dy, dx_nod%dzi%df_dz)
      end if
!
      call cal_filter_and_coefficients(ele, g_FEM, jac_3d)
      call normalize_each_filter_weight
!
      call s_delete_small_weighting(node%numnod)
      call write_each_filter_stack_coef(file_name, inod)
!
      end subroutine set_simple_filter_nod_by_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_simple_fl_filter_nod_by_nod                        &
     &         (file_name, node, ele, g_FEM, jac_3d, FEM_elen, dx_nod,  &
     &          inod, ele_4_nod, neib_nod, mom_nod)
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_matrix_4_filter
      use t_filter_moments
!
      use expand_filter_area_4_1node
      use set_simple_filters
      use cal_3d_filter_4_each_node
      use int_filter_functions
      use fem_const_filter_matrix
      use delete_small_weighting
      use write_filters_4_each_node
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: inod
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(dxidx_direction_type), intent(in) :: dx_nod
!
      type(element_around_node), intent(inout) :: ele_4_nod
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
      type(nod_mom_diffs_type), intent(inout) :: mom_nod(2)
!
      integer(kind = kint) :: i
!
!
      call copy_next_nod_ele_4_each                                     &
     &     (inod, node%numnod, ele_4_nod, neib_nod)
!
!    no filtering
!
      if (nnod_near_1nod_weight .eq. 0) then
        call write_each_no_filter_coef(file_name, inod)
      else
!
!   set filter area for tophat filter
        if ( abs(iflag_tgt_filter_type) .eq. 2) then
          do i = 2, maximum_neighbour
            call s_expand_filter_area_4_1node                           &
     &         (inod, node, ele, ele_4_nod, FEM_elen)
            nnod_near_1nod_filter = nnod_near_1nod_weight
            nele_near_1nod_filter = nele_near_1nod_weight
          end do
!
!   set filter area for other filters
        else
          do i = 1, maximum_neighbour
            call s_expand_filter_area_4_1node                           &
     &         (inod, node, ele, ele_4_nod, FEM_elen)
          end do
        end if
        mat_size = nnod_near_1nod_weight
!
!    use same filter for fluid area
!
        if (nnod_near_1nod_weight .eq. nnod_near_nod_weight(inod))      &
     &     then
          call write_each_same_filter_coef(file_name, inod)
          call copy_moments_each_point                                  &
     &       (inod, mom_nod(1)%moms, inod, mom_nod(2)%moms)
!
!    construct filter for fluid area
!
        else
          call int_node_filter_matrix                                   &
     &       (node, ele, g_FEM, jac_3d, inod, num_int_points,           &
     &        nele_near_1nod_weight, iele_near_1nod_weight(1),          &
     &        nnod_near_1nod_weight, inod_near_1nod_weight(1),          &
     &        nnod_near_1nod_filter)
!
          call copy_2_filter_matrix(num_fixed_point)
!
!      set filter function without normalization
!
          if      ( abs(iflag_tgt_filter_type) .eq. 2) then
            call set_tophat_filter_4_each_nod(nnod_near_1nod_filter)
          else if ( abs(iflag_tgt_filter_type) .eq. 3) then
            call set_linear_filter_4_each_nod                           &
     &         (nnod_near_1nod_filter, idist_from_center_1nod,          &
     &          maximum_neighbour)
          else if ( abs(iflag_tgt_filter_type) .eq. 4) then
            call set_gaussian_filter_each_nod                           &
     &         (node%numnod, node%xx, inod, node%numnod,                &
     &          dx_nod%dxi%df_dx, dx_nod%dxi%df_dy, dx_nod%dxi%df_dz,   &
     &          dx_nod%dei%df_dx, dx_nod%dei%df_dy, dx_nod%dei%df_dz,   &
     &          dx_nod%dzi%df_dx, dx_nod%dzi%df_dy, dx_nod%dzi%df_dz)
          end if
!
          call cal_filter_and_coefficients(ele, g_FEM, jac_3d)
          call normalize_each_filter_weight
!
          call s_delete_small_weighting(node%numnod)
          call write_each_filter_stack_coef(file_name, inod)
        end if
      end if
!
      end subroutine set_simple_fl_filter_nod_by_nod
!
! -----------------------------------------------------------------------
!
      end module cal_simple_filter_each_node
