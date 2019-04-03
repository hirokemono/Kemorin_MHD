!
!      module cal_filter_func_each_node
!
!     Written by H. Matsui on Apr., 2008
!
!!      subroutine const_filter_func_nod_by_nod(file_name, inod,        &
!!     &          gfil_p, node, ele, g_FEM, jac_3d, FEM_elen, ref_m,    &
!!     &          ele_4_nod, neib_nod, fil_coef, tmp_coef, whole_area,  &
!!     &          fil_mat, ierr)
!!      subroutine const_fluid_filter_nod_by_nod(file_name, inod,       &
!!     &          gfil_p, node, ele, g_FEM, jac_3d, FEM_elen, ref_m,    &
!!     &          ele_4_nod, neib_nod, fil_coef, tmp_coef, fluid_area,  &
!!     &          fil_mat, ierr)
!!        type(node_data),           intent(in) :: node
!!        type(element_data),        intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(reference_moments), intent(in) :: ref_m
!!        type(element_around_node), intent(inout) :: ele_4_nod
!!        type(next_nod_id_4_nod), intent(inout) :: neib_nod
!!        type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
!!        type(filter_area_flag), intent(inout) :: whole_area
!!        type(filter_area_flag), intent(inout) :: fluid_area
!!        type(matrix_4_filter), intent(inout) :: fil_mat
!
      module cal_filter_func_each_node
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_crs_matrix_4_filter
!
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_filter_elength
      use t_reference_moments
      use t_filter_coefs
      use t_matrix_4_filter
      use t_ctl_params_4_gen_filter
!
      use fem_const_filter_matrix
      use int_filter_functions
!
      use cal_sol_filter_func_nod
      use cal_3d_filter_4_each_node
      use write_filters_4_each_node
      use expand_filter_area_4_1node
      use copy_moments_2_matrix
      use delete_small_weighting
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_filter_func_nod_by_nod(file_name, inod,          &
     &          gfil_p, node, ele, g_FEM, jac_3d, FEM_elen, ref_m,      &
     &          ele_4_nod, neib_nod, fil_coef, tmp_coef, whole_area,    &
     &          fil_mat, ierr)
!
      use cal_1d_moments_4_fliter
!
      character(len = kchara), intent(in) :: file_name
      type(ctl_params_4_gen_filter), intent(in) :: gfil_p
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
!
      integer(kind = kint), intent(in) :: inod
!
      integer(kind = kint), intent(inout) :: ierr
      type(element_around_node), intent(inout) :: ele_4_nod
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
      type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
      type(filter_area_flag), intent(inout) :: whole_area
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      integer(kind = kint) :: i, ist, ied, iint, ntmp, num_free, imat
      integer(kind = kint) :: ibest_fixed_point, ibest_mat_size
      integer(kind = kint) :: num_fixed_point, num_previous_comp
      real(kind= kreal) :: det_mat_solution
!
!
      call copy_next_nod_ele_4_each                                     &
     &     (inod, node%numnod, ele_4_nod, neib_nod, fil_coef)
!
        ibest_mat_size =   -1
        ibest_fixed_point = 0
        if(gfil_p%iflag_tgt_filter_type .gt. iflag_undefined)           &
     &                                      min_rms_weight = 1.0d30
        do i = 1, gfil_p%maximum_neighbour
          if (i .eq. 1) then
            num_previous_comp = gfil_p%minimum_comp
          else
            num_previous_comp = fil_coef%nnod_4_1nod_f
          end if
!
          call s_expand_filter_area_4_1node                             &
     &       (inod, gfil_p, node, ele, ele_4_nod, FEM_elen, fil_coef)
          call resize_matrix_size_gen_filter(ele%nnod_4_ele,            &
     &        fil_coef, fil_tbl_crs, fil_mat_crs, fil_mat)
!
        end do
!
        fil_mat%a_mat = 0.0d0
        fil_mat%vec_mat = 0.0d0
!
!    set nxn matrix
!
        call int_node_filter_matrix(node, ele, g_FEM, jac_3d,           &
     &      ref_m, fil_coef, inod, gfil_p%num_int_points, fil_mat)
!
        if (gfil_p%ist_num_free .eq. -1) then
          if(gfil_p%iflag_tgt_filter_type .eq. -iflag_commutative) then
            ist = gfil_p%minimum_comp
            ied = min(fil_coef%nnod_4_1nod_f,ref_m%num_order_3d)
            iint =  1
          else
            ist = min(fil_coef%nnod_4_1nod_f,ref_m%num_order_3d)
            ied = gfil_p%minimum_comp
            iint = -1
          end if
        else
          if(gfil_p%iflag_tgt_filter_type .eq. -iflag_commutative) then
            ist = gfil_p%ist_num_free
            ied = min(gfil_p%ied_num_free,ref_m%num_order_3d)
            iint =  1
          else
            ist = min(gfil_p%ied_num_free,ref_m%num_order_3d)
            ied = gfil_p%ist_num_free
            iint = -1
          end if
        end if
!
        do num_free = ist, ied, iint
!
          if(gfil_p%iflag_use_fixed_points .eq. 1) then
            ntmp = min(fil_coef%nnod_4_1nod_f, ref_m%num_order_3d)
          else
            ntmp = num_free
          end if
!
          do imat = num_free, ntmp
            num_fixed_point = imat - num_free
!
            call const_filter_mat_each_nod(gfil_p%ref_filter_width(1),  &
     &          node, FEM_elen, ref_m, fil_coef, inod, num_fixed_point, &
     &          fil_mat, ierr)
!
            if (ierr .eq. 1) goto 20
!
            call s_cal_sol_filter_func_nod                              &
     &         (inod, gfil_p, fil_mat, ierr)
            if (ierr .gt. 0) goto 20

            call cal_filter_and_coefficients(gfil_p%num_int_points,     &
     &          ele, g_FEM, jac_3d, fil_mat, fil_coef)
            call cal_rms_filter_coefs(fil_coef, rms_weight, ierr)
!
!              write(70+my_rank,*) 'det_mat', imat, num_fixed_point,    &
!     &                            det_mat, vec_norm, ratio_vec_mat
!
            if ( rms_weight .lt. min_rms_weight                         &
     &            .and. (gfil_p%iflag_negative_center*ierr).eq.0) then
              min_rms_weight = rms_weight
              ibest_fixed_point = num_fixed_point
              ibest_mat_size =   imat
              det_mat_solution = fil_mat%det_mat
              fil_coef%ilevel_exp_1nod_w = gfil_p%maximum_neighbour
              call copy_each_filter_coefs(fil_coef, tmp_coef)
            end if
!
  20        continue
            if(gfil_p%iflag_err_level_filter.eq.3) write(*,*) my_rank,  &
     &          inod, imat, num_fixed_point, rms_weight,                &
     &          fil_mat%det_mat, ierr
!
            if (min_rms_weight.lt. (gfil_p%max_rms_weight_limit/10.0d0) &
     &          .and. imat.eq.ist .and. num_fixed_point.eq.0) exit
          end do
          if (min_rms_weight .lt. gfil_p%max_rms_weight_limit) exit
        end do
!
!     cal filter functions
!
        write(70+my_rank,'(4i16,1p2e23.12)')                            &
     &    inod, gfil_p%maximum_neighbour, ibest_mat_size,               &
     &    ibest_fixed_point, min_rms_weight, det_mat_solution
        if(gfil_p%iflag_err_level_filter.ge.2)                          &
     &   write(*,'(5i16,1p2e23.12)')                                    &
     &    my_rank, inod, gfil_p%maximum_neighbour, ibest_mat_size,      &
     &    ibest_fixed_point, min_rms_weight, det_mat_solution
!
        if (ibest_mat_size .gt. 0) then
          call copy_each_filter_coefs(tmp_coef, fil_coef)
          call s_delete_small_weighting(gfil_p%omitted_ratio, fil_coef)
        else
          if (gfil_p%iflag_tgt_filter_type .gt. iflag_undefined) then
            call set_failed_filter_coefs                                &
     &         (gfil_p%maximum_neighbour, fil_coef)
            whole_area%num_failed = whole_area%num_failed + 1
          else
            call copy_each_filter_coefs(tmp_coef, fil_coef)
          end if
        end if
!
        call write_each_filter_stack_coef                               &
     &     (file_name, inod, fil_coef, ierr)
!
      end subroutine const_filter_func_nod_by_nod
!
! -----------------------------------------------------------------------
!
      subroutine const_fluid_filter_nod_by_nod(file_name, inod,         &
     &          gfil_p, node, ele, g_FEM, jac_3d, FEM_elen, ref_m,      &
     &          ele_4_nod, neib_nod, fil_coef, tmp_coef, fluid_area,    &
     &          fil_mat, ierr)
!
      character(len = kchara), intent(in) :: file_name
      type(ctl_params_4_gen_filter), intent(in) :: gfil_p
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
!
      integer(kind = kint), intent(in) :: inod
!
      integer(kind = kint), intent(inout) :: ierr
      type(element_around_node), intent(inout) :: ele_4_nod
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
      type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
      type(filter_area_flag), intent(inout) :: fluid_area
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      integer(kind = kint) :: i, ist, ied, iint, ntmp, num_free
      integer(kind = kint) :: ibest_fixed_point, ibest_mat_size
      integer(kind = kint) :: num_fixed_point, imat
      real(kind= kreal) :: det_mat_solution
!
!
      call copy_next_nod_ele_4_each                                     &
     &     (inod, node%numnod, ele_4_nod, neib_nod, fil_coef)
!
!    no filtering
!
        if (fil_coef%nnod_4_1nod_w .eq. 0) then
          call write_each_no_filter_coef                                &
     &       (file_name, inod, fil_coef, ierr)
        else
!
          do i = 1, gfil_p%maximum_neighbour
            call s_expand_filter_area_4_1node                           &
     &         (inod, gfil_p, node, ele, ele_4_nod, FEM_elen, fil_coef)
            call resize_matrix_size_gen_filter(ele%nnod_4_ele,          &
     &          fil_coef, fil_tbl_crs, fil_mat_crs, fil_mat)
          end do
!
!    use same filter for fluid area
!
          if(fil_coef%nnod_4_1nod_w                                     &
     &        .eq. fil_coef%nnod_near_nod_w(inod)) then
            fil_coef%ilevel_exp_1nod_w = gfil_p%maximum_neighbour
            call write_each_same_filter_coef                            &
     &         (file_name, inod, fil_coef, ierr)
          else
!
!    construct filter for fluid area
!
            max_det_mat = 0.0d0
            ibest_mat_size =   -1
            ibest_fixed_point = 0
            if(gfil_p%iflag_tgt_filter_type .gt. iflag_undefined)       &
     &                          min_rms_weight = 1.0d30
!
            fil_mat%a_mat = 0.0d0
            fil_mat%vec_mat = 0.0d0
!
            call int_node_filter_matrix(node, ele, g_FEM, jac_3d,       &
     &          ref_m, fil_coef, inod, gfil_p%num_int_points, fil_mat)
!
            if (gfil_p%ist_num_free .eq. -1) then
              if (gfil_p%iflag_tgt_filter_type                          &
     &                    .eq. -iflag_commutative) then
                ist = gfil_p%minimum_comp
                ied = min(fil_coef%nnod_4_1nod_f,ref_m%num_order_3d)
                iint = 1
              else
                ist = min(fil_coef%nnod_4_1nod_f,ref_m%num_order_3d)
                ied = gfil_p%minimum_comp
                iint = -1
              end if
            else
              if (gfil_p%iflag_tgt_filter_type                          &
     &                     .eq. -iflag_commutative) then
                ist = gfil_p%ist_num_free
                ied = min(gfil_p%ied_num_free,ref_m%num_order_3d)
                iint = 1
              else
                ist = min(gfil_p%ied_num_free,ref_m%num_order_3d)
                ied = gfil_p%ist_num_free
                iint = -1
              end if
            end if
!
            do num_free = ist, ied, iint
!
              if(gfil_p%iflag_use_fixed_points .eq. 1) then
                ntmp = min(fil_coef%nnod_4_1nod_f,ref_m%num_order_3d)
              else
                ntmp = num_free
              end if
!
              do imat = num_free, ntmp
                num_fixed_point = imat - num_free
!
                call const_filter_mat_each_nod                          &
     &             (gfil_p%ref_filter_width(1), node, FEM_elen, ref_m,  &
     &              fil_coef, inod, num_fixed_point, fil_mat, ierr)
                if (ierr .eq. 1) goto 21
!
                call s_cal_sol_filter_func_nod                          &
     &             (inod, gfil_p, fil_mat, ierr)
                if (ierr .gt. 0) goto 21
!
                call cal_filter_and_coefficients(gfil_p%num_int_points, &
     &              ele, g_FEM, jac_3d, fil_mat, fil_coef)
                call cal_rms_filter_coefs(fil_coef, rms_weight, ierr)
!
                if ( rms_weight .lt. min_rms_weight                     &
     &            .and. (gfil_p%iflag_negative_center*ierr).eq.0) then
                  min_rms_weight = rms_weight
                  ibest_fixed_point = num_fixed_point
                  ibest_mat_size =    imat
                  det_mat_solution =  fil_mat%det_mat
                  fil_coef%ilevel_exp_1nod_w = gfil_p%maximum_neighbour
                  call copy_each_filter_coefs(fil_coef, tmp_coef)
                end if
!
  21            continue
                if(gfil_p%iflag_err_level_filter.eq.3) write(*,*)       &
     &              my_rank, inod, imat, num_fixed_point,               &
     &              rms_weight, fil_mat%det_mat, ierr
                if(min_rms_weight                                       &
     &             .lt. (gfil_p%max_rms_weight_limit/10.0d0)            &
     &               .and. imat.eq.ist                                  &
     &               .and. num_fixed_point.eq.0) exit
              end do
              if (min_rms_weight .lt. gfil_p%max_rms_weight_limit) exit
            end do
!
            write(70+my_rank,'(4i16,1p2e23.12)')                        &
     &        inod, gfil_p%maximum_neighbour, ibest_mat_size,           &
     &        ibest_fixed_point, min_rms_weight, det_mat_solution
            if (gfil_p%iflag_err_level_filter.ge.2) write(*,*)          &
     &        my_rank, inod, ibest_mat_size, ibest_fixed_point,         &
     &        min_rms_weight, det_mat_solution
!
!
            if (ibest_mat_size .gt. 0) then
              call copy_each_filter_coefs(tmp_coef, fil_coef)
              call s_delete_small_weighting                             &
     &           (gfil_p%omitted_ratio, fil_coef)
            else
              if(gfil_p%iflag_tgt_filter_type                           &
     &                           .gt. iflag_undefined) then
                call set_failed_filter_coefs                            &
     &            (gfil_p%maximum_neighbour, fil_coef)
                fluid_area%num_failed = fluid_area%num_failed + 1
              else
                call copy_each_filter_coefs(tmp_coef, fil_coef)
              end if
            end if
!
            call write_each_filter_stack_coef                           &
     &         (file_name, inod, fil_coef, ierr)
          end if
        end if
!
      end subroutine const_fluid_filter_nod_by_nod
!
! -----------------------------------------------------------------------
!
      end module cal_filter_func_each_node
