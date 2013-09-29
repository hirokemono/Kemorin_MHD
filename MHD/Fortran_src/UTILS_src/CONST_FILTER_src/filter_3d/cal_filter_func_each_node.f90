!
!      module cal_filter_func_each_node
!
!     Written by H. Matsui on Apr., 2008
!
!      subroutine const_filter_func_nod_by_nod(inod, ierr)
!      subroutine const_fluid_filter_nod_by_nod(inod, ierr)
!
      module cal_filter_func_each_node
!
      use m_precision
!
      use m_constants
      use m_parallel_var_dof
      use m_ctl_params_4_gen_filter
      use m_geometry_parameter
      use m_filter_coefs
      use m_matrix_4_filter
      use m_reference_moments
      use m_element_id_4_node
      use m_next_node_id_4_node
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
      subroutine const_filter_func_nod_by_nod(inod, ierr)
!
      use cal_1d_moments_4_fliter
      use set_simple_filters
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: i, ist, ied, iint, ntmp, num_free
      integer(kind = kint) :: ibest_fixed_point, ibest_mat_size
      integer(kind = kint) :: num_fixed_point, num_previous_comp
      real(kind= kreal) :: det_mat_solution
!
!
        call copy_next_nod_ele_4_each(inod, numnod)
!
        ibest_mat_size =   -1
        ibest_fixed_point = 0
        if (iflag_tgt_filter_type .gt. 0) min_rms_weight = 1.0d30
        do i = 1, maximum_neighbour
!
          if (i .eq. 1) then
            num_previous_comp = minimum_comp
          else
            num_previous_comp = nnod_near_1nod_filter
          end if
!
          call s_expand_filter_area_4_1node(inod)
          call resize_matrix_size_gen_filter
!
        end do
!
        a_mat = 0.0d0
        vec_mat = 0.0d0
!
!    set nxn matrix
!
        call int_node_filter_matrix(inod, num_int_points,               &
     &       nele_near_1nod_weight, iele_near_1nod_weight(1),           &
     &       nnod_near_1nod_weight, inod_near_1nod_weight(1),           &
     &       nnod_near_1nod_filter)
!
        if (ist_num_free .eq. -1) then
          if (iflag_tgt_filter_type .eq. -1) then
            ist = minimum_comp
            ied = min(nnod_near_1nod_filter,num_order_3d)
            iint =  1
          else
            ist = min(nnod_near_1nod_filter,num_order_3d)
            ied = minimum_comp
            iint = -1
          end if
        else
          if (iflag_tgt_filter_type .eq. -1) then
            ist = ist_num_free
            ied = min(ied_num_free,num_order_3d)
            iint =  1
          else
            ist = min(ied_num_free,num_order_3d)
            ied = ist_num_free
            iint = -1
          end if
        end if
!
        do num_free = ist, ied, iint
!
          if (iflag_use_fixed_points .eq. 1) then
            ntmp = min(nnod_near_1nod_filter,num_order_3d)
          else
            ntmp = num_free
          end if
!
          do mat_size = num_free, ntmp
            num_fixed_point = mat_size - num_free
!
            call const_filter_mat_each_nod(inod, num_fixed_point, ierr)
!
            if (ierr .eq. 1) goto 20
!
            call s_cal_sol_filter_func_nod(inod, ierr)
            if (ierr .gt. 0) goto 20

            call cal_filter_and_coefficients
            call cal_rms_filter_coefs(rms_weight, ierr)
!
!              write(70+my_rank,*) 'det_mat', mat_size, num_fixed_point,&
!     &                            det_mat, vec_norm, ratio_vec_mat
!
            if ( rms_weight .lt. min_rms_weight                         &
     &            .and. (iflag_negative_center*ierr).eq.0) then
              min_rms_weight = rms_weight
              ibest_fixed_point = num_fixed_point
              ibest_mat_size = mat_size
              det_mat_solution = det_mat
              i_exp_level_1nod_weight = maximum_neighbour
              call copy_filter_coefs_to_tmp
            end if
!
  20        continue
            if (iflag_err_level_filter.eq.3) write(*,*) my_rank,        &
     &          inod, mat_size, num_fixed_point, rms_weight, det_mat,   &
     &          ierr
!
            if (min_rms_weight.lt. (max_rms_weight_limit/10.0d0)        &
     &          .and. mat_size.eq.ist .and. num_fixed_point.eq.0) exit
          end do
          if (min_rms_weight .lt. max_rms_weight_limit) exit
        end do
!
!     cal filter functions
!
        write(70+my_rank,'(4i10,1p2e23.12)')                            &
     &    inod, maximum_neighbour, ibest_mat_size,                      &
     &    ibest_fixed_point, min_rms_weight, det_mat_solution
        if (iflag_err_level_filter.ge.2) write(*,'(5i10,1p2e23.12)')    &
     &    my_rank, inod, maximum_neighbour, ibest_mat_size,             &
     &    ibest_fixed_point, min_rms_weight, det_mat_solution
!
        if (ibest_mat_size .gt. 0) then
          call copy_filter_coefs_from_tmp
          call s_delete_small_weighting
        else
          if (iflag_tgt_filter_type .gt. 0) then
            i_exp_level_1nod_weight = -maximum_neighbour
            filter_1nod(1:nnod_near_1nod_weight) = 0.0e00
            weight_1nod(1) = 1.0e00
            weight_1nod(2:nnod_near_1nod_weight) = 0.0e00
            num_failed_whole = num_failed_whole + 1
          else
            call copy_filter_coefs_from_tmp
          end if
        end if
!
        call write_each_filter_stack_coef(inod)
!
        nnod_near_nod_weight(inod) = nnod_near_1nod_weight
        call cal_filter_moments_each_nod(ione, inod)
!
      end subroutine const_filter_func_nod_by_nod
!
! -----------------------------------------------------------------------
!
      subroutine const_fluid_filter_nod_by_nod(inod, ierr)
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: i, ist, ied, iint, ntmp, num_free
      integer(kind = kint) :: ibest_fixed_point, ibest_mat_size
      integer(kind = kint) :: num_fixed_point
      real(kind= kreal) :: det_mat_solution
!
!
        call copy_next_nod_ele_4_each(inod, numnod)
!
!    no filtering
!
        if (nnod_near_1nod_weight .eq. 0) then
          call write_each_no_filter_coef(inod)
        else
!
          do i = 1, maximum_neighbour
            call s_expand_filter_area_4_1node(inod)
            call resize_matrix_size_gen_filter
          end do
!
!    use same filter for fluid area
!
          if (nnod_near_1nod_weight .eq. nnod_near_nod_weight(inod))    &
     &      then
            i_exp_level_1nod_weight = maximum_neighbour
            call write_each_same_filter_coef(inod)
          else
!
!    construct filter for fluid area
!
            max_det_mat = 0.0d0
            ibest_mat_size =   -1
            ibest_fixed_point = 0
            if (iflag_tgt_filter_type .gt. 0) min_rms_weight = 1.0d30
!
            a_mat = 0.0d0
            vec_mat = 0.0d0
!
            call int_node_filter_matrix(inod, num_int_points,           &
     &            nele_near_1nod_weight, iele_near_1nod_weight(1),      &
     &            nnod_near_1nod_weight, inod_near_1nod_weight(1),      &
     &            nnod_near_1nod_filter)
!
            if (ist_num_free .eq. -1) then
              if (iflag_tgt_filter_type .eq. -1) then
                ist = minimum_comp
                ied = min(nnod_near_1nod_filter,num_order_3d)
                iint = 1
              else
                ist = min(nnod_near_1nod_filter,num_order_3d)
                ied = minimum_comp
                iint = -1
              end if
            else
              if (iflag_tgt_filter_type .eq. -1) then
                ist = ist_num_free
                ied = min(ied_num_free,num_order_3d)
                iint = 1
              else
                ist = min(ied_num_free,num_order_3d)
                ied = ist_num_free
                iint = -1
              end if
            end if
!
            do num_free = ist, ied, iint
!
              if (iflag_use_fixed_points .eq. 1) then
                ntmp = min(nnod_near_1nod_filter,num_order_3d)
              else
                ntmp = num_free
              end if
!
              do mat_size = num_free, ntmp
                num_fixed_point = mat_size - num_free
!
                call const_filter_mat_each_nod(inod, num_fixed_point,   &
     &              ierr)
                if (ierr .eq. 1) goto 21
!
                call s_cal_sol_filter_func_nod(inod, ierr)
                if (ierr .gt. 0) goto 21
!
                call cal_filter_and_coefficients
                call cal_rms_filter_coefs(rms_weight, ierr)
!
                if ( rms_weight .lt. min_rms_weight                     &
     &            .and. (iflag_negative_center*ierr).eq.0) then
                  min_rms_weight = rms_weight
                  ibest_fixed_point = num_fixed_point
                  ibest_mat_size = mat_size
                  det_mat_solution = det_mat
                  i_exp_level_1nod_weight = maximum_neighbour
                  call copy_filter_coefs_to_tmp
                end if
!
  21            continue
                if (iflag_err_level_filter.eq.3) write(*,*)             &
     &              my_rank, inod, mat_size, num_fixed_point,           &
     &              rms_weight, det_mat, ierr
                if (min_rms_weight.lt. (max_rms_weight_limit/10.0d0)    &
     &               .and. mat_size.eq.ist                              &
     &               .and. num_fixed_point.eq.0) exit
              end do
              if (min_rms_weight .lt. max_rms_weight_limit) exit
            end do
!
            write(70+my_rank,'(4i10,1p2e23.12)')                        &
     &        inod, maximum_neighbour, ibest_mat_size,                  &
     &        ibest_fixed_point, min_rms_weight, det_mat_solution
            if (iflag_err_level_filter.ge.2) write(*,*) my_rank, inod,  &
     &        ibest_mat_size, ibest_fixed_point, min_rms_weight,        &
     &        det_mat_solution
!
!
            if (ibest_mat_size .gt. 0) then
              call copy_filter_coefs_from_tmp
              call s_delete_small_weighting
            else
              if (iflag_tgt_filter_type .gt. 0) then
                i_exp_level_1nod_weight = -maximum_neighbour
                filter_1nod(1:nnod_near_1nod_weight) = 0.0e00
                weight_1nod(1) = 1.0e00
                weight_1nod(2:nnod_near_1nod_weight) = 0.0e00
                num_failed_fluid = num_failed_fluid + 1
              else
                call copy_filter_coefs_from_tmp
              end if
            end if
!
            call write_each_filter_stack_coef(inod)
!
          end if
        end if
!
      end subroutine const_fluid_filter_nod_by_nod
!
! -----------------------------------------------------------------------
!
      end module cal_filter_func_each_node
