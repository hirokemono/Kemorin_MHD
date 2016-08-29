!
!      module set_ctl_gen_filter
!
!     Written by H. Matsui on July, 2006
!
!      subroutine set_ctl_params_gen_filter(FEM_elen)
!      subroutine set_file_heads_3d_comm_filter
!
      module set_ctl_gen_filter
!
      use m_precision
      use m_machine_parameter
      use m_ctl_data_4_platforms
      use m_ctl_data_gen_filter
      use m_ctl_data_gen_3d_filter
      use m_ctl_data_filter_files
      use m_ctl_params_4_gen_filter
      use m_ctl_param_newdom_filter
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_gen_filter(FEM_elen)
!
      use calypso_mpi
      use m_error_IDs
      use m_ctl_data_4_solvers
      use m_ctl_data_org_filter_name
      use m_reference_moments
!
      use t_filter_elength
!
      use skip_comment_f
!
      type(gradient_model_data_type), intent(inout) :: FEM_elen
!
      integer(kind = kint) :: i
!
!
      call set_file_heads_3d_comm_filter
!
      np_smp = 1
      if(num_smp_ctl%iflag .gt. 0) np_smp = num_smp_ctl%intvalue
!
      num_int_points = 4
      if (i_num_int_points .ne. 0) num_int_points = num_int_points_ctl
!
      minimum_comp = minimum_comp_ctl
!
      num_filtering_grp = filter_area_ctl%num
      if (iflag_debug.gt.0) then
        write(*,*) 'np_smp', np_smp
        write(*,*) 'num_int_points', num_int_points
        write(*,*) 'minimum_comp', minimum_comp
        write(*,*) 'num_filtering_grp', num_filtering_grp
      end if
!
!
      if ( num_filtering_grp .eq. 0) then
        num_filtering_grp = 1
        call allocate_ref_filter_type
        filter_area_name(1) = 'all'
      else if ( num_filtering_grp .gt. 0) then
        call allocate_ref_filter_area
        filter_area_name(1:num_filtering_grp)                           &
     &        = filter_area_ctl%c_tbl(1:num_filtering_grp)
        call deallocate_filtering_area_ctl
      end if
!
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'num_filtering_grp ', num_filtering_grp
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'filter_area_name ', filter_area_name
!
!
      num_ref_filter = reference_filter_ctl%num
      FEM_elen%filter_conf%nf_type = num_ref_filter
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'num_ref_filter', num_ref_filter
!
      if ( num_ref_filter .gt. 0) then
        call allocate_ref_filter_type
        call alloc_ref_1d_mom_type(FEM_elen%filter_conf)
        
        do i = 1, num_ref_filter
          iref_filter_type(i) = iflag_tophat_filter
          if(cmp_no_case(reference_filter_ctl%c_tbl(i), 'Gaussian')     &
     &          ) iref_filter_type(i) = iflag_gaussian_filter
          if(cmp_no_case(reference_filter_ctl%c_tbl(i), 'Linear')       &
     &          ) iref_filter_type(i) = iflag_linear_filter
          if(cmp_no_case(reference_filter_ctl%c_tbl(i), 'Tophat')       &
     &          ) iref_filter_type(i) = iflag_tophat_filter
!
          FEM_elen%filter_conf%filter_type(i)                           &
     &          = reference_filter_ctl%c_tbl(i)
!
          ref_filter_width(i) = reference_filter_ctl%vect(i)
          FEM_elen%filter_conf%f_width(i) = ref_filter_width(i)
        end do
!
        call deallocate_ref_filter_ctl
!
        if (iflag_debug.gt.0)  then
          write(*,*) 'iref_filter_type', iref_filter_type
          write(*,*) 'ref_filter_width', ref_filter_width
        end if
!
      end if
!
      num_moments_order = ref_filter_mom_ctl%num
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'num_moments_order', num_moments_order
!
      if (num_moments_order .gt. 0) then
        call allocate_moment_parameter
!
        do i = 1, num_moments_order
          mom_order(i) = ref_filter_mom_ctl%ivec(i)
          mom_value(i) = ref_filter_mom_ctl%vect(i)
        end do
!
        max_num_order_1d = mom_order(1)
        do i = 2, num_moments_order
          max_num_order_1d = max(max_num_order_1d,mom_order(i))
        end do
        max_num_order_3d =  (max_num_order_1d+1)**3
        num_order_3d = num_moments_order**3
        num_order_1d = num_moments_order
!
        do i = 1, num_moments_order
          if(cmp_no_case(ref_filter_mom_ctl%c_tbl(i), 'refered')) then
            iref_mom_type(i) = 1
          else
            iref_mom_type(i) = 0
          end if
        end do
!
        call dealloc_control_array_i_c_r(ref_filter_mom_ctl)
!
      if (iflag_debug.gt.0)  then
          write(*,*) 'mom_order', mom_order
          write(*,*) 'mom_value', mom_value
          write(*,*) 'iref_mom_type', iref_mom_type
        end if
!
      end if
!
      if (i_minimum_det .gt. 0) then
        minimum_det_mat = minimum_det_ctl
      end if
!
      if (i_maximum_neighbour .gt. 0) then
        maximum_neighbour = maximum_neighbour_ctl
      else
        maximum_neighbour = 2
      end if
!
      if (i_tgt_filter_type .gt. 0) then
        if     (cmp_no_case(tgt_filter_type_ctl,'TOPHAT')) then
          iflag_tgt_filter_type = 2
        else if(cmp_no_case(tgt_filter_type_ctl,'LINEAR')) then
          iflag_tgt_filter_type = 3
        else if(cmp_no_case(tgt_filter_type_ctl,'GAUSSIAN')) then
          iflag_tgt_filter_type = 4
        else if(cmp_no_case(tgt_filter_type_ctl,'COMMUTATIVE')) then
          iflag_tgt_filter_type = 1
        else if(cmp_no_case(tgt_filter_type_ctl,'NONE')) then
          iflag_tgt_filter_type = -10
        else if(cmp_no_case(tgt_filter_type_ctl,'NO')) then
          iflag_tgt_filter_type = -10
        else
          iflag_tgt_filter_type = 0
        end if
      else
        iflag_tgt_filter_type = 0
      end if
!
      if (i_filter_corection .gt. 0) then
        if      (yes_flag(filter_correction_ctl)) then
          if (iflag_tgt_filter_type .gt. 0) then
            iflag_tgt_filter_type = -iflag_tgt_filter_type
          end if
        end if
      end if
!
!
      if (iflag_tgt_filter_type .lt. 0) then
        if(i_org_filter_coef_head .gt. 0) then
          org_filter_coef_head = org_filter_coef_head_ctl
        else
          write(*,*) 'set original filter coefs file name'
        end if
      end if
!
!
      if (i_filter_fixed_point .gt. 0) then
        if      (yes_flag(filter_fixed_point_ctl)) then
          iflag_use_fixed_points = 1
        else
          iflag_use_fixed_points = 0
        end if
      else
        iflag_use_fixed_points = 0
      end if
!
      if (i_filter_negative_center .gt. 0) then
        if      (yes_flag(negative_center_ctl)) then
          iflag_negative_center = 0
        else
          iflag_negative_center = 1
        end if
      else
        iflag_negative_center = 1
      end if
!
      if (i_err_level_commute .gt. 0) then
        iflag_err_level_filter = ilevel_filter_error_info
      else
        iflag_err_level_filter = 0
      end if
!
      if (i_maximum_rms .gt. 0) then
        max_rms_weight_limit = maximum_rms_ctl
      end if
!
      if (i_momentum_type .gt. 0) then
        if     (cmp_no_case(momentum_type_ctl,'NO_CROSS')) then
          iflag_momentum_type = 1
        else if(cmp_no_case(momentum_type_ctl,'NORMAL')) then
          iflag_momentum_type = 0
        else
          iflag_momentum_type = 0
        end if
      else
        iflag_momentum_type = 0
      end if
!
      if (i_ordering_list.gt.0 .and. iflag_tgt_filter_type.eq.0) then
        if     (cmp_no_case(ordering_list_ctl,'CONNECTION')) then
          iflag_ordering_list = 0
        else if(cmp_no_case(ordering_list_ctl,'DISTANCE')) then
          iflag_ordering_list = 1
        else if(cmp_no_case(ordering_list_ctl,'DISTANCE_RATIO')) then
          iflag_ordering_list = 2
        end if 
      else
        iflag_ordering_list = 0
      end if
!
!
      if     (cmp_no_case(mass_matrix_type_ctl,'CONSIST')) then
        itype_mass_matrix = 1
      else
        itype_mass_matrix = 0
      end if
!
!
      if     (cmp_no_case(f_solver_type_ctl,'ITERATIVE')) then
        id_solver_type = 1
      else
        id_solver_type = 0
      end if
!
      if (i_start_node_ctl .gt. 0) then
        inod_start_filter = start_node_ctl
      else
        inod_start_filter = 1
      end if
!
      if (i_end_node_ctl .gt. 0) then
        inod_end_filter = end_node_ctl
      else
        inod_end_filter = -1
      end if
!
      if (i_start_nfree_mat .gt. 0) then
        ist_num_free = ist_num_free_ctl
      else
        ist_num_free = -1
      end if
!
!
      if (i_end_nfree_mat .gt. 0) then
        ied_num_free = ied_num_free_ctl
      else
        ied_num_free = -1
      end if
!
      if(method_ctl%iflag .gt. 0)  method =  method_ctl%charavalue
      if(precond_ctl%iflag .gt. 0) precond = precond_ctl%charavalue
      if(itr_ctl%iflag .gt. 0) itr = itr_ctl%intvalue
      if(eps_ctl%iflag .gt. 0) eps = eps_ctl%realvalue
      if(sigma_ctl%iflag .gt. 0) sigma = sigma_ctl%realvalue
      if(sigma_diag_ctl%iflag .gt. 0) then
        sigma_diag =  sigma_diag_ctl%realvalue
      end if
!
!
      method_elesize =      method_esize_ctl
      precond_elesize =     precond_esize_ctl
      itr_elesize =         itr_esize_ctl
      eps_elesize =         eps_esize_ctl
      sigma_elesize =       sigma_esize_ctl
      sigma_diag_elesize =  sigma_diag_esize_ctl
!
      if (iflag_debug.gt.0) then
          write(*,*) 'id_solver_type ', id_solver_type
          write(*,*) 'method ', method
          write(*,*) 'precond ', precond
          write(*,*) 'method ', method
          write(*,*) 'itr ', itr
          write(*,*) 'eps ', eps
          write(*,*) 'sigma ', sigma
          write(*,*) 'sigma_diag ', sigma_diag
        end if
!
      if (iflag_tgt_filter_type .eq. -1                                 &
     &  .and. i_org_filter_coef_head .eq. 0) then
        e_message = "set original filter coefficient datafile"
        call calypso_MPI_abort(ierr_file, e_message)
      end if
!
      end subroutine set_ctl_params_gen_filter
!
!  ---------------------------------------------------------------------
!
      subroutine set_file_heads_3d_comm_filter
!
      use m_read_mesh_data
      use m_file_format_switch
      use m_filter_file_names
!
!
      if (mesh_file_prefix%iflag .ne. 0) then
        mesh_file_head = mesh_file_prefix%charavalue
      else
        mesh_file_head = def_mesh_file_head
      end if
      if (iflag_debug.gt.0) write(*,*) 'mesh_file_head ', mesh_file_head
!
      if (filter_head_ctl%iflag .ne. 0) then
        filter_3d_head = filter_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_3d_head ', filter_3d_head
      end if
!
      if (filter_coef_head_ctl%iflag .ne. 0) then
        filter_coef_head = filter_coef_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_coef_head ', filter_coef_head
      end if
!
      if (filter_elen_head_ctl%iflag .ne. 0) then
        filter_elen_head = filter_elen_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_elen_head ', filter_elen_head
      end if
!
      if (filter_moms_head_ctl%iflag .ne. 0) then
        filter_moms_head = filter_moms_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_moms_head ', filter_moms_head
      end if
!
      omitted_ratio = omitted_ratio_ctl
!
!   set data format
!
      call choose_file_format(mesh_file_fmt_ctl, iflag_mesh_file_fmt)
!
      call choose_file_format(filter_3d_format, ifmt_3d_filter)
      call choose_file_format(filter_elen_format, ifmt_filter_elen)
      ifmt_filter_moms = ifmt_filter_elen
!
      end subroutine set_file_heads_3d_comm_filter
!
!  ---------------------------------------------------------------------
!
      subroutine set_numdomain_3d_comm_filter(nprocs)
!
      integer(kind = kint), intent(inout) :: nprocs
!
!
      if (ndomain_ctl%iflag .ne. 0) then
        nprocs = ndomain_ctl%intvalue
      else
        write(*,*) 'set number of domains'
        stop
      end if
      write(*,*) 'number of subdomain: ', nprocs
!
      end subroutine set_numdomain_3d_comm_filter
!
!  ---------------------------------------------------------------------
!
      end module set_ctl_gen_filter
