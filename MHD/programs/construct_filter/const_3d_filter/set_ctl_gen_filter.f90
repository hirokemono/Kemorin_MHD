!
!      module set_ctl_gen_filter
!
!     Written by H. Matsui on July, 2006
!
!      subroutine set_ctl_params_gen_filter
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
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_gen_filter
!
      use calypso_mpi
      use calypso_mpi
      use m_ctl_data_4_solvers
      use m_ctl_data_org_filter_name
      use m_filter_elength
      use m_reference_moments
!
      integer(kind = kint) :: i
!
!
      call set_file_heads_3d_comm_filter
!
      np_smp = 1
      if(i_num_smp .gt. 0) np_smp = num_smp_ctl
!
      num_int_points = 4
      if (i_num_int_points .ne. 0) num_int_points = num_int_points_ctl
!
      minimum_comp = minimum_comp_ctl
!
      num_filtering_grp = num_filtering_grp_ctl
      if (iflag_debug.gt.0) then
        write(*,*) 'np_smp', np_smp
        write(*,*) 'num_int_points', num_int_points
        write(*,*) 'minimum_comp', minimum_comp
        write(*,*) 'num_filtering_grp_ctl', num_filtering_grp_ctl
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
     &        = filter_area_name_ctl(1:num_filtering_grp)
        call deallocate_filtering_area_ctl
      end if
!
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'num_filtering_grp ', num_filtering_grp
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'filter_area_name ', filter_area_name
!
!
      num_ref_filter = num_ref_filter_ctl
      nf_type = num_ref_filter
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'num_ref_filter', num_ref_filter
!
      if ( num_ref_filter .gt. 0) then
        call allocate_ref_filter_type
        call allocate_ref_1d_moment
        
        do i = 1, num_ref_filter
          if (      ref_filter_type_ctl(i) .eq. 'gaussian'              &
     &         .or. ref_filter_type_ctl(i) .eq. 'Gaussian'              &
     &         .or. ref_filter_type_ctl(i) .eq. 'GAUSSIAN' ) then
            iref_filter_type(i) = 2
          else if(  ref_filter_type_ctl(i) .eq. 'linear'                &
     &         .or. ref_filter_type_ctl(i) .eq. 'Linear'                &
     &         .or. ref_filter_type_ctl(i) .eq. 'LINEAR' ) then
            iref_filter_type(i) = 1
          else if(  ref_filter_type_ctl(i) .eq. 'tophat'                &
     &         .or. ref_filter_type_ctl(i) .eq. 'Tophat'                &
     &         .or. ref_filter_type_ctl(i) .eq. 'TOPHAT' ) then
            iref_filter_type(i) = 0
          else
            iref_filter_type(i) = 0
          end if
!
          filter_type(i) = ref_filter_type_ctl(i)
!
          ref_filter_width(i) = ref_filter_width_ctl(i)
          f_width(i) = ref_filter_width_ctl(i)
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
      num_moments_order = num_moments_order_ctl
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'num_moments_order', num_moments_order
!
      if (num_moments_order .gt. 0) then
        call allocate_moment_parameter
!
        mom_order = mom_order_ctl
        mom_value = mom_value_ctl
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
          if   (ref_mom_type_ctl(i) .eq. 'refered'                      &
     &     .or. ref_mom_type_ctl(i) .eq. 'Refered'                      &
     &     .or. ref_mom_type_ctl(i) .eq. 'REFERED' ) then
            iref_mom_type(i) = 1
          else
            iref_mom_type(i) = 0
          end if
        end do
!
        call deallocate_mom_param_ctl
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
        if      (tgt_filter_type_ctl .eq. 'tophat'                      &
     &      .or. tgt_filter_type_ctl .eq. 'Tophat'                      &
     &      .or. tgt_filter_type_ctl .eq. 'TOPHAT') then
          iflag_tgt_filter_type = 2
        else if (tgt_filter_type_ctl .eq. 'linear'                      &
     &      .or. tgt_filter_type_ctl .eq. 'Linear'                      &
     &      .or. tgt_filter_type_ctl .eq. 'LINEAR') then
          iflag_tgt_filter_type = 3
        else if (tgt_filter_type_ctl .eq. 'gaussian'                    &
     &      .or. tgt_filter_type_ctl .eq. 'Gaussian'                    &
     &      .or. tgt_filter_type_ctl .eq. 'GAUSSIAN') then
          iflag_tgt_filter_type = 4
        else if (tgt_filter_type_ctl .eq. 'commutative'                 &
     &      .or. tgt_filter_type_ctl .eq. 'Commutative'                 &
     &      .or. tgt_filter_type_ctl .eq. 'COMMUTATIVE') then
          iflag_tgt_filter_type = 1
        else if (tgt_filter_type_ctl .eq. 'none'                        &
     &      .or. tgt_filter_type_ctl .eq. 'None'                        &
     &      .or. tgt_filter_type_ctl .eq. 'NONE'                        &
     &      .or. tgt_filter_type_ctl .eq. 'no'                          &
     &      .or. tgt_filter_type_ctl .eq. 'No'                          &
     &      .or. tgt_filter_type_ctl .eq. 'NO') then
          iflag_tgt_filter_type = -10
        else
          iflag_tgt_filter_type = 0
        end if
      else
        iflag_tgt_filter_type = 0
      end if
!
      if (i_filter_corection .gt. 0) then
        if      (filter_correction_ctl .eq. 'on'                        &
     &      .or. filter_correction_ctl .eq. 'On'                        &
     &      .or. filter_correction_ctl .eq. 'ON') then
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
        if      (filter_fixed_point_ctl .eq. 'on'                       &
     &      .or. filter_fixed_point_ctl .eq. 'On'                       &
     &      .or. filter_fixed_point_ctl .eq. 'ON') then
          iflag_use_fixed_points = 1
        else
          iflag_use_fixed_points = 0
        end if
      else
        iflag_use_fixed_points = 0
      end if
!
      if (i_filter_negative_center .gt. 0) then
        if      (negative_center_ctl .eq. 'on'                          &
     &      .or. negative_center_ctl .eq. 'On'                          &
     &      .or. negative_center_ctl .eq. 'ON') then
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
        if      (momentum_type_ctl .eq. 'no_cross'                      &
     &      .or. momentum_type_ctl .eq. 'No_cross'                      &
     &      .or. momentum_type_ctl .eq. 'NO_CROSS') then
          iflag_momentum_type = 1
        else if (momentum_type_ctl .eq. 'normal'                        &
     &      .or. momentum_type_ctl .eq. 'Normal'                        &
     &      .or. momentum_type_ctl .eq. 'NORMAL') then
          iflag_momentum_type = 0
        else
          iflag_momentum_type = 0
        end if
      else
        iflag_momentum_type = 0
      end if
!
      if (i_ordering_list.gt.0 .and. iflag_tgt_filter_type.eq.0) then
        if      (ordering_list_ctl .eq. 'connection'                    &
     &      .or. ordering_list_ctl .eq. 'Connection'                    &
     &      .or. ordering_list_ctl .eq. 'CONNECTION') then
          iflag_ordering_list = 0
        else if (ordering_list_ctl .eq. 'distance'                      &
     &      .or. ordering_list_ctl .eq. 'Distance'                      &
     &      .or. ordering_list_ctl .eq. 'DISTANCE') then
          iflag_ordering_list = 1
        else if (ordering_list_ctl .eq. 'distance_ratio'                &
     &      .or. ordering_list_ctl .eq. 'Distance_ratio'                &
     &      .or. ordering_list_ctl .eq. 'DISTANCE_RATIO') then
          iflag_ordering_list = 2
        end if 
      else
        iflag_ordering_list = 0
      end if
!
!
      if (      mass_matrix_type_ctl .eq. 'consist'                     &
     &     .or. mass_matrix_type_ctl .eq. 'Consist'                     &
     &     .or. mass_matrix_type_ctl .eq. 'CONSIST' ) then
        itype_mass_matrix = 1
      else
        itype_mass_matrix = 0
      end if
!
!
      if (      solver_type_ctl .eq. 'iterative'                       &
     &     .or. solver_type_ctl .eq. 'Iterative'                       &
     &     .or. solver_type_ctl .eq. 'ITERATIVE' ) then
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
      method =      method_ctl
      precond =     precond_ctl
      itr =         itr_ctl
      eps =         eps_ctl
      sigma =       sigma_ctl
      sigma_diag =  sigma_diag_ctl
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
        call calypso_MPI_abort(100, e_message)
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
      if (i_mesh_header .ne. 0) then
        mesh_file_head = mesh_file_prefix
      else
        mesh_file_head = def_mesh_file_head
      end if
      if (iflag_debug.gt.0) write(*,*) 'mesh_file_head ', mesh_file_head
!
      if (i_filter_head_ctl .ne. 0) then
        filter_3d_head = filter_head_ctl
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_3d_head ', filter_3d_head
      end if
!
      if (i_filter_coef_head_ctl .ne. 0) then
        filter_coef_head = filter_coef_head_ctl
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_coef_head ', filter_coef_head
      end if
!
      if (i_filter_elen_head_ctl .ne. 0) then
        filter_elen_head = filter_elen_head_ctl
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_elen_head ', filter_elen_head
      end if
!
      if (i_filter_moms_head_ctl .ne. 0) then
        filter_moms_head = filter_moms_head_ctl
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_moms_head ', filter_moms_head
      end if
!
      omitted_ratio = omitted_ratio_ctl
!
!   set data format
!
      call choose_file_format(mesh_file_fmt_ctl, i_mesh_file_fmt,       &
     &          iflag_mesh_file_fmt)
!
      call choose_file_format(filter_3d_format, i_filter_3d_fmt,        &
     &          ifmt_3d_filter)
      call choose_file_format(filter_elen_format, i_filter_elen_fmt,    &
     &          ifmt_filter_elen)
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
      if (i_num_subdomain .ne. 0) then
        nprocs = num_subdomain_ctl
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
