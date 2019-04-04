!
!      module set_ctl_gen_filter
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine set_controls_gen_3dfilter(filter3d_ctl, FEM_elens,   &
!!     &          mesh_file, gfil_p, newfil_p, ref_m, f_matrices)
!!      subroutine set_controls_sort_3dfilter                           &
!!     &         (filter3d_ctl, mesh_file, gfil_p, num_pe)
!!        type(ctl_data_gen_3d_filter), intent(in) :: filter3d_ctl
!!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!!        type(ctl_param_newdom_filter), intent(inout) :: newfil_p
!!        type(matrices_4_filter), intent(inout) :: f_matrices
!!        type(reference_moments), intent(inout) :: ref_m
!
      module set_ctl_gen_filter
!
      use m_precision
      use m_machine_parameter
      use t_file_IO_parameter
      use t_ctl_data_gen_filter
      use t_ctl_data_gen_3d_filter
      use t_ctl_params_4_gen_filter
      use m_ctl_param_newdom_filter
!
      implicit none
!
      private :: set_ctl_params_gen_filter
      private :: set_file_heads_3d_comm_filter
      private :: set_numdomain_3d_comm_filter
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_controls_gen_3dfilter(filter3d_ctl, FEM_elens,     &
     &          mesh_file, gfil_p, newfil_p, ref_m, f_matrices)
!
      use t_matrix_4_filter
      use t_reference_moments
      use t_filter_elength
      use t_ctl_param_newdom_filter
      use set_control_platform_data
!
      type(ctl_data_gen_3d_filter), intent(in) :: filter3d_ctl
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(field_IO_params), intent(inout) ::  mesh_file
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(ctl_param_newdom_filter), intent(inout) :: newfil_p
      type(reference_moments), intent(inout) :: ref_m
      type(matrices_4_filter), intent(inout) :: f_matrices
!
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_gen_filter'
      call set_control_mesh_def                                         &
     &   (filter3d_ctl%fil3_ctl%gen_filter_plt, mesh_file)
      call set_file_heads_3d_comm_filter                                &
     &   (filter3d_ctl%gen_f_ctl, filter3d_ctl%fil3_ctl%ffile_3d_ctl,   &
     &    gfil_p)
      call set_ctl_params_gen_filter                                    &
     &   (filter3d_ctl%gen_f_ctl, filter3d_ctl%fil3_ctl,                &
     &    filter3d_ctl%org_fil_files_ctl,                               &
     &    newfil_p%org_filter_coef_head,                                &
     &    gfil_p, FEM_elens, f_matrices%fil_mat_crs, ref_m)
!
      end subroutine set_controls_gen_3dfilter
!
!   --------------------------------------------------------------------
!
      subroutine set_controls_sort_3dfilter                             &
     &         (filter3d_ctl, mesh_file, gfil_p, num_pe)
!
      use set_control_platform_data
!
      type(ctl_data_gen_3d_filter), intent(in) :: filter3d_ctl
      type(field_IO_params), intent(inout) ::  mesh_file
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!
      integer, intent(inout) :: num_pe
!
!
      call set_control_mesh_def                                         &
     &   (filter3d_ctl%fil3_ctl%gen_filter_plt, mesh_file)
      call set_file_heads_3d_comm_filter                                &
     &   (filter3d_ctl%gen_f_ctl, filter3d_ctl%fil3_ctl%ffile_3d_ctl,   &
     &    gfil_p)
      call set_numdomain_3d_comm_filter                                 &
     &   (filter3d_ctl%fil3_ctl, num_pe)
!
      end subroutine set_controls_sort_3dfilter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_gen_filter(gen_f_ctl, fil3_ctl,         &
     &          org_fil_files_ctl, org_filter_coef_head, gfil_p,        &
     &          FEM_elens, fil_mat_crs, ref_m)
!
      use calypso_mpi
      use m_error_IDs
!
      use t_reference_moments
      use t_ctl_data_3d_filter
      use t_filter_elength
      use t_crs_matrix
!
      use skip_comment_f
!
      type(ctl_data_gen_filter), intent(in) :: gen_f_ctl
      type(ctl_data_3d_filter), intent(in) :: fil3_ctl
      type(org_filter_prefix_ctls), intent(in) :: org_fil_files_ctl
!
      character(len=kchara), intent(inout) :: org_filter_coef_head
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(CRS_matrix), intent(inout) :: fil_mat_crs
      type(reference_moments), intent(inout) :: ref_m
!
      integer(kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!
      np_smp = 1
      if(fil3_ctl%gen_filter_plt%num_smp_ctl%iflag .gt. 0) then
         np_smp = fil3_ctl%gen_filter_plt%num_smp_ctl%intvalue
      end if
!
      gfil_p%num_int_points = 4
      if(gen_f_ctl%num_int_points_ctl%iflag .ne. 0) then
        gfil_p%num_int_points = gen_f_ctl%num_int_points_ctl%intvalue
      end if
!
      gfil_p%minimum_comp = 11
      if(gen_f_ctl%minimum_comp_ctl%iflag .gt. 0) then
        gfil_p%minimum_comp = gen_f_ctl%minimum_comp_ctl%intvalue
      end if
!
      gfil_p%num_filtering_grp = fil3_ctl%filter_area_ctl%num
      if (iflag_debug.gt.0) then
        write(*,*) 'np_smp', np_smp
        write(*,*) 'num_int_points', gfil_p%num_int_points
        write(*,*) 'minimum_comp', gfil_p%minimum_comp
        write(*,*) 'num_filtering_grp', gfil_p%num_filtering_grp
      end if
!
!
      if(gfil_p%num_filtering_grp .eq. 0) then
        gfil_p%num_filtering_grp = 1
        call alloc_ref_filter_type(gfil_p)
        gfil_p%filter_area_name(1) = 'all'
      else if(gfil_p%num_filtering_grp .gt. 0) then
        call alloc_ref_filter_area(gfil_p)
        gfil_p%filter_area_name(1:gfil_p%num_filtering_grp)             &
     &     = fil3_ctl%filter_area_ctl%c_tbl(1:gfil_p%num_filtering_grp)
      end if
!
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'num_filtering_grp ', gfil_p%num_filtering_grp
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'filter_area_name ', gfil_p%filter_area_name
!
!
      gfil_p%num_ref_filter = gen_f_ctl%reference_filter_ctl%num
      FEM_elens%filter_conf%nf_type = gfil_p%num_ref_filter
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'num_ref_filter', gfil_p%num_ref_filter
!
      if (gfil_p%num_ref_filter .gt. 0) then
        call alloc_ref_filter_type(gfil_p)
        call alloc_ref_1d_mom_type(FEM_elens%filter_conf)
        
        do i = 1, gfil_p%num_ref_filter
          gfil_p%iref_filter_type(i) = iflag_tophat_filter
          tmpchara = gen_f_ctl%reference_filter_ctl%c_tbl(i)
          if(cmp_no_case(tmpchara, 'Gaussian')) then
            gfil_p%iref_filter_type(i) = iflag_gaussian_filter
          else if(cmp_no_case(tmpchara, 'Linear')) then
            gfil_p%iref_filter_type(i) = iflag_linear_filter
          else if(cmp_no_case(tmpchara, 'Tophat')) then
            gfil_p%iref_filter_type(i) = iflag_tophat_filter
          end if
!
          FEM_elens%filter_conf%filter_type(i)                          &
     &          = gen_f_ctl%reference_filter_ctl%c_tbl(i)
!
          gfil_p%ref_filter_width(i)                                    &
     &          = gen_f_ctl%reference_filter_ctl%vect(i)
          FEM_elens%filter_conf%f_width(i) = gfil_p%ref_filter_width(i)
        end do
!
        if (iflag_debug.gt.0)  then
          write(*,*) 'iref_filter_type', gfil_p%iref_filter_type
          write(*,*) 'ref_filter_width', gfil_p%ref_filter_width
        end if
!
      end if
!
      gfil_p%num_moments_order = gen_f_ctl%ref_filter_mom_ctl%num
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'num_moments_order', gfil_p%num_moments_order
!
      if(gfil_p%num_moments_order .gt. 0) then
        call alloc_moment_parameter(gfil_p)
!
        do i = 1, gfil_p%num_moments_order
          gfil_p%mom_order(i) = gen_f_ctl%ref_filter_mom_ctl%ivec(i)
          gfil_p%mom_value(i) = gen_f_ctl%ref_filter_mom_ctl%vect(i)
        end do
!
        ref_m%max_num_order_1d = gfil_p%mom_order(1)
        do i = 2, gfil_p%num_moments_order
          ref_m%max_num_order_1d                                        &
     &       = max(ref_m%max_num_order_1d,gfil_p%mom_order(i))
        end do
        ref_m%max_num_order_3d =  (ref_m%max_num_order_1d + 1)**3
        ref_m%num_order_3d = gfil_p%num_moments_order**3
        ref_m%num_order_1d = gfil_p%num_moments_order
!
        do i = 1, gfil_p%num_moments_order
          tmpchara = gen_f_ctl%ref_filter_mom_ctl%c_tbl(i)
          if(cmp_no_case(tmpchara, 'refered')) then
            gfil_p%iref_mom_type(i) = 1
          else
            gfil_p%iref_mom_type(i) = 0
          end if
        end do
!
      if (iflag_debug.gt.0)  then
          write(*,*) 'mom_order',     gfil_p%mom_order
          write(*,*) 'mom_value',     gfil_p%mom_value
          write(*,*) 'iref_mom_type', gfil_p%iref_mom_type
        end if
      end if
!
      if(gen_f_ctl%minimum_det_ctl%iflag .gt. 0) then
        gfil_p%minimum_det_mat = gen_f_ctl%minimum_det_ctl%realvalue
      end if
!
      if(gen_f_ctl%maximum_neighbour_ctl%iflag .gt. 0) then
        gfil_p%maximum_neighbour                                        &
     &         = gen_f_ctl%maximum_neighbour_ctl%intvalue
      else
        gfil_p%maximum_neighbour = 2
      end if
!
      if (gen_f_ctl%tgt_filter_type_ctl%iflag .gt. 0) then
        tmpchara = gen_f_ctl%tgt_filter_type_ctl%charavalue
        if     (cmp_no_case(tmpchara,'TOPHAT')) then
          gfil_p%iflag_tgt_filter_type = iflag_tophat
        else if(cmp_no_case(tmpchara,'LINEAR')) then
          gfil_p%iflag_tgt_filter_type = iflag_linear
        else if(cmp_no_case(tmpchara,'GAUSSIAN')) then
          gfil_p%iflag_tgt_filter_type = iflag_gaussian
        else if(cmp_no_case(tmpchara,'COMMUTATIVE')) then
          gfil_p%iflag_tgt_filter_type = iflag_commutative
        else if(cmp_no_case(tmpchara,'NONE')) then
          gfil_p%iflag_tgt_filter_type = iflag_no_filter
        else if(cmp_no_case(tmpchara,'NO')) then
          gfil_p%iflag_tgt_filter_type = iflag_no_filter
        else
          gfil_p%iflag_tgt_filter_type = 0
        end if
      else
        gfil_p%iflag_tgt_filter_type = 0
      end if
!
      if(gen_f_ctl%filter_correction_ctl%iflag .gt. 0) then
        if(yes_flag(gen_f_ctl%filter_correction_ctl%charavalue)) then
          if (gfil_p%iflag_tgt_filter_type .gt. 0) then
            gfil_p%iflag_tgt_filter_type                                &
     &              = - gfil_p%iflag_tgt_filter_type
          end if
        end if
      end if
!
!
      org_filter_coef_head = "org/filter_coef"
      if (gfil_p%iflag_tgt_filter_type .lt. iflag_undefined) then
        if(org_fil_files_ctl%org_filter_coef_head_ctl%iflag .gt. 0) then
          org_filter_coef_head                                          &
     &         = org_fil_files_ctl%org_filter_coef_head_ctl%charavalue
        else
          write(*,*) 'set original filter coefs file name'
        end if
      end if
!
!
      if(gen_f_ctl%filter_fixed_point_ctl%iflag .gt. 0) then
        if(yes_flag(gen_f_ctl%filter_fixed_point_ctl%charavalue)) then
          gfil_p%iflag_use_fixed_points = 1
        else
          gfil_p%iflag_use_fixed_points = 0
        end if
      else
        gfil_p%iflag_use_fixed_points = 0
      end if
!
      if(gen_f_ctl%negative_center_ctl%iflag .gt. 0) then
        if(yes_flag(gen_f_ctl%negative_center_ctl%charavalue)) then
          gfil_p%iflag_negative_center = 0
        else
          gfil_p%iflag_negative_center = 1
        end if
      else
        gfil_p%iflag_negative_center = 1
      end if
!
      if(gen_f_ctl%ilevel_filter_error_info%iflag .gt. 0) then
        gfil_p%iflag_err_level_filter                                   &
     &      = gen_f_ctl%ilevel_filter_error_info%intvalue
      else
        gfil_p%iflag_err_level_filter = 0
      end if
!
      if(gen_f_ctl%maximum_rms_ctl%iflag .gt. 0) then
        gfil_p%max_rms_weight_limit                                     &
     &          = gen_f_ctl%maximum_rms_ctl%realvalue
      end if
!
      if(gen_f_ctl%momentum_type_ctl%iflag .gt. 0) then
        tmpchara = gen_f_ctl%momentum_type_ctl%charavalue
        if     (cmp_no_case(tmpchara,'NO_CROSS')) then
          gfil_p%iflag_momentum_type = 1
        else if(cmp_no_case(tmpchara,'NORMAL')) then
          gfil_p%iflag_momentum_type = 0
        else
          gfil_p%iflag_momentum_type = 0
        end if
      else
        gfil_p%iflag_momentum_type = 0
      end if
!
      if(gen_f_ctl%ordering_list_ctl%iflag .gt. 0                       &
     &  .and. gfil_p%iflag_tgt_filter_type .eq. iflag_undefined) then
        tmpchara = gen_f_ctl%ordering_list_ctl%charavalue
        if     (cmp_no_case(tmpchara,'CONNECTION')) then
          gfil_p%iflag_ordering_list = 0
        else if(cmp_no_case(tmpchara,'DISTANCE')) then
          gfil_p%iflag_ordering_list = 1
        else if(cmp_no_case(tmpchara,'DISTANCE_RATIO')) then
          gfil_p%iflag_ordering_list = 2
        end if 
      else
        gfil_p%iflag_ordering_list = 0
      end if
!
!
      gfil_p%itype_mass_matrix = 0
      if(fil3_ctl%mass_matrix_type_ctl%iflag .gt. 0) then
        tmpchara = fil3_ctl%mass_matrix_type_ctl%charavalue
        if(cmp_no_case(tmpchara, 'CONSIST')) then
          gfil_p%itype_mass_matrix = 1
        end if
      end if
!
!
      gfil_p%id_solver_type = 0
      if(gen_f_ctl%f_solver_type_ctl%iflag .gt. 0) then
        tmpchara = gen_f_ctl%f_solver_type_ctl%charavalue
        if(cmp_no_case(tmpchara,'ITERATIVE')) gfil_p%id_solver_type = 1
      end if
!
      gfil_p%inod_start_filter = 1
      if(gen_f_ctl%start_node_ctl%iflag .gt. 0) then
        gfil_p%inod_start_filter = gen_f_ctl%start_node_ctl%intvalue
      end if
!
      gfil_p%inod_end_filter = -1
      if(gen_f_ctl%end_node_ctl%iflag .gt. 0) then
        gfil_p%inod_end_filter = gen_f_ctl%end_node_ctl%intvalue
      end if
!
      gfil_p%ist_num_free = -1
      if(gen_f_ctl%ist_num_free_ctl%iflag .gt. 0) then
        gfil_p%ist_num_free = gen_f_ctl%ist_num_free_ctl%intvalue
      end if
!
!
      gfil_p%ied_num_free = -1
      if(gen_f_ctl%ied_num_free_ctl%iflag .gt. 0) then
        gfil_p%ied_num_free = gen_f_ctl%ied_num_free_ctl%intvalue
      end if
!
      if(gen_f_ctl%CG_filter_ctl%method_ctl%iflag .gt. 0)  then
        fil_mat_crs%METHOD_crs                                          &
     &        =  gen_f_ctl%CG_filter_ctl%method_ctl%charavalue
      end if
      if(gen_f_ctl%CG_filter_ctl%precond_ctl%iflag .gt. 0) then
        fil_mat_crs%PRECOND_crs                                         &
     &        = gen_f_ctl%CG_filter_ctl%precond_ctl%charavalue
      end if
      if(gen_f_ctl%CG_filter_ctl%itr_ctl%iflag .gt. 0)        then
        gfil_p%itr = gen_f_ctl%CG_filter_ctl%itr_ctl%intvalue
      end if
      if(gen_f_ctl%CG_filter_ctl%eps_ctl%iflag .gt. 0)        then
        gfil_p%eps = gen_f_ctl%CG_filter_ctl%eps_ctl%realvalue
      end if
      if(gen_f_ctl%CG_filter_ctl%sigma_ctl%iflag .gt. 0)      then
        gfil_p%sigma = gen_f_ctl%CG_filter_ctl%sigma_ctl%realvalue
      end if
      if(gen_f_ctl%CG_filter_ctl%sigma_diag_ctl%iflag .gt. 0) then
        gfil_p%sigma_diag                                               &
     &        =  gen_f_ctl%CG_filter_ctl%sigma_diag_ctl%realvalue
      end if
!
!
      gfil_p%method_elesize =     'GPBiCG'
      gfil_p%precond_elesize =    'DIAG'
      gfil_p%itr_elesize =         20000
      gfil_p%eps_elesize =         1.0d-15
      gfil_p%sigma_elesize =       1.0d0
      gfil_p%sigma_diag_elesize  = 1.0d0
!
      if(fil3_ctl%method_esize_ctl%iflag .gt. 0) then
        gfil_p%method_elesize = fil3_ctl%method_esize_ctl%charavalue
      end if
      if(fil3_ctl%precond_esize_ctl%iflag .gt. 0) then
        gfil_p%precond_elesize = fil3_ctl%precond_esize_ctl%charavalue
      end if
      if(fil3_ctl%itr_esize_ctl%iflag .gt. 0) then
        gfil_p%itr_elesize = fil3_ctl%itr_esize_ctl%intvalue
      end if
      if(fil3_ctl%eps_esize_ctl%iflag .gt. 0) then
        gfil_p%eps_elesize = fil3_ctl%eps_esize_ctl%realvalue
      end if
      if(fil3_ctl%sigma_esize_ctl%iflag .gt. 0) then
        gfil_p%sigma_elesize = fil3_ctl%sigma_esize_ctl%realvalue
      end if
      if(fil3_ctl%sigma_diag_esize_ctl%iflag .gt. 0) then
        gfil_p%sigma_diag_elesize                                       &
     &     = fil3_ctl%sigma_diag_esize_ctl%realvalue
      end if
!
      if (iflag_debug.gt.0) then
          write(*,*) 'id_solver_type ', gfil_p%id_solver_type
          write(*,*) 'method ',  fil_mat_crs%METHOD_crs
          write(*,*) 'precond ', fil_mat_crs%PRECOND_crs
          write(*,*) 'itr ',        gfil_p%itr
          write(*,*) 'eps ',        gfil_p%eps
          write(*,*) 'sigma ',      gfil_p%sigma
          write(*,*) 'sigma_diag ', gfil_p%sigma_diag
        end if
!
      if (gfil_p%iflag_tgt_filter_type .eq. -iflag_commutative          &
     &  .and. org_fil_files_ctl%org_filter_coef_head_ctl%iflag .eq. 0)  &
     &  then
         e_message = "set original filter coefficient datafile"
         call calypso_MPI_abort(ierr_file, e_message)
      end if
!
      end subroutine set_ctl_params_gen_filter
!
!  ---------------------------------------------------------------------
!
      subroutine set_file_heads_3d_comm_filter                          &
     &         (gen_f_ctl, ffile_ctl, gfil_p)
!
      use t_ctl_data_filter_files
      use m_file_format_switch
      use m_filter_file_names
!
      type(ctl_data_gen_filter), intent(in) :: gen_f_ctl
      type(filter_file_control), intent(in) :: ffile_ctl
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!
!
      if (ffile_ctl%filter_head_ctl%iflag .ne. 0) then
        filter_3d_head = ffile_ctl%filter_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_3d_head ', filter_3d_head
      end if
!
      if (ffile_ctl%filter_coef_head_ctl%iflag .ne. 0) then
        filter_coef_head = ffile_ctl%filter_coef_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_coef_head ', filter_coef_head
      end if
!
      if (ffile_ctl%filter_elen_head_ctl%iflag .ne. 0) then
        filter_elen_head = ffile_ctl%filter_elen_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_elen_head ', filter_elen_head
      end if
!
      if (ffile_ctl%filter_moms_head_ctl%iflag .ne. 0) then
        filter_moms_head = ffile_ctl%filter_moms_head_ctl%charavalue
        if (iflag_debug.gt.0)                                           &
     &     write(*,*) 'filter_moms_head ', filter_moms_head
      end if
!
      gfil_p%omitted_ratio = 1.0d-30
      if(gen_f_ctl%omitted_ratio_ctl%iflag .gt. 0) then
        gfil_p%omitted_ratio = gen_f_ctl%omitted_ratio_ctl%realvalue
      end if
!
!   set data format
!
      ifmt_3d_filter = choose_file_format(ffile_ctl%filter_3d_format)
      ifmt_filter_elen                                                  &
     &     = choose_file_format(ffile_ctl%filter_elen_format)
      ifmt_filter_moms = ifmt_filter_elen
!
      end subroutine set_file_heads_3d_comm_filter
!
!  ---------------------------------------------------------------------
!
      subroutine set_numdomain_3d_comm_filter(fil3_ctl, num_pe)
!
      type(ctl_data_3d_filter), intent(in) :: fil3_ctl
      integer, intent(inout) :: num_pe
!
!
      if(fil3_ctl%gen_filter_plt%ndomain_ctl%iflag .ne. 0) then
        num_pe = int(fil3_ctl%gen_filter_plt%ndomain_ctl%intvalue)
      else
        write(*,*) 'set number of domains'
        stop
      end if
      write(*,*) 'number of subdomain: ', num_pe
!
      end subroutine set_numdomain_3d_comm_filter
!
!  ---------------------------------------------------------------------
!
      end module set_ctl_gen_filter
