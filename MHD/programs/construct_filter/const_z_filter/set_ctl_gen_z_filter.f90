!
!      module set_ctl_gen_z_filter
!
!     Written by H. Matsui on July, 2006
!
!      subroutine set_ctl_params_4_gen_z_filter
!
      module set_ctl_gen_z_filter
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_4_gen_z_filter
!
      use m_constants
      use m_machine_parameter
      use m_iccg_parameter
      use m_crs_matrix
      use m_commute_filter_z
      use m_ctl_data_4_plane_model
      use m_ctl_data_4_solvers
      use m_ctl_data_gen_filter
      use m_ctl_data_gen_z_filter
!
      use set_parallel_file_name
      use skip_comment_f
!
      integer(kind = kint) :: i
      real(kind = kreal) :: pi
!
!
      pi = four * atan(one)
!
      if (i_filter_head_ctl .ne. 0) then
        filter_z_file_head = filter_head_ctl
      else
        filter_z_file_head = 'filter_node_l.0'
      end if
      call add_dat_extension(filter_z_file_head, filter_z_file_name)
      write(*,*) 'filter_z_file_name ', filter_z_file_name
!
      if (i_ip_smp_p_ctl .ne. 0) then
        np_smp = ip_smp_p_ctl
      else
        np_smp = 1
      end if
      write(*,*) 'np_smp', np_smp
!
!    set plane layer parameters
!
      totalnod_x = nnod_plane_ctl%intvalue(1)
      totalnod_y = nnod_plane_ctl%intvalue(2)
      totalnod =   nnod_plane_ctl%intvalue(3)
!
      if(cmp_no_case(unit_len_plane_ctl%charavalue(1),'pi').gt. 0) then
        xsize = pi * plane_size_ctl%realvalue(1)
      else
        xsize = plane_size_ctl%realvalue(1)
      end if
!
      if(cmp_no_case(unit_len_plane_ctl%charavalue(2),'pi').gt. 0) then
        ysize = pi * plane_size_ctl%realvalue(2)
      else
        ysize = plane_size_ctl%realvalue(2)
      end if
!
      if(cmp_no_case(unit_len_plane_ctl%charavalue(3),'pi').gt. 0) then
        zsize = pi * plane_size_ctl%realvalue(3)
      else
        zsize = plane_size_ctl%realvalue(3)
      end if
!
      if      (cmp_no_case(horizontal_grid_ctl%charavalue,              &
     &                     'equi_distance' ) .gt. 0) then
        iflag_grid = 0
      else if (cmp_no_case(horizontal_grid_ctl%charavalue,              &
     &                     'half_chebyshev') .gt. 0) then
        iflag_grid = 1
      else if (cmp_no_case(horizontal_grid_ctl%charavalue,              &
     &                     'chebyshev'     ) .gt. 0) then
        iflag_grid = 2
      end if
!
!   set number of integration points
!
      if (i_num_int_points .ne. 0) then
        i_int_z_filter = num_int_points_ctl
      else
        i_int_z_filter = 6
      end if
      write(*,*) 'num_int_points', i_int_z_filter
!
!   set filter types
!
      if (reference_filter_ctl%icou .ne. 0) then
        num_filter_z = reference_filter_ctl%num
      else
        num_filter_z = 1
      end if
      if (horizontal_filter_ctl%icou .ne. 0) then
        num_filter_h = horizontal_filter_ctl%num
      else
        num_filter_h = 1
      end if
!
      type_filter_z = reference_filter_ctl%c_tbl(1)
      type_filter_h = horizontal_filter_ctl%c_tbl(1)
      f_width =   reference_filter_ctl%vect(1)
      f_width_h = horizontal_filter_ctl%vect(1)
!
      call deallocate_ref_filter_ctl
      call deallocate_horiz_filter_ctl
!
      if      (   type_filter_z .eq. 'tophat'                           &
     &       .or. type_filter_z .eq. 'Tophat'                           &
     &       .or. type_filter_z .eq. 'TOPHAT') then
       iflag_filter = 0
      else if (   type_filter_z .eq. 'linear'                           &
     &       .or. type_filter_z .eq. 'Linear'                           &
     &       .or. type_filter_z .eq. 'LINEAR') then
       iflag_filter = 1
      else if (   type_filter_z .eq. 'gaussian'                         &
     &       .or. type_filter_z .eq. 'Gaussian'                         &
     &       .or. type_filter_z .eq. 'GAUSSIAN') then
       iflag_filter = 2
     end if
!
      if      (   type_filter_h .eq. 'tophat'                           &
     &       .or. type_filter_h .eq. 'Tophat'                           &
     &       .or. type_filter_h .eq. 'TOPHAT') then
       iflag_filter_h = 0
      else if (   type_filter_h .eq. 'linear'                           &
     &       .or. type_filter_h .eq. 'Linear'                           &
     &       .or. type_filter_h .eq. 'LINEAR') then
       iflag_filter_h = 1
      else if (   type_filter_h .eq. 'gaussian'                         &
     &       .or. type_filter_h .eq. 'Gaussian'                         &
     &       .or. type_filter_h .eq. 'GAUSSIAN') then
       iflag_filter_h = 2
     end if
!
      write(*,*) 'iflag_filter', iflag_filter, iflag_filter_h
      write(*,*) 'width', f_width, f_width_h
!
      if (i_nele_filtering .ne. 0) then
        numfilter = num_ele_4_filter_ctl
      else
        numfilter = 2
      end if
!
!
      ncomp_norm = ref_filter_mom_ctl%num
      call allocate_z_filter_mom_params
!
      if (ncomp_norm.gt.0) then
        kcomp_norm(1:ncomp_norm)                                        &
     &                      = ref_filter_mom_ctl%ivec(1:ncomp_norm)
        f_mom(1:ncomp_norm) = ref_filter_mom_ctl%vect(1:ncomp_norm)
        filter_moment_type(1:ncomp_norm)                                &
     &                      = ref_filter_mom_ctl%c_tbl(1:ncomp_norm)
        write(*,*) 'kcomp_norm(i), f_mom(i), filter_moment_type(i)'
        do i = 1, ncomp_norm
          write(*,*) i, kcomp_norm(i), f_mom(i),                        &
     &              trim(filter_moment_type(i))
        end do
      end if
!
!     set solver information
!
      SOLVER_crs =  solver_type_ctl
!
      precond =    precond_ctl
      method =     method_ctl
      eps =        eps_ctl
      itr =        itr_ctl
      sigma =      sigma_ctl
      sigma_diag = sigma_diag_ctl
!
      METHOD_crs =       method
      PRECOND_crs =      precond
      INTARRAY_crs(1) =  itr
      REALARRAY_crs(1) = eps
      REALARRAY_crs(2) = sigma_diag
      REALARRAY_crs(3) = sigma
!
      if ( order_method_ctl .eq. 'RCM_DJDS') then 
        iflag_ordering = 1
        mc_color = 0
        if (i_min_color.eq.0) then
          min_color = 0
        else
          min_color = min_color_ctl
        end if
      else if  ( order_method_ctl .eq. 'MC_DJDS') then
        iflag_ordering = 2
        if (i_mc_color.eq.0) then
          mc_color = 0
        else
          mc_color = mc_color_ctl
        end if
        min_color = 0
      end if
!
      write(*,*) 'iflag_ordering', iflag_ordering
      write(*,*) 'min_color', min_color
      write(*,*) 'mc_color', mc_color
!
      end subroutine set_ctl_params_4_gen_z_filter
!
!   --------------------------------------------------------------------
!
      end module set_ctl_gen_z_filter
