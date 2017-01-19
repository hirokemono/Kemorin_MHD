!
!      module set_ctl_gen_z_filter
!
!     Written by H. Matsui on July, 2006
!
!      subroutine set_ctl_params_4_gen_z_filter(mat_crs)
!
      module set_ctl_gen_z_filter
!
      use m_precision
      use t_crs_matrix
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_4_gen_z_filter(mat_crs)
!
      use m_constants
      use m_machine_parameter
      use m_iccg_parameter
      use m_commute_filter_z
      use m_ctl_data_4_plane_model
      use m_ctl_data_gen_filter
      use m_ctl_data_gen_z_filter
      use m_spheric_constants
!
      use set_parallel_file_name
      use skip_comment_f
!
      type(CRS_matrix), intent(inout) :: mat_crs
!
      integer(kind = kint) :: i
      real(kind = kreal) :: pi
!
!
      pi = four * atan(one)
!
      if (z_filter_head_ctl%iflag .ne. 0) then
        filter_z_file_head = z_filter_head_ctl%charavalue
      else
        filter_z_file_head = 'filter_node_l.0'
      end if
      call add_dat_extension(filter_z_file_head, filter_z_file_name)
      write(*,*) 'filter_z_file_name ', filter_z_file_name
!
      if (ip_smp_z_ctl%iflag .ne. 0) then
        np_smp = ip_smp_z_ctl%intvalue
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
      if(cmp_no_case(unit_len_plane_ctl%charavalue(1),'pi')) then
        xsize = pi * plane_size_ctl%realvalue(1)
      else
        xsize = plane_size_ctl%realvalue(1)
      end if
!
      if(cmp_no_case(unit_len_plane_ctl%charavalue(2),'pi')) then
        ysize = pi * plane_size_ctl%realvalue(2)
      else
        ysize = plane_size_ctl%realvalue(2)
      end if
!
      if(cmp_no_case(unit_len_plane_ctl%charavalue(3),'pi')) then
        zsize = pi * plane_size_ctl%realvalue(3)
      else
        zsize = plane_size_ctl%realvalue(3)
      end if
!
      if      (cmp_no_case(horizontal_grid_ctl%charavalue,              &
     &                     label_equi     )) then
        iflag_grid = igrid_equidistance
      else if (cmp_no_case(horizontal_grid_ctl%charavalue,              &
     &                     label_half_Cbyv)) then
        iflag_grid = igrid_half_Chebyshev
      else if (cmp_no_case(horizontal_grid_ctl%charavalue,              &
     &                     label_Chebyshev)) then
        iflag_grid = igrid_Chebyshev
      end if
!
!   set number of integration points
!
      if (num_int_points_ctl%iflag .ne. 0) then
        i_int_z_filter = num_int_points_ctl%intvalue
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
      call dealloc_control_array_c_r(reference_filter_ctl)
      call dealloc_control_array_c_r(horizontal_filter_ctl)
!
      if      (cmp_no_case(type_filter_z, 'tophat')) then
       iflag_filter = 0
      else if (cmp_no_case(type_filter_z, 'linear')) then
       iflag_filter = 1
      else if (cmp_no_case(type_filter_z, 'gaussian')) then
       iflag_filter = 2
      end if
!
      if      (cmp_no_case(type_filter_h, 'tophat')) then
       iflag_filter_h = 0
      else if (cmp_no_case(type_filter_h, 'linear')) then
       iflag_filter_h = 1
      else if (cmp_no_case(type_filter_h, 'gaussian')) then
       iflag_filter_h = 2
     end if
!
      write(*,*) 'iflag_filter', iflag_filter, iflag_filter_h
      write(*,*) 'width', f_width, f_width_h
!
      if (num_ele_4_filter_ctl%iflag .ne. 0) then
        numfilter = num_ele_4_filter_ctl%intvalue
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
      mat_crs%SOLVER_crs = 'CRS'
      if(f_solver_type_ctl%iflag .gt. 0) then
        mat_crs%SOLVER_crs =  f_solver_type_ctl%charavalue
      end if
!
      if(CG_filter_ctl%precond_ctl%iflag .gt. 0) then
        precond = CG_filter_ctl%precond_ctl%charavalue
      end if
      if(CG_filter_ctl%method_ctl%iflag .gt. 0)  then
        method =  CG_filter_ctl%method_ctl%charavalue
      end if
      if(CG_filter_ctl%eps_ctl%iflag .gt. 0)        then
        eps = CG_filter_ctl%eps_ctl%realvalue
      end if
      if(CG_filter_ctl%itr_ctl%iflag .gt. 0)        then
        itr = CG_filter_ctl%itr_ctl%intvalue
      end if
      if(CG_filter_ctl%sigma_ctl%iflag .gt. 0)      then
        sigma = CG_filter_ctl%sigma_ctl%realvalue
      end if
      if(CG_filter_ctl%sigma_diag_ctl%iflag .gt. 0) then
        sigma_diag =  CG_filter_ctl%sigma_diag_ctl%realvalue
      end if
!
      mat_crs%METHOD_crs =       method
      mat_crs%PRECOND_crs =      precond
      mat_crs%INTARRAY_crs(1) =  itr
      mat_crs%REALARRAY_crs(1) = eps
      mat_crs%REALARRAY_crs(2) = sigma_diag
      mat_crs%REALARRAY_crs(3) = sigma
!
      call set_control_4_DJDS_solver(CG_filter_ctl%DJDS_ctl)
!
      end subroutine set_ctl_params_4_gen_z_filter
!
!   --------------------------------------------------------------------
!
      end module set_ctl_gen_z_filter
