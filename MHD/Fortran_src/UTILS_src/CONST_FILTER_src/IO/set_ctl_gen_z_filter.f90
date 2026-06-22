!>@file   set_ctl_gen_z_filter.f90
!!        module set_ctl_gen_z_filter
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!
!>@brief Set commutive filter parameters in vertical direction
!!
!!@verbatim
!!      subroutine set_ctl_params_4_gen_z_filter(z_filter_ctl,          &
!!     &          z_commute, mat_crs, CG_param, DJDS_param)
!!        type(ctl_data_gen_z_filter), intent(in) :: z_filter_ctl
!!        type(CRS_matrix), intent(inout) :: mat_crs
!!        type(CG_poarameter), intent(inout) :: CG_param
!!        type(DJDS_poarameter), intent(inout) :: DJDS_param
!!@endverbatim
!
      module set_ctl_gen_z_filter
!
      use m_precision
      use t_ctl_data_gen_z_filter
      use t_commute_filter_z
      use t_crs_matrix
      use t_iccg_parameter
!
      implicit none
!
      private :: set_ctl_parameters_z_filter
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_4_gen_z_filter(z_filter_ctl,            &
     &          z_commute, mat_crs, CG_param, DJDS_param)
!
      use m_constants
      use m_machine_parameter
      use m_commute_filter_z
!
      use set_parallel_file_name
!
      type(ctl_data_gen_z_filter), intent(in) :: z_filter_ctl
!
      type(vart_filter_moments), intent(inout) :: z_commute
      type(CRS_matrix), intent(inout) :: mat_crs
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_poarameter), intent(inout) :: DJDS_param
!
!
      if (z_filter_ctl%z_filter_head_ctl%iflag .ne. 0) then
        filter_z_file_head = z_filter_ctl%z_filter_head_ctl%charavalue
      else
        filter_z_file_head = 'filter_node_l.0'
      end if
      filter_z_file_name = add_dat_extension(filter_z_file_head)
      write(*,*) 'filter_z_file_name ', filter_z_file_name
!
      if(z_filter_ctl%ip_smp_z_ctl%iflag .ne. 0) then
        np_smp = z_filter_ctl%ip_smp_z_ctl%intvalue
      else
        np_smp = 1
      end if
      write(*,*) 'np_smp', np_smp
!
      call set_ctl_parameters_z_filter                                  &
     &   (z_filter_ctl%cube_c, z_filter_ctl%gen_f_ctl,                  &
     &    z_commute, mat_crs, CG_param, DJDS_param)
!
      end subroutine set_ctl_params_4_gen_z_filter
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_parameters_z_filter(cube_c, gen_f_ctl,         &
     &          z_commute, mat_crs, CG_param, DJDS_param)
!
      use m_constants
      use m_machine_parameter
      use m_commute_filter_z
      use m_spheric_constants
      use t_ctl_data_4_plane_model
      use t_ctl_data_gen_filter
!
      use set_parallel_file_name
      use skip_comment_f
!
      type(ctl_data_4_plane_model), intent(in) :: cube_c
      type(ctl_data_gen_filter), intent(in) :: gen_f_ctl
!
      type(vart_filter_moments), intent(inout) :: z_commute
      type(CRS_matrix), intent(inout) :: mat_crs
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_poarameter), intent(inout) :: DJDS_param
!
      real(kind = kreal) :: pi
      character(len = kchara) :: tmpchara
!
!
      pi = four * atan(one)
!
!    set plane layer parameters
!
      totalnod_x = cube_c%nnod_plane_ctl%intvalue(1)
      totalnod_y = cube_c%nnod_plane_ctl%intvalue(2)
      totalnod =   cube_c%nnod_plane_ctl%intvalue(3)
!
      tmpchara = cube_c%unit_len_plane_ctl%charavalue(1)
      if(cmp_no_case(tmpchara,'pi')) then
        xsize = pi * cube_c%plane_size_ctl%realvalue(1)
      else
        xsize = cube_c%plane_size_ctl%realvalue(1)
      end if
!
      tmpchara = cube_c%unit_len_plane_ctl%charavalue(2)
      if(cmp_no_case(tmpchara,'pi')) then
        ysize = pi * cube_c%plane_size_ctl%realvalue(2)
      else
        ysize = cube_c%plane_size_ctl%realvalue(2)
      end if
!
      tmpchara = cube_c%unit_len_plane_ctl%charavalue(3)
      if(cmp_no_case(tmpchara,'pi')) then
        zsize = pi * cube_c%plane_size_ctl%realvalue(3)
      else
        zsize = cube_c%plane_size_ctl%realvalue(3)
      end if
!
      if      (cmp_no_case(cube_c%horizontal_grid_ctl%charavalue,       &
     &                     label_equi     )) then
        iflag_grid = igrid_equidistance
      else if (cmp_no_case(cube_c%horizontal_grid_ctl%charavalue,       &
     &                     label_half_Cbyv)) then
        iflag_grid = igrid_half_Chebyshev
      else if (cmp_no_case(cube_c%horizontal_grid_ctl%charavalue,       &
     &                     label_Chebyshev)) then
        iflag_grid = igrid_Chebyshev
      end if
!
!   set number of integration points
!
      if(gen_f_ctl%num_int_points_ctl%iflag .ne. 0) then
        i_int_z_filter = gen_f_ctl%num_int_points_ctl%intvalue
      else
        i_int_z_filter = 6
      end if
      write(*,*) 'i_int_z_filter', i_int_z_filter
!
!   set filter types
!
      if(gen_f_ctl%reference_filter_ctl%icou .ne. 0) then
        num_filter_z = gen_f_ctl%reference_filter_ctl%num
      else
        num_filter_z = 1
      end if
      if(gen_f_ctl%horizontal_filter_ctl%icou .ne. 0) then
        num_filter_h = gen_f_ctl%horizontal_filter_ctl%num
      else
        num_filter_h = 1
      end if
!
      type_filter_z = gen_f_ctl%reference_filter_ctl%c_tbl(1)
      type_filter_h = gen_f_ctl%horizontal_filter_ctl%c_tbl(1)
      f_width =   gen_f_ctl%reference_filter_ctl%vect(1)
      f_width_h = gen_f_ctl%horizontal_filter_ctl%vect(1)
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
      if(gen_f_ctl%num_ele_4_filter_ctl%iflag .ne. 0) then
        numfilter = gen_f_ctl%num_ele_4_filter_ctl%intvalue
      else
        numfilter = 2
      end if
!
!
      call alloc_z_filter_mom_params(gen_f_ctl%ref_filter_mom_ctl%num,  &
     &                               z_commute)
!
      if(z_commute%ncomp_norm .gt. .0) then
        z_commute%kcomp_norm_z(1:z_commute%ncomp_norm)                  &
     &     = gen_f_ctl%ref_filter_mom_ctl%ivec(1:z_commute%ncomp_norm)
        z_commute%f_mom_z(1:z_commute%ncomp_norm)                       &
     &     = gen_f_ctl%ref_filter_mom_ctl%vect(1:z_commute%ncomp_norm)
        z_commute%z_filter_mom_type(1:z_commute%ncomp_norm)             &
     &     = gen_f_ctl%ref_filter_mom_ctl%c_tbl(1:z_commute%ncomp_norm)
      end if
      call write_vart_filter_moments(isix, z_commute)
!
!     set solver information
!
      mat_crs%SOLVER_crs = 'CRS'
      if(gen_f_ctl%f_solver_type_ctl%iflag .gt. 0) then
        mat_crs%SOLVER_crs =  gen_f_ctl%f_solver_type_ctl%charavalue
      end if
!
      call set_control_4_CG_solver(gen_f_ctl%CG_filter_ctl, CG_param)
      call set_control_4_DJDS_solver                                    &
     &   (gen_f_ctl%CG_filter_ctl%DJDS_ctl, DJDS_param)
!
      call copy_from_iccg_parameter(CG_param, mat_crs)
!
      end subroutine set_ctl_parameters_z_filter
!
!   --------------------------------------------------------------------
!
      end module set_ctl_gen_z_filter
