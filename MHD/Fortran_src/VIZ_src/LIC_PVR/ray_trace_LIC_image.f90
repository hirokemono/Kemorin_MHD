!>@file   ray_trace_LIC_image.f90
!!@brief  module ray_trace_LIC_image
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine ray_trace_each_lic_image(node, ele, surf,            &
!!     &          lic_p, pvr_screen, field_lic, draw_param, color_param,&
!!     &          viewpoint_vec, ray_vec4, num_pvr_ray, id_pixel_check, &
!!     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,    &
!!     &          xx4_pvr_start, xx4_pvr_ray_start, rgba_ray)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_projected_position), intent(in) :: pvr_screen
!!@endverbatim
!
      module ray_trace_LIC_image
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
      use lic_rgba_4_each_pixel
!
      use t_geometry_data
      use t_surface_data
      use t_control_params_4_pvr
      use t_control_param_LIC
      use t_geometries_in_pvr_screen
      use t_lic_field_data
      use m_machine_parameter
      use cal_lic_on_surf_viz
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine ray_trace_each_lic_image(node, ele, surf,              &
     &          lic_p, pvr_screen, field_lic, draw_param, color_param,  &
     &          viewpoint_vec, ray_vec4, num_pvr_ray, id_pixel_check,   &
     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,      &
     &          xx4_pvr_start, xx4_pvr_ray_start, rgba_ray)
!
      use t_noise_node_data
      use lic_pixel_ray_trace_fix_len
      use lic_pixel_ray_trace_by_ele
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_projected_position), intent(in) :: pvr_screen
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: ray_vec4(4)
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in)                                  &
     &                    :: id_pixel_check(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                    :: icount_pvr_trace(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                    :: isf_pvr_ray_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xi_pvr_start(2,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xx4_pvr_start(4,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                    ::  xx4_pvr_ray_start(4,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                    ::  rgba_ray(4,num_pvr_ray)
!
      integer(kind = kint) :: inum, iflag_comm
      real(kind = kreal) :: rgba_tmp(4)
!
!      type(noise_mask), allocatable :: n_mask
      integer(kind = kint) :: sample_cnt

      sample_cnt = 0
!
      if(lic_p%iflag_vr_sample_mode .eq. iflag_fixed_size) then
!$omp parallel do private(inum, iflag_comm,rgba_tmp)
        do inum = 1, num_pvr_ray
!         if(id_pixel_check(inum)*draw_param%num_sections .gt. 0) then
!           write(*,*) 'check section trace for ', my_rank, inum
!         end if
!
          rgba_tmp(1:4) = zero
          call s_lic_pixel_ray_trace_by_ele(node, ele, surf,            &
     &       pvr_screen%arccos_sf, pvr_screen%x_nod_model,              &
     &       viewpoint_vec, lic_p, field_lic, draw_param, color_param,  &
     &       ray_vec4, id_pixel_check(inum), isf_pvr_ray_start(1,inum), &
     &       xx4_pvr_ray_start(1,inum), xx4_pvr_start(1,inum),          &
     &       xi_pvr_start(1,inum), rgba_tmp(1), icount_pvr_trace(inum), &
     &       node%xyz_min_gl, node%xyz_max_gl, iflag_comm)
          rgba_ray(1:4,inum) = rgba_tmp(1:4)
          sample_cnt = sample_cnt + icount_pvr_trace(inum)
        end do
!$omp end parallel do
!
      else
!$omp parallel do private(inum, iflag_comm,rgba_tmp)
        do inum = 1, num_pvr_ray
!         if(id_pixel_check(inum)*draw_param%num_sections .gt. 0) then
!           write(*,*) 'check section trace for ', my_rank, inum
!         end if
!
          rgba_tmp(1:4) = zero
          call s_lic_pixel_ray_trace_fix_len(node, ele, surf,           &
     &       pvr_screen%arccos_sf, pvr_screen%x_nod_model,              &
     &       viewpoint_vec, lic_p, field_lic, draw_param, color_param,  &
     &       ray_vec4, id_pixel_check(inum), isf_pvr_ray_start(1,inum), &
     &       xx4_pvr_ray_start(1,inum), xx4_pvr_start(1,inum),          &
     &       xi_pvr_start(1,inum), rgba_tmp(1), icount_pvr_trace(inum), &
     &       node%xyz_min_gl, node%xyz_max_gl, iflag_comm)
          rgba_ray(1:4,inum) = rgba_tmp(1:4)
          sample_cnt = sample_cnt + icount_pvr_trace(inum)
        end do
!$omp end parallel do
      end if
      write(*,*) "pvr sampling cnt:", my_rank, sample_cnt
!
      end subroutine ray_trace_each_lic_image
!
!  ---------------------------------------------------------------------
!
      end module ray_trace_LIC_image
