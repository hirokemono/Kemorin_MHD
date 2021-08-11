!>@file   ray_trace_LIC_image.F90
!!@brief  module ray_trace_LIC_image
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine ray_trace_each_lic_image(mesh, group, sf_grp_4_sf,   &
!!     &          lic_p, field_lic, draw_param, color_param,            &
!!     &          viewpoint_vec, modelview_mat, projection_mat,         &
!!     &          ray_vec4, num_pvr_ray, id_pixel_check,                &
!!     &          isf_pvr_ray_start, xi_pvr_start,                      &
!!     &          xx4_pvr_start, xx4_pvr_ray_start, rgba_ray,           &
!!     &          elapse_ray_trace_out, count_int_nod)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind = kreal), intent(in) :: viewpoint_vec(3)
!!        real(kind = kreal), intent(in) :: modelview_mat(4,4)
!!@endverbatim
!
      module ray_trace_LIC_image
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use m_machine_parameter
      use m_elapsed_labels_4_VIZ
      use m_work_time
      use calypso_mpi
      use lic_rgba_4_each_pixel
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surf_grp_list_each_surf
      use t_control_params_4_pvr
      use t_control_param_LIC
      use t_geometries_in_pvr_screen
      use t_lic_field_data
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
      subroutine ray_trace_each_lic_image(mesh, group, sf_grp_4_sf,     &
     &          lic_p, field_lic, draw_param, color_param,              &
     &          viewpoint_vec, modelview_mat, projection_mat,           &
     &          ray_vec4, num_pvr_ray, id_pixel_check,                  &
     &          isf_pvr_ray_start, xi_pvr_start,                        &
     &          xx4_pvr_start, xx4_pvr_ray_start, rgba_ray,             &
     &          elapse_ray_trace_out, count_int_nod)
!
      use calypso_mpi_int
      use calypso_mpi_real
      use t_noise_node_data
      use t_each_lic_trace_count_time
      use lic_pixel_ray_trace_fix_len
      use lic_pixel_ray_trace_by_ele
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
      real(kind = kreal), intent(in) :: projection_mat(4,4)
      real(kind = kreal), intent(in) :: ray_vec4(4)
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in)                                  &
     &                    :: id_pixel_check(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                    :: isf_pvr_ray_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xi_pvr_start(2,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xx4_pvr_start(4,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                    ::  xx4_pvr_ray_start(4,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                    ::  rgba_ray(4,num_pvr_ray)
      real(kind = kreal), intent(inout) :: elapse_ray_trace_out(2)
      real(kind = kreal), intent(inout)                                 &
     &                    :: count_int_nod(mesh%node%numnod)
!
      integer(kind = kint) :: inum, iflag_comm
      real(kind = kreal) :: rgba_tmp(4)
!
!      type(noise_mask), allocatable :: n_mask
      integer(kind = kint) :: icount_rtrace
      real(kind = kreal) :: elapse_line_tmp, end_time
!
      type(each_lic_trace_counts) :: l_elsp1
!
      integer(kind = kint) :: inod, ip, ip_smp
!
#ifdef _OPENMP
      integer, external :: omp_get_thread_num
#endif
!
      call init_icou_int_nod_smp(mesh%node, l_elsp1)
!
      ip_smp =     1
      icount_rtrace = 0
      elapse_line_tmp = 0.0d0
      l_elsp1%elapse_rtrace = MPI_WTIME()
!
      if(lic_p%iflag_vr_sample_mode .eq. iflag_fixed_size) then
!$omp parallel do private(inum,iflag_comm,rgba_tmp)                     &
!$omp& reduction(+:elapse_line_tmp,icount_rtrace)
        do inum = 1, num_pvr_ray
!         if(id_pixel_check(inum)*draw_param%num_sections .gt. 0) then
!           write(*,*) 'check section trace for ', my_rank, inum
!         end if
!
#ifdef _OPENMP
          ip_smp = omp_get_thread_num() + 1
#endif
          rgba_tmp(1:4) = zero
          call s_lic_pixel_ray_trace_fix_len(mesh%node, mesh%ele,       &
     &       mesh%surf, group%surf_grp, sf_grp_4_sf,                    &
     &       viewpoint_vec, modelview_mat, projection_mat,              &
     &       lic_p, field_lic, draw_param, color_param,                 &
     &       ray_vec4, id_pixel_check(inum), isf_pvr_ray_start(1,inum), &
     &       xx4_pvr_ray_start(1,inum), xx4_pvr_start(1,inum),          &
     &       xi_pvr_start(1,inum), rgba_tmp(1),                         &
     &       l_elsp1%icou_line_smp(1,ip_smp), icount_rtrace,            &
     &       elapse_line_tmp, iflag_comm)
          rgba_ray(1:4,inum) = rgba_tmp(1:4)
        end do
!$omp end parallel do
!
      else
!$omp parallel do private(inum, iflag_comm,rgba_tmp)                    &
!$omp& reduction(+:elapse_line_tmp,icount_rtrace)
        do inum = 1, num_pvr_ray
!         if(id_pixel_check(inum)*draw_param%num_sections .gt. 0) then
!           write(*,*) 'check section trace for ', my_rank, inum
!         end if
!
#ifdef _OPENMP
          ip_smp = omp_get_thread_num() + 1
#endif
          rgba_tmp(1:4) = zero
          call s_lic_pixel_ray_trace_by_ele(mesh%node, mesh%ele,        &
     &       mesh%surf, group%surf_grp, sf_grp_4_sf,                    &
     &       viewpoint_vec, modelview_mat, projection_mat,              &
     &       lic_p, field_lic, draw_param, color_param,                 &
     &       ray_vec4, id_pixel_check(inum), isf_pvr_ray_start(1,inum), &
     &       xx4_pvr_ray_start(1,inum), xx4_pvr_start(1,inum),          &
     &       xi_pvr_start(1,inum), rgba_tmp(1),                         &
     &       l_elsp1%icou_line_smp(1,ip_smp), icount_rtrace,            &
     &       elapse_line_tmp, iflag_comm)
          rgba_ray(1:4,inum) = rgba_tmp(1:4)
        end do
!$omp end parallel do
      end if
      end_time = MPI_WTIME()
      l_elsp1%icount_trace =    icount_rtrace
      l_elsp1%elapse_line_int = elapse_line_tmp
!
      call sum_icou_int_nod_smp(mesh%node, mesh%ele, end_time,          &
     &                          l_elsp1, count_int_nod)
!
      if(iflag_LIC_time) then
        elps1%elapsed(ist_elapsed_LIC+3)                                &
     &     = elps1%elapsed(ist_elapsed_LIC+3) + l_elsp1%elapse_rtrace
        elps1%elapsed(ist_elapsed_LIC+4)                                &
     &     = elps1%elapsed(ist_elapsed_LIC+4) + l_elsp1%elapse_line_int
      end if
!
      call cal_trace_time_statistic(mesh%node, mesh%ele, lic_p,         &
     &    field_lic, l_elsp1, elapse_ray_trace_out)
      call dealloc_icou_int_nod_smp(l_elsp1)
!
      end subroutine ray_trace_each_lic_image
!
!  ---------------------------------------------------------------------
!
      end module ray_trace_LIC_image
