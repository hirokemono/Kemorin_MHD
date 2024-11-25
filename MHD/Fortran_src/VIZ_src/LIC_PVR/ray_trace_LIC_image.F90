!>@file   ray_trace_LIC_image.F90
!!@brief  module ray_trace_LIC_image
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine ray_trace_each_lic_image                             &
!!     &         (elps_LIC, mesh, group, sf_grp_4_sf, lic_p, field_lic, &
!!     &          pvr_screen, draw_param, color_param, pvr_start,       &
!!     &          elapse_ray_trace_out, count_int_nod)
!!        type(elapsed_lables), intent(in) :: elps_LIC
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(pvr_projected_position), intent(in) :: pvr_screen
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_ray_start_type), intent(inout) :: pvr_start
!!        real(kind = kreal), intent(inout) :: elapse_ray_trace_out(2)
!!        real(kind = kreal), intent(inout)                             &
!!     &                    :: count_int_nod(mesh%node%numnod)
!!@endverbatim
!
      module ray_trace_LIC_image
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use m_machine_parameter
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
      use t_pvr_colormap_parameter
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
      subroutine ray_trace_each_lic_image                               &
     &         (elps_LIC, mesh, group, sf_grp_4_sf, lic_p, field_lic,   &
     &          pvr_screen, draw_param, color_param, pvr_start,         &
     &          elapse_ray_trace_out, count_int_nod)
!
      use calypso_mpi_int
      use calypso_mpi_real
      use t_pvr_ray_startpoints
      use t_noise_node_data
      use t_each_lic_trace_count_time
      use lic_pixel_ray_trace_fix_len
      use lic_pixel_ray_trace_by_ele
!
      type(elapsed_lables), intent(in) :: elps_LIC
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
      type(pvr_projected_position), intent(in) :: pvr_screen
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      real(kind = kreal), intent(inout) :: elapse_ray_trace_out(2)
      real(kind = kreal), intent(inout)                                 &
     &                    :: count_int_nod(mesh%node%numnod)
!
      type(each_lic_trace_counts) :: l_elsp1
      integer(kind = kint) :: inum, iflag_comm
      integer(kind = kint) :: ip, ip_smp
!
#ifdef _OPENMP
      integer, external :: omp_get_thread_num
#endif
!
      call init_icou_int_nod_smp(mesh%node, l_elsp1)
!
      ip_smp =     1
!$omp parallel do
      do ip = 1, l_elsp%np_smp_sys
        l_elsp1%line_count_smp(ip)%icount_trace_smp = 0
        l_elsp1%line_count_smp(ip)%elapse_lint_smp = 0.0d0
      end do
!$omp end parallel do
      l_elsp1%elapse_rtrace = MPI_WTIME()
!
!
      if(lic_p%iflag_vr_sample_mode .eq. iflag_fixed_size) then
!$omp parallel do private(inum,iflag_comm)
        do inum = 1, pvr_start%num_pvr_ray
#ifdef _OPENMP
          ip_smp = omp_get_thread_num() + 1
#endif
          pvr_start%rgba_ray(1:4,inum) = zero
          call s_lic_pixel_ray_trace_fix_len(mesh%node, mesh%ele,       &
     &       mesh%surf, group%surf_grp, sf_grp_4_sf,                    &
     &       pvr_screen%viewpoint_vec, pvr_screen%modelview_mat,        &
     &       pvr_screen%projection_mat,  lic_p, field_lic,              &
     &       draw_param, color_param, ray_vec4,                         &
     &       pvr_start%id_pixel_check(inum),                            &
     &       pvr_start%isf_pvr_ray_start(1,inum),                       &
     &       pvr_start%xx4_pvr_ray_start(1,inum),                       &
     &       pvr_start%xx4_pvr_start(1,inum),                           &
     &       pvr_start%xi_pvr_start(1,inum),                            &
     &       pvr_start%rgba_ray(1,inum),                                &
     &       l_elsp1%line_count_smp(ip_smp), iflag_comm)
        end do
!$omp end parallel do
!
      else
!$omp parallel do private(inum, iflag_comm)
        do inum = 1, pvr_start%num_pvr_ray
#ifdef _OPENMP
          ip_smp = omp_get_thread_num() + 1
#endif
          pvr_start%rgba_ray(1:4,inum) = zero
          call s_lic_pixel_ray_trace_by_ele(mesh%node, mesh%ele,        &
     &       mesh%surf, group%surf_grp, sf_grp_4_sf,                    &
     &       pvr_screen%viewpoint_vec, pvr_screen%modelview_mat,        &
     &       pvr_screen%projection_mat, lic_p, field_lic,               &
     &       draw_param, color_param, ray_vec4,                         &
     &       pvr_start%id_pixel_check(inum),                            &
     &       pvr_start%isf_pvr_ray_start(1,inum),                       &
     &       pvr_start%xx4_pvr_ray_start(1,inum),                       &
     &       pvr_start%xx4_pvr_start(1,inum),                           &
     &       pvr_start%xi_pvr_start(1,inum),                            &
     &       pvr_start%rgba_ray(1,inum),                                &
     &       l_elsp1%line_count_smp(ip_smp), iflag_comm)
        end do
!$omp end parallel do
      end if
      l_elsp1%elapse_rtrace = MPI_WTIME() - l_elsp1%elapse_rtrace       &
     &                                    - l_elsp1%elapse_line_int
      call sum_icou_int_nod_smp(mesh%node, mesh%ele,                    &
     &                          l_elsp1, count_int_nod)
!
      if(elps_LIC%flag_elapsed) then
        elps1%elapsed(elps_LIC%ist_elapsed+3)                           &
     &     = elps1%elapsed(elps_LIC%ist_elapsed+3)                      &
     &      + l_elsp1%elapse_rtrace
        elps1%elapsed(elps_LIC%ist_elapsed+4)                           &
     &     = elps1%elapsed(elps_LIC%ist_elapsed+4)                      &
     &      + l_elsp1%elapse_line_int
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
