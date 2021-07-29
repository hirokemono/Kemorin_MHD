!>@file   ray_trace_LIC_image.f90
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
!!     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,    &
!!     &          xx4_pvr_start, xx4_pvr_ray_start, rgba_ray)
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
     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,      &
     &          xx4_pvr_start, xx4_pvr_ray_start, rgba_ray)
!
      use t_noise_node_data
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
      real(kind = kreal) :: elapse_trace
!
!      type(noise_mask), allocatable :: n_mask
      integer(kind = kint) :: sample_cnt

      sample_cnt = 0
      elapse_trace = 0.0d0
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+3)
      if(lic_p%iflag_vr_sample_mode .eq. iflag_fixed_size) then
!$omp parallel do private(inum,iflag_comm,rgba_tmp)                     &
!$omp& reduction(+:elapse_trace)
        do inum = 1, num_pvr_ray
!         if(id_pixel_check(inum)*draw_param%num_sections .gt. 0) then
!           write(*,*) 'check section trace for ', my_rank, inum
!         end if
!
          rgba_tmp(1:4) = zero
          call s_lic_pixel_ray_trace_fix_len(mesh%node, mesh%ele,       &
     &       mesh%surf, group%surf_grp, sf_grp_4_sf,                    &
     &       viewpoint_vec, modelview_mat, projection_mat,              &
     &       lic_p, field_lic, draw_param, color_param,                 &
     &       ray_vec4, id_pixel_check(inum), isf_pvr_ray_start(1,inum), &
     &       xx4_pvr_ray_start(1,inum), xx4_pvr_start(1,inum),          &
     &       xi_pvr_start(1,inum), rgba_tmp(1), icount_pvr_trace(inum), &
     &       elapse_trace, iflag_comm)
          rgba_ray(1:4,inum) = rgba_tmp(1:4)
          sample_cnt = sample_cnt + icount_pvr_trace(inum)
        end do
!$omp end parallel do
!
      else
!$omp parallel do private(inum, iflag_comm,rgba_tmp)                    &
!$omp& reduction(+:elapse_trace)
        do inum = 1, num_pvr_ray
!         if(id_pixel_check(inum)*draw_param%num_sections .gt. 0) then
!           write(*,*) 'check section trace for ', my_rank, inum
!         end if
!
          rgba_tmp(1:4) = zero
          call s_lic_pixel_ray_trace_by_ele(mesh%node, mesh%ele,        &
     &       mesh%surf, group%surf_grp, sf_grp_4_sf,                    &
     &       viewpoint_vec, modelview_mat, projection_mat,              &
     &       lic_p, field_lic, draw_param, color_param,                 &
     &       ray_vec4, id_pixel_check(inum), isf_pvr_ray_start(1,inum), &
     &       xx4_pvr_ray_start(1,inum), xx4_pvr_start(1,inum),          &
     &       xi_pvr_start(1,inum), rgba_tmp(1), icount_pvr_trace(inum), &
     &       elapse_trace, iflag_comm)
          rgba_ray(1:4,inum) = rgba_tmp(1:4)
          sample_cnt = sample_cnt + icount_pvr_trace(inum)
        end do
!$omp end parallel do
      end if
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+3)
!      if(i_debug .gt. 0) write(*,*)                                    &
!      write(*,*)                                                       &
!     &                 "pvr sampling cnt:", my_rank, sample_cnt
!
      elps1%elapsed(ist_elapsed_LIC+4)                                  &
     &       = elps1%elapsed(ist_elapsed_LIC+4)                         &
     &        +  elapse_trace / dble(np_smp)
      trace_time_lc =    elps%elapsed(ist_elapsed_LIC+3)
      line_int_time_lc = elps%elapsed(ist_elapsed_LIC+4)
!
      call calypso_mpi_allreduce_one_real                               &
     &   (trace_time_lc, dmin_trace_time, MPI_MIN)
      call calypso_mpi_allreduce_one_real                               &
     &   (line_int_time_lc, dmin_line_int_time, MPI_MIN)
      call calypso_mpi_allreduce_one_int                                &
     &   (sample_cnt, min_sample_cnt, MPI_MIN)
!
      call calypso_mpi_allreduce_one_real                               &
     &   (trace_time_lc, dmax_trace_time, MPI_MAX)
      call calypso_mpi_allreduce_one_real                               &
     &   (line_int_time_lc, dmax_line_int_time, MPI_MAX)
      call calypso_mpi_allreduce_one_int                                &
     &   (sample_cnt, max_sample_cnt, MPI_MAX)
!
      call calypso_mpi_allreduce_one_real                               &
     &   (trace_time_lc, ave_trace_time, MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (line_int_time_lc, ave_line_int_time, MPI_SUM)
      call calypso_mpi_allreduce_one_int                                &
     &   (sample_cnt, ave_sample_cnt, MPI_SUM)
!
      ave_trace_time =    ave_trace_time / dble(nprocs)
      ave_line_int_time = ave_line_int_time / dble(nprocs)
      ave_sample_cnt =    ave_sample_cnt / dble(nprocs)
!
      sq_trace_time =    (trace_time_lc - ave_trace_time)**2
      sq_line_int_time = (line_int_time_lc - ave_line_int_time)**2
      sq_sample_cnt =    (sample_cnt - ave_sample_cnt)**2
!
      call calypso_mpi_allreduce_one_real                               &
     &   (sq_trace_time,    std_trace_time, MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (sq_line_int_time, std_line_int_time, MPI_SUM)
      call calypso_mpi_allreduce_one_int                                &
     &   (sq_sample_cnt,    std_sample_cnt, MPI_SUM)
!
      std_trace_time =    sqrt(std_trace_time / dble(nprocs))
      std_line_int_time = sqrt(std_line_int_time / dble(nprocs))
      std_sample_cnt =    sqrt(std_sample_cnt / dble(nprocs))
!
      if(my_rank .eq. 0) then
        write(*,*) 'Trace counts, rendering, line_integration'
        write(*,*) 'Average:   ',  
        write(*,'(a,i8,1p2e15.7)') 'Deviation: ',                       &
     &          ave_sample_cnt, ave_trace_time, ave_line_int_time
        write(*,'(a,i8,1p2e15.7)') 'Minimum:   ',                       &
     &          min_sample_cnt, dmin_trace_time, dmin_line_int_time
        write(*,'(a,i8,1p2e15.7)') 'Maximum:   ',                       &
     &          max_sample_cnt, dmax_trace_time, dmax_line_int_time
      end if
!
!
      trace_time_lc = trace_time_lc / dble(mesh%node%internal_node)
      do inod = 1, mesh%node%internal_node
        ref_trace_time(inod) = trace_time_lc
      end do
!
      
!
      end subroutine ray_trace_each_lic_image
!
!  ---------------------------------------------------------------------
!
      end module ray_trace_LIC_image
