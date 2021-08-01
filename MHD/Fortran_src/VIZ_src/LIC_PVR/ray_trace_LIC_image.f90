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
!!     &          xx4_pvr_start, xx4_pvr_ray_start, rgba_ray,           &
!!     &          elapse_ray_trace_out)
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
     &          xx4_pvr_start, xx4_pvr_ray_start, rgba_ray,             &
     &          elapse_ray_trace_out)
!
      use calypso_mpi_int
      use calypso_mpi_real
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
      real(kind = kreal), intent(inout) :: elapse_ray_trace_out(2)
!
      integer(kind = kint) :: inum, iflag_comm
      real(kind = kreal) :: rgba_tmp(4)
!
!      type(noise_mask), allocatable :: n_mask
      integer(kind = kint) :: sample_cnt
      real(kind = kreal) :: ave_sample_cnt
      real(kind = kreal) :: std_sample_cnt, sq_sample_cnt
      integer(kind = kint) :: min_sample_cnt, max_sample_cnt
      real(kind = kreal) :: elapse_rtrace, sq_trace_time
      real(kind = kreal) :: ave_trace_time,   std_trace_time
      real(kind = kreal) :: dmin_trace_time,  dmax_trace_time
      real(kind = kreal) :: elapse_line_int,    sq_line_int_time
      real(kind = kreal) :: ave_line_int_time,  std_line_int_time
      real(kind = kreal) :: dmin_line_int_time, dmax_line_int_time
!
!
      sample_cnt = 0
      elapse_line_int = 0.0d0
      elapse_rtrace = MPI_WTIME()
!
      if(lic_p%iflag_vr_sample_mode .eq. iflag_fixed_size) then
!$omp parallel do private(inum,iflag_comm,rgba_tmp)                     &
!$omp& reduction(+:elapse_line_int)
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
     &       elapse_line_int, iflag_comm)
          rgba_ray(1:4,inum) = rgba_tmp(1:4)
          sample_cnt = sample_cnt + icount_pvr_trace(inum)
        end do
!$omp end parallel do
!
      else
!$omp parallel do private(inum, iflag_comm,rgba_tmp)                    &
!$omp& reduction(+:elapse_line_int)
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
     &       elapse_line_int, iflag_comm)
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
      elapse_line_int = elapse_line_int / dble(np_smp)
      elapse_rtrace = MPI_WTIME() - elapse_rtrace - elapse_line_int
      if(iflag_LIC_time) then
        elps1%elapsed(ist_elapsed_LIC+3)                                &
     &       = elps1%elapsed(ist_elapsed_LIC+3) +  elapse_rtrace
        elps1%elapsed(ist_elapsed_LIC+4)                                &
     &       = elps1%elapsed(ist_elapsed_LIC+4) +  elapse_line_int
      end if
!
      call calypso_mpi_allreduce_one_real                               &
     &   (elapse_rtrace, dmin_trace_time, MPI_MIN)
      call calypso_mpi_allreduce_one_real                               &
     &   (elapse_line_int, dmin_line_int_time, MPI_MIN)
      call calypso_mpi_allreduce_one_int                                &
     &   (sample_cnt, min_sample_cnt, MPI_MIN)
!
      call calypso_mpi_allreduce_one_real                               &
     &   (elapse_rtrace, dmax_trace_time, MPI_MAX)
      call calypso_mpi_allreduce_one_real                               &
     &   (elapse_line_int, dmax_line_int_time, MPI_MAX)
      call calypso_mpi_allreduce_one_int                                &
     &   (sample_cnt, max_sample_cnt, MPI_MAX)
!
      call calypso_mpi_allreduce_one_real                               &
     &   (elapse_rtrace, ave_trace_time, MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (elapse_line_int, ave_line_int_time, MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (dble(sample_cnt), ave_sample_cnt, MPI_SUM)
!
      ave_trace_time =    ave_trace_time / dble(nprocs)
      ave_line_int_time = ave_line_int_time / dble(nprocs)
      ave_sample_cnt =    ave_sample_cnt / dble(nprocs)
!
      sq_trace_time =    (elapse_rtrace -    ave_trace_time)**2
      sq_line_int_time = (elapse_line_int -  ave_line_int_time)**2
      sq_sample_cnt =    (dble(sample_cnt) - ave_sample_cnt)**2
!
      call calypso_mpi_allreduce_one_real                               &
     &   (sq_trace_time,    std_trace_time, MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (sq_line_int_time, std_line_int_time, MPI_SUM)
      call calypso_mpi_allreduce_one_real                               &
     &   (sq_sample_cnt,    std_sample_cnt, MPI_SUM)
!
      std_trace_time =    sqrt(std_trace_time / dble(nprocs))
      std_line_int_time = sqrt(std_line_int_time / dble(nprocs))
      std_sample_cnt =    sqrt(std_sample_cnt / dble(nprocs))
!
      if(my_rank .eq. 0) then
        write(*,*) 'Trace counts, rendering, line_integration'
        write(*,'(a,i8,1p2e15.7)') 'Average: ',                         &
     &          ave_sample_cnt, ave_trace_time, ave_line_int_time
        write(*,'(a,i8,1p2e15.7)') 'Deviation: ',                       &
     &          int(std_sample_cnt), std_trace_time, std_line_int_time
        write(*,'(a,i8,1p2e15.7)') 'Minimum:   ',                       &
     &          min_sample_cnt, dmin_trace_time, dmin_line_int_time
        write(*,'(a,i8,1p2e15.7)') 'Maximum:   ',                       &
     &          max_sample_cnt, dmax_trace_time, dmax_line_int_time
      end if
!
      elapse_ray_trace_out(1)                                           &
     &     = elapse_rtrace / dble(mesh%node%internal_node)
      elapse_ray_trace_out(2) = elapse_line_int                         &
     &     / dble(nnod_masked_4_LIC(mesh%node, lic_p, field_lic))
!
      end subroutine ray_trace_each_lic_image
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function nnod_masked_4_LIC                   &
     &                            (node, lic_p, field_lic)
!
      type(node_data), intent(in) :: node
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
!
      real(kind = kreal), allocatable :: value(:,:)
      integer(kind = kint), allocatable :: icou_smp(:)
      integer(kind = kint) :: ip, ist, ied, inod
!
!
      allocate(value(lic_p%num_masking,np_smp))
      allocate(icou_smp(np_smp))
!
      icou_smp(1:np_smp) = 0
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = node%istack_internal_smp(ip-1) + 1
        ied = node%istack_internal_smp(ip)
        do inod = ist, ied
          value(1:lic_p%num_masking,ip)                                 &
     &          = field_lic%s_lic(inod,1:lic_p%num_masking)
          if(lic_mask_flag(lic_p, value(1,ip))) then
            icou_smp(ip) = icou_smp(ip) + 1
          end if
        end do
      end do
!$omp end parallel do
!
      nnod_masked_4_LIC = sum(icou_smp)
      deallocate(value, icou_smp)
!
      end function nnod_masked_4_LIC
!
! ----------------------------------------------------------------------
!
      end module ray_trace_LIC_image
