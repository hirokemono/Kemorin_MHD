!>@file  set_lic_controls.f90
!!       module set_lic_controls
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine bcast_lic_controls                                   &
!!     &         (num_lic_ctl, pvr_ctl_type, lic_ctl_type, cflag_update)
!!        integer(kind = kint), intent(in) :: num_lic_ctl
!!        type(pvr_parameter_ctl), intent(inout)                        &
!!     &                        :: pvr_ctl_type(num_lic_ctl)
!!        type(lic_parameter_ctl), intent(inout)                        &
!!     &                        :: lic_ctl_type(num_lic_ctl)
!!      subroutine s_set_lic_controls(group, nod_fld, tracer, fline,    &
!!     &          fname_lic_ctl, pvr_ctl_type, lic_ctl_type, lic_param, &
!!     &          rep_ref, pvr, flag_each_repart)
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(tracer_module), intent(in) :: tracer
!!        type(fieldline_module), intent(in) :: fline
!!        type(volume_rendering_module), intent(inout) :: pvr
!!        type(pvr_parameter_ctl),intent(in):: pvr_ctl_type(pvr%num_pvr)
!!        type(lic_parameter_ctl),intent(in):: lic_ctl_type(pvr%num_pvr)
!!        character(len = kchara),intent(in):: fname_lic_ctl(pvr%num_pvr)
!!        type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
!!        type(lic_repart_reference), intent(inout)                     &
!!     &                             :: rep_ref(pvr%num_pvr)
!!        logical, intent(inout) :: flag_each_repart
!!      subroutine flush_each_lic_control(lic_param)
!!        type(lic_parameters), intent(inout) :: lic_param
!!@endverbatim
!
      module set_lic_controls
!
      use m_precision
      use m_machine_parameter
      use m_constants
!
      use calypso_mpi
!
      use t_control_param_LIC
      use t_control_data_LIC
      use t_sort_PVRs_by_type
!
      implicit  none
!
      private :: set_control_lic_noise, set_from_lic_control
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_lic_controls                                     &
     &         (num_lic_ctl, pvr_ctl_type, lic_ctl_type, cflag_update)
!
      use bcast_control_data_4_pvr
      use bcast_control_data_4_lic
      use set_pvr_control
!
      integer(kind = kint), intent(in) :: num_lic_ctl
!
      type(pvr_parameter_ctl), intent(inout)                            &
     &                        :: pvr_ctl_type(num_lic_ctl)
      type(lic_parameter_ctl), intent(inout)                            &
     &                        :: lic_ctl_type(num_lic_ctl)
      character(len=kchara), intent(inout) :: cflag_update
!
      integer(kind = kint) :: i_lic
!
!
      if(pvr_ctl_type(1)%updated_ctl%iflag .gt. 0) then
        cflag_update = pvr_ctl_type(1)%updated_ctl%charavalue
      end if
!
      do i_lic = 1, num_lic_ctl
        call bcast_vr_psf_ctl(pvr_ctl_type(i_lic))
        call bcast_lic_control_data(lic_ctl_type(i_lic))
      end do
!
      end subroutine bcast_lic_controls
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_lic_controls(group, nod_fld, tracer, fline,      &
     &          fname_lic_ctl, pvr_ctl_type, lic_ctl_type, lic_param,   &
     &          rep_ref, pvr, flag_each_repart)
!
      use m_error_IDs
      use t_phys_data
      use t_group_data
      use t_particle_trace
      use t_fieldline
      use t_lic_repart_reference
      use t_volume_rendering
      use set_control_each_pvr
      use set_field_comp_for_viz
      use set_pvr_modelview_matrix
      use set_control_pvr_movie
      use cal_3d_noise
      use set_pvr_control
      use rendering_and_image_nums
!
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
      type(volume_rendering_module), intent(inout) :: pvr
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type(pvr%num_pvr)
      type(lic_parameter_ctl), intent(in) :: lic_ctl_type(pvr%num_pvr)
      character(len = kchara), intent(in) :: fname_lic_ctl(pvr%num_pvr)
!
      type(lic_parameters), intent(inout) :: lic_param(pvr%num_pvr)
      type(lic_repart_reference), intent(inout) :: rep_ref(pvr%num_pvr)
      logical, intent(inout) :: flag_each_repart
!
      integer(kind = kint) :: i_lic
!
!
      call s_sort_PVRs_by_type(pvr%num_pvr, pvr_ctl_type, pvr%PVR_sort)
!
      if(iflag_debug .gt. 0) then
        do i_lic = 0, 6
          write(*,*) i_lic, 'pvr%PVR_sort%istack_PVR_modes',            &
    &       pvr%PVR_sort%istack_PVR_modes(i_lic)
        end do
        do i_lic = 1, pvr%num_pvr
          write(*,*) i_lic, trim(fname_lic_ctl(i_lic)), '  ',           &
    &            yes_flag(pvr_ctl_type(i_lic)%anaglyph_ctl%charavalue), &
    &            '  ', pvr_ctl_type(i_lic)%movie%movie_mode_ctl%iflag,  &
    &            yes_flag(pvr_ctl_type(i_lic)%quilt_ctl%charavalue),    &
    &            '  ', pvr%PVR_sort%ipvr_sorted(i_lic)
        end do
      end if
 !
      do i_lic = 1, pvr%num_pvr
        call alloc_iflag_pvr_boundaries                                 &
     &     (group%surf_grp, pvr%pvr_param(i_lic)%draw_param)
      end do
!
      call set_from_lic_control(group, nod_fld, tracer, fline,          &
     &    pvr%num_pvr,  pvr_ctl_type, lic_ctl_type, pvr%PVR_sort,       &
     &    lic_param, pvr%pvr_param, rep_ref, flag_each_repart)
!
!   Set image output PE
      call count_num_rendering_and_images(pvr%num_pvr, pvr%pvr_param,   &
     &    pvr%num_pvr_images, pvr%PVR_sort%istack_pvr_images)
!
      call alloc_pvr_images(pvr)
      call set_rendering_and_image_pes                                  &
     &   (nprocs, pvr%num_pvr, pvr_ctl_type, pvr%PVR_sort,              &
     &    pvr%num_pvr_images, pvr%pvr_rgb)
      call dealloc_sort_PVRs_list(pvr%PVR_sort)
!
      end subroutine s_set_lic_controls
!
!   --------------------------------------------------------------------
!
      subroutine set_from_lic_control(group, nod_fld, tracer, fline,    &
     &          num_lic, pvr_ctl_type, lic_ctl_type, PVR_sort,          &
     &          lic_param, pvr_param, rep_ref, flag_each_repart)
!
      use m_error_IDs
      use t_phys_data
      use t_group_data
      use t_particle_trace
      use t_fieldline
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
      use t_control_data_pvr_sections
      use t_lic_repart_reference
      use t_pvr_image_array
      use set_control_each_pvr
      use set_field_comp_for_viz
      use set_pvr_modelview_matrix
      use set_control_pvr_movie
      use cal_3d_noise
      use set_pvr_control
      use rendering_and_image_nums
!
      integer(kind = kint), intent(in) :: num_lic
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(tracer_module), intent(in) :: tracer
      type(fieldline_module), intent(in) :: fline
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type(num_lic)
      type(lic_parameter_ctl), intent(in) :: lic_ctl_type(num_lic)
!
      type(sort_PVRs_by_type), intent(inout) :: PVR_sort
      type(lic_parameters), intent(inout) :: lic_param(num_lic)
      type(PVR_control_params), intent(inout) :: pvr_param(num_lic)
      type(lic_repart_reference), intent(inout) :: rep_ref(num_lic)
      logical, intent(inout) :: flag_each_repart
!
      integer(kind = kint) :: i_ctl, i_lic
!
!
      flag_each_repart = .FALSE.
      do i_ctl = 1, num_lic
        i_lic = PVR_sort%ipvr_sorted(i_ctl)
        if(iflag_debug .gt. 0) write(*,*) 'PVR parameters for'
        call set_pvr_stereo_control(pvr_ctl_type(i_ctl),                &
     &                              pvr_param(i_lic)%stereo_def)
        call s_set_control_pvr_movie(pvr_ctl_type(i_ctl)%movie,         &
     &                               pvr_param(i_lic)%movie_def)
!
        call set_control_lic_parameter                                  &
     &     (nod_fld%num_phys, nod_fld%phys_name,                        &
     &      lic_ctl_type(i_ctl), lic_param(i_lic), rep_ref(i_lic),      &
     &      flag_each_repart)
!
        if(iflag_debug .gt. 0) write(*,*) 'set_control_pvr'
        call set_control_pvr(pvr_ctl_type(i_ctl),                       &
     &      group%ele_grp, group%surf_grp, tracer, fline,               &
     &      pvr_param(i_lic)%area_def, pvr_param(i_lic)%draw_param,     &
     &      pvr_param(i_lic)%color, pvr_param(i_lic)%colorbar)
        pvr_param(i_lic)%colorbar%iflag_opacity = 0
!
!   set transfer matrix
        call set_pvr_mul_view_params(pvr_ctl_type(i_ctl)%mat,           &
     &      pvr_ctl_type(i_ctl)%quilt_c, pvr_ctl_type(i_ctl)%movie,     &
     &      pvr_param(i_lic))
      end do
!
!   set noise for LIC
      call set_control_lic_noise(num_lic, lic_param)
!
      end subroutine set_from_lic_control
!
!   --------------------------------------------------------------------
!
      subroutine set_control_lic_noise(num_lic, lic_param)
!
      use t_control_data_LIC
      use cal_3d_noise
      use bcast_3d_noise
!
      integer(kind = kint), intent(in) :: num_lic
      type(lic_parameters), intent(inout) :: lic_param(num_lic)
!
      integer(kind = kint) :: ierr, i_lic
!
!
      if(my_rank .eq. 0) then
        do i_lic = 1, num_lic
          call sel_const_3d_cube_noise(lic_param(i_lic)%noise_t)
        end do
        call finalize_kemo_mt_stream
!
        do i_lic = 1, num_lic
          call sel_input_3d_cube_noise                                  &
     &       (my_rank, lic_param(i_lic)%noise_t, ierr)
          if(ierr .gt. 0) call calypso_mpi_abort(ierr, e_message)
!
          call noise_normalization(lic_param(i_lic)%noise_t%n_cube,     &
     &                            lic_param(i_lic)%noise_t%nidx_xyz,    &
     &                            lic_param(i_lic)%noise_t%rnoise_grad)
          call grad_3d_noise(lic_param(i_lic)%noise_t%n_cube,           &
     &                       lic_param(i_lic)%noise_t%nidx_xyz,         &
     &                       lic_param(i_lic)%noise_t%asize_cube,       &
     &                       lic_param(i_lic)%noise_t%rnoise_grad)
          call sel_output_3d_cube_noise                                 &
     &       (lic_param(i_lic)%noise_t)
        end do
      end if
!
      do i_lic = 1, num_lic
        call bcast_3d_cube_noise(lic_param(i_lic)%noise_t)
      end do
!
      end subroutine set_control_lic_noise
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine flush_each_lic_control(lic_param)
!
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
!
      type(lic_parameters), intent(inout) :: lic_param
!
!
      call dealloc_3d_cube_noise(lic_param%noise_t)
      call dealloc_lic_masking_ranges(lic_param)
      call dealloc_lic_kernel(lic_param%kernel_t)
!
      end subroutine flush_each_lic_control
!
!  ---------------------------------------------------------------------
!
      end module set_lic_controls
