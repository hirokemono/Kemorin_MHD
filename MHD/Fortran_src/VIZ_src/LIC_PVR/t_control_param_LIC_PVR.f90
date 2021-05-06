!>@file  t_control_param_LIC_PVR.f90
!!       module t_control_param_LIC_PVR
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
!!      subroutine s_set_lic_controls(group, nod_fld, num_lic,          &
!!     &           pvr_ctl_type, lic_ctl_type, lic_fld_pm, pvr_param)
!!        integer(kind = kint), intent(in) :: num_lic
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type(num_lic)
!!        type(lic_parameter_ctl), intent(in) :: lic_ctl_type(num_lic)
!!        type(LIC_field_params), intent(inout) :: lic_fld_pm(num_lic)
!!        type(PVR_control_params), intent(inout) :: pvr_param(num_lic)
!!      subroutine flush_each_lic_control(lic_fld_pm)
!!        type(LIC_field_params), intent(inout) :: lic_fld_pm
!!@endverbatim
!
      module t_control_param_LIC_PVR
!
      use m_precision
      use m_machine_parameter
      use m_constants
!
      use calypso_mpi
!
      use t_control_param_LIC
      use t_control_data_LIC
      use t_control_data_lic_pvr
      use t_lic_field_data
!
      implicit  none
!
!>      Structure of LIC field parameters
      type LIC_field_params
!>        Structure for field parameter for PVR
        type(lic_parameters) :: lic_param
!>        Structure for field data for LIC
        type(lic_field_data), pointer :: field_lic
!
!>        Structure for field data for LIC
        type(lic_field_data), pointer :: nod_fld_lic
      end type LIC_field_params
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
      subroutine s_set_lic_controls(group, nod_fld, num_lic,            &
     &           pvr_ctl_type, lic_ctl_type, lic_fld_pm, pvr_param)
!
      use m_error_IDs
      use t_phys_data
      use t_group_data
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
      use t_control_data_pvr_sections
      use set_control_each_pvr
      use set_field_comp_for_viz
      use set_pvr_modelview_matrix
      use cal_3d_noise
!
      integer(kind = kint), intent(in) :: num_lic
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type(num_lic)
      type(lic_parameter_ctl), intent(in) :: lic_ctl_type(num_lic)
!
      type(LIC_field_params), intent(inout) :: lic_fld_pm(num_lic)
      type(PVR_control_params), intent(inout) :: pvr_param(num_lic)
!
      integer(kind = kint) :: i_lic
!
!
      do i_lic = 1, num_lic
        if(iflag_debug .gt. 0) write(*,*) 'PVR parameters for'
        call set_control_pvr_movie                                      &
     &     (pvr_ctl_type(i_lic)%movie, pvr_param(i_lic)%view)
        call set_pvr_stereo_control                                     &
     &     (pvr_ctl_type(i_lic), pvr_param(i_lic)%view)
!
        call set_control_lic_parameter                                  &
     &     (nod_fld%num_phys, nod_fld%phys_name,                        &
     &      lic_ctl_type(i_lic), lic_fld_pm(i_lic)%lic_param)
!
        if(iflag_debug .gt. 0) write(*,*) 'set_control_pvr'
        call set_control_pvr                                            &
     &     (pvr_ctl_type(i_lic), group%ele_grp, group%surf_grp,         &
     &      pvr_param(i_lic)%area_def, pvr_param(i_lic)%draw_param,     &
     &      pvr_param(i_lic)%color, pvr_param(i_lic)%colorbar)
!        pvr_param(i_lic)%colorbar%iflag_opacity = 0
!
!   set transfer matrix
!
        call s_set_pvr_modelview_matrix                                 &
     &     (pvr_ctl_type(i_lic)%mat, pvr_param(i_lic)%view)
      end do
!
      call set_control_lic_noise(num_lic, lic_fld_pm)
!
      end subroutine s_set_lic_controls
!
!   --------------------------------------------------------------------
!
      subroutine set_control_lic_noise(num_lic, lic_fld_pm)
!
      use t_control_data_LIC
      use cal_3d_noise
      use bcast_3d_noise
!
      integer(kind = kint), intent(in) :: num_lic
      type(LIC_field_params), intent(inout) :: lic_fld_pm(num_lic)
!
      integer(kind = kint) :: ierr, i_lic, iflag
      integer(kind = kint_gl) :: ntot_noise
!
!
      if(my_rank .eq. 0) then
        ntot_noise = 0
        do i_lic = 1, num_lic
          iflag = lic_fld_pm(i_lic)%lic_param%noise_t%iflag_noise_type
          if(iflag .ne. iflag_from_file) then
            ntot_noise = ntot_noise                                     &
     &                + lic_fld_pm(i_lic)%lic_param%noise_t%n_cube
          end if
        end do
        write(*,*) 'ntot_noise', ntot_noise
!
        do i_lic = 1, num_lic
          call sel_const_3d_cube_noise                                  &
     &       (lic_fld_pm(i_lic)%lic_param%noise_t, i_lic)
        end do
        call finalize_kemo_mt_stream
!
        do i_lic = 1, num_lic
          call sel_input_3d_cube_noise(my_rank,                         &
     &        lic_fld_pm(i_lic)%lic_param%noise_t, ierr)
          if(ierr .gt. 0) call calypso_mpi_abort(ierr, e_message)
!
          call noise_normalization                                      &
     &       (lic_fld_pm(i_lic)%lic_param%noise_t%n_cube,               &
     &        lic_fld_pm(i_lic)%lic_param%noise_t%nidx_xyz,             &
     &        lic_fld_pm(i_lic)%lic_param%noise_t%rnoise_grad)
          call grad_3d_noise                                            &
     &       (lic_fld_pm(i_lic)%lic_param%noise_t%n_cube,               &
     &        lic_fld_pm(i_lic)%lic_param%noise_t%nidx_xyz,             &
     &        lic_fld_pm(i_lic)%lic_param%noise_t%asize_cube,           &
     &        lic_fld_pm(i_lic)%lic_param%noise_t%rnoise_grad)
          call sel_output_3d_cube_noise                                 &
     &       (lic_fld_pm(i_lic)%lic_param%noise_t)
        end do
      end if
!
      do i_lic = 1, num_lic
        call bcast_3d_cube_noise(lic_fld_pm(i_lic)%lic_param%noise_t)
      end do
!
      end subroutine set_control_lic_noise
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine flush_each_lic_control(lic_fld_pm)
!
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
!
      type(LIC_field_params), intent(inout) :: lic_fld_pm
!
!
      call dealloc_3d_cube_noise(lic_fld_pm%lic_param%noise_t)
      call dealloc_lic_masking_ranges(lic_fld_pm%lic_param)
      call dealloc_lic_kernel(lic_fld_pm%lic_param%kernel_t)
!
      end subroutine flush_each_lic_control
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module t_control_param_LIC_PVR
