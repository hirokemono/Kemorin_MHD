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
!!     &           pvr_ctl_type, lic_ctl_type, lic_fld, pvr_param)
!!        integer(kind = kint), intent(in) :: num_lic
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type(num_lic)
!!        type(lic_parameter_ctl), intent(in) :: lic_ctl_type(num_lic)
!!        type(LIC_field_params), intent(inout) :: lic_fld(num_lic)
!!        type(PVR_control_params), intent(inout) :: pvr_param(num_lic)
!!      subroutine flush_each_lic_control(lic_fld)
!!        type(LIC_field_params), intent(inout) :: lic_fld
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
!
      implicit  none
!
!>      Structure of PVR field parameters
      type LIC_field_params
!>        Structure for field parameter for PVR
        type(lic_parameters) :: lic_param
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
     &           pvr_ctl_type, lic_ctl_type, lic_fld, pvr_param)
!
      use m_error_IDs
      use t_phys_data
      use t_group_data
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
      use t_control_data_pvr_sections
      use t_LIC_kernel_image
      use set_control_each_pvr
      use set_field_comp_for_viz
      use set_pvr_modelview_matrix
!
      integer(kind = kint), intent(in) :: num_lic
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type(num_lic)
      type(lic_parameter_ctl), intent(in) :: lic_ctl_type(num_lic)
!
      type(LIC_field_params), intent(inout) :: lic_fld(num_lic)
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
     &      lic_ctl_type(i_lic), lic_fld(i_lic)%lic_param)
!
        if(lic_fld(i_lic)%lic_param%iflag_noise_type                    &
     &      .eq. iflag_from_file) then
          call load_noise_data(lic_fld(i_lic)%lic_param)
        else
          write(e_message,*)                                            &
            'Currently, noise data is only loaded from file'
          call calypso_mpi_abort(ierr_LIC, e_message)
        end if
!
        if(lic_fld(i_lic)%lic_param%iflag_kernel_type                   &
     &      .eq. iflag_from_file) then
          call load_kernel_data_from_file                               &
     &       (lic_fld(i_lic)%lic_param%kernel_image_prefix,             &
     &        lic_fld(i_lic)%lic_param%kernel_image)
        end if
!
        if(iflag_debug .gt. 0) write(*,*) 'set_control_pvr'
        call set_control_pvr                                            &
     &     (pvr_ctl_type(i_lic), group%ele_grp, group%surf_grp,         &
     &      pvr_param(i_lic)%area_def, pvr_param(i_lic)%field,          &
     &      pvr_param(i_lic)%color, pvr_param(i_lic)%colorbar)
        pvr_param(i_lic)%colorbar%iflag_opacity = 0
!
!   set transfer matrix
!
        call s_set_pvr_modelview_matrix                                 &
     &     (pvr_ctl_type(i_lic)%mat, pvr_param(i_lic)%view)
      end do
!
      end subroutine s_set_lic_controls
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine flush_each_lic_control(lic_fld)
!
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
!
      type(LIC_field_params), intent(inout) :: lic_fld
!
!
      call dealloc_lic_noise_data(lic_fld%lic_param)
      call dealloc_lic_masking_ranges(lic_fld%lic_param)
      call dealloc_lic_kernel(lic_fld%lic_param)
!
      end subroutine flush_each_lic_control
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module t_control_param_LIC_PVR
