!>@file  t_control_param_LIC_PVR.f90
!!       module t_control_param_LIC_PVR
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine read_lic_controls                                    &
!!     &         (hd_pvr_ctl, hd_pvr_colordef, num_lic_ctl,             &
!!     &          fname_lic_ctl, pvr_ctl_type, lic_ctl_type,            &
!!     &          cflag_update)
!!        integer(kind = kint), intent(in) :: num_lic_ctl
!!        type(pvr_parameter_ctl), intent(inout)                        &
!!     &                        :: pvr_ctl_type(num_lic_ctl)
!!        type(lic_parameter_ctl), intent(inout)                        &
!!     &                        :: lic_ctl_type(num_lic_ctl)
!!      subroutine s_set_lic_controls                                   &
!!     &       (group, nod_fld, num_lic, pvr_ctl_type, lic_ctl_type,    &
!!     &        lic_fld, pvr_param, pvr_data)
!!        integer(kind = kint), intent(in) :: num_lic
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type(num_lic)
!!        type(lic_parameter_ctl), intent(in) :: lic_ctl_type(num_lic)
!!        type(LIC_field_params), intent(inout) :: lic_fld(num_lic)
!!        type(PVR_control_params), intent(inout) :: pvr_param(num_lic)
!!        type(PVR_image_generator), intent(inout) :: pvr_data(num_lic)
!!      subroutine flush_each_lic_control(lic_fld, pvr_data, pvr_param)
!!        type(LIC_field_params), intent(inout) :: lic_fld
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!!        type(PVR_control_params), intent(inout) :: pvr_param
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
      integer(kind = kint), parameter :: lic_ctl_file_code = 11
!
!>      Structure of PVR field parameters
      type LIC_field_params
!>        Structure for field parameter for PVR
        type(lic_parameters) :: lic_param
!>        Structure for rendering area by element group
        type(viz_area_parameter) :: area_def
      end type LIC_field_params
!
      private :: read_control_lic_pvr
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_lic_controls                                      &
     &         (hd_pvr_ctl, hd_pvr_colordef, num_lic_ctl,               &
     &          fname_lic_ctl, pvr_ctl_type, lic_ctl_type,              &
     &          cflag_update)
!
      use bcast_control_data_4_pvr
      use set_pvr_control
!
      integer(kind = kint), intent(in) :: num_lic_ctl
      character(len = kchara), intent(in)  :: hd_pvr_ctl
      character(len = kchara), intent(in) :: hd_pvr_colordef
      character(len = kchara), intent(in)                               &
     &                         :: fname_lic_ctl(num_lic_ctl)
!
      type(pvr_parameter_ctl), intent(inout)                            &
     &                        :: pvr_ctl_type(num_lic_ctl)
      type(lic_parameter_ctl), intent(inout)                            &
     &                        :: lic_ctl_type(num_lic_ctl)
      character(len=kchara), intent(inout) :: cflag_update
!
      integer(kind = kint) :: i_lic, i_psf
!
!
      if(pvr_ctl_type(1)%updated_ctl%iflag .gt. 0) then
        cflag_update = pvr_ctl_type(1)%updated_ctl%charavalue
      end if
!
      ctl_file_code = lic_ctl_file_code
      do i_lic = 1, num_lic_ctl
        call read_control_lic_pvr                                       &
     &     (i_lic, hd_pvr_ctl, hd_pvr_colordef, fname_lic_ctl(i_lic),   &
     &       pvr_ctl_type(i_lic), lic_ctl_type(i_lic))
        call read_control_modelview(i_lic, pvr_ctl_type(i_lic))
        call read_control_colormap                                      &
     &     (hd_pvr_colordef, i_lic, pvr_ctl_type(i_lic))
!
        do i_psf = 1, pvr_ctl_type(i_lic)%num_pvr_sect_ctl
          call read_control_pvr_section_def                             &
     &       (pvr_ctl_type(i_lic)%pvr_sect_ctl(i_psf))
        end do
!
        call bcast_vr_psf_ctl(pvr_ctl_type(i_lic))
        call bcast_lic_control_data(lic_ctl_type(i_lic))
      end do
!
      end subroutine read_lic_controls
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_lic_controls                                     &
     &       (group, nod_fld, num_lic, pvr_ctl_type, lic_ctl_type,      &
     &        lic_fld, pvr_param, pvr_data)
!
      use m_error_IDs
      use t_phys_data
      use t_group_data
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
      use t_control_data_pvr_misc
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
      type(PVR_image_generator), intent(inout) :: pvr_data(num_lic)
!
      integer(kind = kint) :: i_lic
!
!
      do i_lic = 1, num_lic
        if(iflag_debug .gt. 0) write(*,*) 'PVR parameters for'
        call set_pvr_file_control(pvr_ctl_type(i_lic),                  &
     &      pvr_param(i_lic)%file, pvr_data(i_lic)%view)
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
     &      lic_fld(i_lic)%area_def, pvr_param(i_lic)%field,            &
     &      pvr_data(i_lic)%color, pvr_param(i_lic)%colorbar)
!
!   set transfer matrix
!
        call s_set_pvr_modelview_matrix (pvr_ctl_type(i_lic)%mat,       &
     &      pvr_data(i_lic)%view, pvr_data(i_lic)%screen)
      end do
!
      end subroutine s_set_lic_controls
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine flush_each_lic_control(lic_fld, pvr_data, pvr_param)
!
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
!
      type(LIC_field_params), intent(inout) :: lic_fld
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(PVR_control_params), intent(inout) :: pvr_param
!
!
      if(pvr_param%field%num_sections .gt. 0) then
        call dealloc_pvr_sections(pvr_param%field)
      end if
!
      if(pvr_param%field%num_isosurf .gt. 0) then
        call dealloc_pvr_isosurfaces(pvr_param%field)
      end if
!
      call dealloc_lic_noise_data(lic_fld%lic_param)
      call dealloc_lic_masking_ranges(lic_fld%lic_param)
      call dealloc_lic_kernel(lic_fld%lic_param)
!
      call dealloc_pvr_element_group(lic_fld%area_def)
      call dealloc_pvr_color_parameteres(pvr_data%color)
!
      end subroutine flush_each_lic_control
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_lic_pvr                                   &
     &         (i_pvr, hd_lic_ctl, hd_lic_colordef, fname_pvr_ctl,      &
     &          pvr_ctl_type, lic_ctl_type)
!
      use calypso_mpi
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: i_pvr
      character(len = kchara), intent(in) :: hd_lic_ctl
      character(len = kchara), intent(in) :: hd_lic_colordef
      character(len = kchara), intent(in) :: fname_pvr_ctl
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
      type(lic_parameter_ctl), intent(inout) :: lic_ctl_type
!
      if(fname_pvr_ctl .eq. 'NO_FILE') return
!
      if(my_rank .eq. 0) then
         write(*,*) 'LIC control:', i_pvr,':  ', trim(fname_pvr_ctl)
!
        open(ctl_file_code, file=fname_pvr_ctl, status='old')
        call load_ctl_label_and_line
        call read_lic_pvr_ctl(hd_lic_ctl, hd_lic_colordef,              &
     &      pvr_ctl_type, lic_ctl_type)
        close(ctl_file_code)
      end if
!
      end subroutine read_control_lic_pvr
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_LIC_PVR
