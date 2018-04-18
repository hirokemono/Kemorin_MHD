!>@file  t_control_param_LIC_PVR.f90
!!       module t_control_param_LIC_PVR
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine read_set_each_lic_controls                           &
!!     &         (i_pvr, hd_pvr_ctl, hd_pvr_colordef, group,            &
!!     &          nod_fld, fname_pvr_ctl, pvr_ctl_type, lic_ctl_type,   &
!!     &          lic_fld, pvr_param, pvr_data)
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
      private :: set_each_lic_control, read_control_lic_pvr
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_set_each_lic_controls                             &
     &         (i_pvr, hd_pvr_ctl, hd_pvr_colordef, group,              &
     &          nod_fld, fname_pvr_ctl, pvr_ctl_type, lic_ctl_type,     &
     &          lic_fld, pvr_param, pvr_data)
!
      use t_mesh_data
      use t_phys_data
      use t_rendering_vr_image
      use bcast_control_data_4_pvr
      use set_pvr_control
!
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: i_pvr
      character(len = kchara), intent(in)  :: hd_pvr_ctl
      character(len = kchara), intent(in) :: hd_pvr_colordef
      character(len = kchara), intent(in)  :: fname_pvr_ctl
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
      type(lic_parameter_ctl), intent(inout) :: lic_ctl_type
      type(LIC_field_params), intent(inout) :: lic_fld
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
!
      integer(kind = kint) :: i_psf
!
!
      ctl_file_code = lic_ctl_file_code
      call read_control_lic_pvr(i_pvr, hd_pvr_ctl, hd_pvr_colordef,     &
     &    fname_pvr_ctl, pvr_ctl_type, lic_ctl_type)
      call read_control_modelview(i_pvr, pvr_ctl_type)
      call read_control_colormap                                        &
     &   (hd_pvr_colordef, i_pvr, pvr_ctl_type)
!
      do i_psf = 1, pvr_ctl_type%num_pvr_sect_ctl
        call read_control_pvr_section_def                               &
     &     (pvr_ctl_type%pvr_sect_ctl(i_psf))
      end do
!
      call bcast_vr_psf_ctl(pvr_ctl_type)
      call bcast_lic_control_data(lic_ctl_type)
!
      call set_each_lic_control(group%ele_grp, group%surf_grp,          &
     &    nod_fld%num_phys, nod_fld%phys_name,                          &
     &    pvr_ctl_type, lic_ctl_type, lic_fld, pvr_data, pvr_param)
!
      call dealloc_lic_count_data(pvr_ctl_type, lic_ctl_type)
!
      end subroutine read_set_each_lic_controls
!
!  ---------------------------------------------------------------------
!
      subroutine set_each_lic_control                                   &
     &       (ele_grp, surf_grp, num_nod_phys, phys_nod_name,           &
     &        pvr_ctl_type, lic_ctl_type, lic_fld, pvr_data, pvr_param)
!
      use t_group_data
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
      use t_control_data_pvr_misc
      use set_control_each_pvr
      use set_field_comp_for_viz
      use set_pvr_modelview_matrix
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: surf_grp
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
      type(lic_parameter_ctl), intent(inout) :: lic_ctl_type
      type(LIC_field_params), intent(inout) :: lic_fld
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
!
!
      if(iflag_debug .gt. 0) write(*,*) 'PVR parameters for'
      call set_pvr_file_control(pvr_ctl_type, pvr_param%file)
!
      call set_control_lic_parameter(num_nod_phys, phys_nod_name,       &
     &    lic_ctl_type, lic_fld%lic_param)
!
      call load_noise_data(lic_fld%lic_param)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_control_pvr'
      call set_control_pvr(pvr_ctl_type, ele_grp, surf_grp,             &
     &    lic_fld%area_def, pvr_data%view, pvr_param%field,             &
     &    pvr_data%color, pvr_param%colorbar)
!
!   set transfer matrix
!
      call s_set_pvr_modelview_matrix                                   &
     &   (pvr_ctl_type%mat, pvr_data%view, pvr_data%screen)
!
      end subroutine set_each_lic_control
!
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
