!
!      module set_pvr_control
!
!     Written by H. Matsui on May., 2006
!
!!      subroutine read_set_each_pvr_controls                           &
!!     &         (i_pvr, hd_pvr_ctl, hd_pvr_colordef, group,            &
!!     &          nod_fld, fname_pvr_ctl, pvr_ctl_type,                 &
!!     &          pvr_fld, pvr_param, pvr_data)
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!!        type(PVR_field_params), intent(inout) :: pvr_fld
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!!
!!      subroutine read_control_pvr_update                              &
!!     &         (hd_pvr_ctl, fname_pvr_ctl, pvr_ctl_type)
!!      subroutine flush_each_pvr_control(pvr_fld, pvr_data, pvr_param)
!!        type(PVR_field_params), intent(inout) :: pvr_fld
!!        type(PVR_control_params), intent(inout) :: pvr_data
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!      subroutine read_control_modelview(i_pvr, pvr_ctl_type)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!!      subroutine read_control_colormap                                &
!!     &         (hd_pvr_colordef, i_pvr, pvr_ctl_type)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!
      module set_pvr_control
!
      use m_precision
      use calypso_mpi
!
      use t_control_data_4_pvr
!
      implicit none
!
      integer(kind = kint), parameter :: pvr_ctl_file_code = 11
!
      character(len=kchara) :: hd_view_transform = 'view_transform_ctl'
      character(len=kchara) :: hd_colormap =      'colormap_ctl'
!
      private :: hd_view_transform, hd_colormap
!
      private :: read_control_pvr, set_each_pvr_control
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_set_each_pvr_controls                             &
     &         (i_pvr, hd_pvr_ctl, hd_pvr_colordef, group,              &
     &          nod_fld, fname_pvr_ctl, pvr_ctl_type,                   &
     &          pvr_fld, pvr_param, pvr_data)
!
      use t_mesh_data
      use t_phys_data
      use t_rendering_vr_image
      use bcast_control_data_4_pvr
!
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: i_pvr
      character(len = kchara), intent(in)  :: hd_pvr_ctl
      character(len = kchara), intent(in) :: hd_pvr_colordef
      character(len = kchara), intent(in)  :: fname_pvr_ctl
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
      type(PVR_field_params), intent(inout) :: pvr_fld
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
!
      integer(kind = kint) :: i_psf
!
!
      ctl_file_code = pvr_ctl_file_code
      call read_control_pvr(i_pvr, hd_pvr_ctl, hd_pvr_colordef,         &
     &    fname_pvr_ctl, pvr_ctl_type)
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
!
      call set_each_pvr_control(group%ele_grp, group%surf_grp,          &
     &    nod_fld%num_phys, nod_fld%phys_name, pvr_ctl_type,            &
     &    pvr_fld, pvr_data, pvr_param)
!
      call deallocate_cont_dat_pvr(pvr_ctl_type)
!
      end subroutine read_set_each_pvr_controls
!
!  ---------------------------------------------------------------------
!
      subroutine set_each_pvr_control                                   &
     &       (ele_grp, surf_grp, num_nod_phys, phys_nod_name,           &
     &        pvr_ctl_type, pvr_fld, pvr_data, pvr_param)
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
      type(PVR_field_params), intent(inout) :: pvr_fld
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
!
      integer(kind = kint) :: icheck_ncomp(1)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'PVR parameters for'
      call set_pvr_file_control(pvr_ctl_type, pvr_param%file)
      call check_pvr_field_control(pvr_ctl_type,                        &
     &    num_nod_phys, phys_nod_name)
!
      call set_control_field_4_pvr                                      &
     &   (pvr_ctl_type%pvr_field_ctl, pvr_ctl_type%pvr_comp_ctl,        &
     &    num_nod_phys, phys_nod_name, pvr_fld%field_def, icheck_ncomp)
      if (icheck_ncomp(1) .gt. 1)                                       &
     &     call calypso_MPI_abort(ierr_PVR, 'set scalar for rendering')
!
      if(iflag_debug .gt. 0) write(*,*) 'set_control_pvr'
      call set_control_pvr(pvr_ctl_type, ele_grp, surf_grp,             &
     &    pvr_fld%area_def, pvr_data%view, pvr_param%field,             &
     &    pvr_data%color, pvr_param%colorbar)
!
!   set transfer matrix
!
      call s_set_pvr_modelview_matrix                                   &
     &   (pvr_ctl_type%mat, pvr_data%view, pvr_data%screen)
!
      end subroutine set_each_pvr_control
!
!   --------------------------------------------------------------------
!
      subroutine flush_each_pvr_control(pvr_fld, pvr_data, pvr_param)
!
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
!
      type(PVR_field_params), intent(inout) :: pvr_fld
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
      call dealloc_pvr_element_group(pvr_fld%area_def)
      call dealloc_pvr_color_parameteres(pvr_data%color)
!
      end subroutine flush_each_pvr_control
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_pvr                                       &
     &         (i_pvr, hd_pvr_ctl, hd_pvr_colordef, fname_pvr_ctl,      &
     &          pvr_ctl_type)
!
      use calypso_mpi
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: i_pvr
      character(len = kchara), intent(in) :: hd_pvr_ctl
      character(len = kchara), intent(in) :: hd_pvr_colordef
      character(len = kchara), intent(in) :: fname_pvr_ctl
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!
      if(fname_pvr_ctl .eq. 'NO_FILE') return
!
      if(my_rank .eq. 0) then
         write(*,*) 'PVR control:', i_pvr,':  ', trim(fname_pvr_ctl)
!
        open(ctl_file_code, file=fname_pvr_ctl, status='old')
        call load_ctl_label_and_line
        call read_pvr_ctl(hd_pvr_ctl, hd_pvr_colordef, pvr_ctl_type)
        close(ctl_file_code)
      end if
!
      end subroutine read_control_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_pvr_update                                &
     &         (hd_pvr_ctl, fname_pvr_ctl, pvr_ctl_type)
!
      use calypso_mpi
      use bcast_control_data_4_pvr
!
      character(len = kchara), intent(in)  :: hd_pvr_ctl
      character(len = kchara), intent(in)  :: fname_pvr_ctl
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!
!
      if(fname_pvr_ctl .eq. 'NO_FILE') return
      if(my_rank .eq. 0) then
        open(ctl_file_code, file=fname_pvr_ctl, status='old')
!
        call load_ctl_label_and_line
        call read_pvr_update_flag(hd_pvr_ctl, pvr_ctl_type)
        close(ctl_file_code)
      end if
!
      call bcast_pvr_update_flag(pvr_ctl_type)
!
      end subroutine read_control_pvr_update
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_modelview(i_pvr, pvr_ctl_type)
!
      use calypso_mpi
      use m_error_IDs
      use t_ctl_data_4_view_transfer
!
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!
!
      if(my_rank .gt. 0) return
!
      if(pvr_ctl_type%view_file_ctl .eq. 'NO_FILE') then
        write(*,*)  'Modelview control:', i_pvr, ' is included'
        return
      end if
!
      write(*,*) 'Modelview control:', i_pvr,':  ',                     &
     &               trim(pvr_ctl_type%view_file_ctl)
!
      open(ctl_file_code,                                               &
     &        file=pvr_ctl_type%view_file_ctl, status='old')
!
      call load_ctl_label_and_line
!
      if(right_begin_flag(hd_view_transform) .gt. 0) then
        call read_view_transfer_ctl                                     &
     &        (hd_view_transform, pvr_ctl_type%mat)
      else
        call calypso_mpi_abort(ierr_PVR, 'Set view matrix file')
      end if
!
      close(ctl_file_code)
!
      end subroutine read_control_modelview
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_colormap                                  &
     &         (hd_pvr_colordef, i_pvr, pvr_ctl_type)
!
      use calypso_mpi
      use m_error_IDs
      use t_ctl_data_pvr_colormap
!
      character(len = kchara), intent(in) :: hd_pvr_colordef
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!
!
      if(my_rank .gt. 0) return
!
      if(pvr_ctl_type%color_file_ctl .eq. 'NO_FILE') then
        write(*,*)  'Colormap control:', i_pvr, ' is included'
        return
      end if
!
      write(*,*) 'Colormap control:', i_pvr,':  ',                      &
     &                 trim(pvr_ctl_type%color_file_ctl)
!
      open(ctl_file_code,                                               &
     &     file=pvr_ctl_type%color_file_ctl,  status='old')
!
      call load_ctl_label_and_line
!
      if(right_begin_flag(hd_pvr_colordef) .gt. 0) then
        call read_pvr_colordef_ctl                                      &
     &     (hd_pvr_colordef, pvr_ctl_type%color)
      else if(right_begin_flag(hd_colormap) .gt. 0) then
        call read_pvr_colordef_ctl                                      &
     &     (hd_colormap, pvr_ctl_type%color)
      else
        call calypso_mpi_abort(ierr_PVR, 'Set correct colormap file')
      end if
!
      close(ctl_file_code)
!
      end subroutine read_control_colormap
!
!  ---------------------------------------------------------------------
!
      end module set_pvr_control
