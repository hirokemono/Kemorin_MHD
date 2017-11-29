!
!      module set_pvr_control
!
!     Written by H. Matsui on May., 2006
!
!!      subroutine read_set_each_pvr_controls                           &
!!     &         (i_pvr, group, nod_fld, fname_pvr_ctl, pvr_ctl_struct, &
!!     &          pvr_param, pvr_data)
!!      subroutine read_control_pvr_update                              &
!!     &         (fname_pvr_ctl, pvr_ctl_struct)
!!      subroutine flush_each_pvr_control                               &
!!     &         (color_params, fld_params, field_pvr)
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
      character(len=kchara) :: hd_pvr_ctl = 'volume_rendering'
      character(len=kchara) :: hd_view_transform = 'view_transform_ctl'
      character(len=kchara) :: hd_colormap =      'colormap_ctl'
      character(len=kchara) :: hd_pvr_colordef =  'pvr_color_ctl'
!
      private :: hd_pvr_ctl
      private :: hd_view_transform, hd_colormap, hd_pvr_colordef
!
      private :: read_control_pvr, set_each_pvr_control
      private :: read_control_modelview, read_control_colormap
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_set_each_pvr_controls                             &
     &         (i_pvr, group, nod_fld, fname_pvr_ctl, pvr_ctl_struct,   &
     &          pvr_param, pvr_data)
!
      use t_mesh_data
      use t_phys_data
      use t_control_data_pvrs
      use t_rendering_vr_image
      use bcast_control_data_4_pvr
!
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: i_pvr
      character(len = kchara), intent(in)  :: fname_pvr_ctl
!
      type(pvr_ctl), intent(inout) :: pvr_ctl_struct
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_image_generator), intent(inout) :: pvr_data
!
      integer(kind = kint) :: i_psf
!
!
      ctl_file_code = pvr_ctl_file_code
      call read_control_pvr(i_pvr, fname_pvr_ctl, pvr_ctl_struct)
      call read_control_modelview(i_pvr, pvr_ctl_struct)
      call read_control_colormap(i_pvr, pvr_ctl_struct)
      do i_psf = 1, pvr_ctl_struct%num_pvr_sect_ctl
        call read_control_pvr_section_def                             &
     &     (pvr_ctl_struct%pvr_sect_ctl(i_psf))
      end do
!
      call bcast_vr_psf_ctl(pvr_ctl_struct)
!
      call set_each_pvr_control(group%ele_grp, group%surf_grp,          &
     &    nod_fld%num_phys, nod_fld%phys_name, pvr_ctl_struct,          &
     &    pvr_param%file, pvr_param%field_def, pvr_data%view,           &
     &    pvr_param%field, pvr_data%screen, pvr_data%color,             &
     &    pvr_param%colorbar)
!
      call deallocate_cont_dat_pvr(pvr_ctl_struct)
!
      end subroutine read_set_each_pvr_controls
!
!  ---------------------------------------------------------------------
!
      subroutine set_each_pvr_control                                   &
     &       (ele_grp, surf_grp, num_nod_phys, phys_nod_name,           &
     &        pvr_control, file_params, fld_params, view_params,        &
     &        field_pvr, pvr_screen, color_params, cbar_params)
!
      use t_group_data
      use t_control_params_4_pvr
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
      type(pvr_ctl), intent(inout) :: pvr_control
      type(pvr_output_parameter), intent(inout) :: file_params
      type(pvr_field_parameter), intent(inout) :: fld_params
      type(pvr_view_parameter), intent(inout) :: view_params
      type(pvr_projected_data), intent(inout) :: pvr_screen
      type(pvr_projected_field), intent(inout) :: field_pvr
      type(pvr_colormap_parameter), intent(inout) :: color_params
      type(pvr_colorbar_parameter), intent(inout) :: cbar_params
!
!
      if(iflag_debug .gt. 0) write(*,*) 'PVR parameters for'
      call set_pvr_file_control(pvr_control,                            &
     &    num_nod_phys, phys_nod_name, file_params)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_control_pvr'
      call set_control_pvr                                              &
     &   (pvr_control, ele_grp, surf_grp, num_nod_phys,                 &
     &    phys_nod_name, fld_params, view_params, field_pvr,            &
     &    color_params, cbar_params)
!
!   set transfer matrix
!
      call s_set_pvr_modelview_matrix                                   &
     &   (pvr_control%mat, view_params, pvr_screen)
!
      end subroutine set_each_pvr_control
!
!   --------------------------------------------------------------------
!
       subroutine flush_each_pvr_control                                &
      &         (color_params, fld_params, field_pvr)
!
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
!
      type(pvr_colormap_parameter), intent(inout) :: color_params
      type(pvr_field_parameter), intent(inout) :: fld_params
      type(pvr_projected_field), intent(inout) :: field_pvr
!
!
      if(field_pvr%num_sections .gt. 0) then
        call dealloc_pvr_sections(field_pvr)
      end if
!
      if(field_pvr%num_isosurf .gt. 0) then
        call dealloc_pvr_isosurfaces(field_pvr)
      end if
!
      call dealloc_pvr_element_group(fld_params)
      call dealloc_pvr_color_parameteres(color_params)
!
      end subroutine flush_each_pvr_control
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_pvr                                       &
     &         (i_pvr, fname_pvr_ctl, pvr_ctl_struct)
!
      use calypso_mpi
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: i_pvr
      character(len = kchara), intent(in)  :: fname_pvr_ctl
      type(pvr_ctl), intent(inout) :: pvr_ctl_struct
!
      if(fname_pvr_ctl .eq. 'NO_FILE') return
!
      if(my_rank .eq. 0) then
         write(*,*) 'PVR control:', i_pvr,':  ', trim(fname_pvr_ctl)
!
        open(ctl_file_code, file=fname_pvr_ctl, status='old')
        call load_ctl_label_and_line
        call read_vr_psf_ctl(hd_pvr_ctl, pvr_ctl_struct)
        close(ctl_file_code)
      end if
!
      end subroutine read_control_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_pvr_update                                &
     &         (fname_pvr_ctl, pvr_ctl_struct)
!
      use calypso_mpi
      use bcast_control_data_4_pvr
!
      character(len = kchara), intent(in)  :: fname_pvr_ctl
      type(pvr_ctl), intent(inout) :: pvr_ctl_struct
!
!
      if(fname_pvr_ctl .eq. 'NO_FILE') return
      if(my_rank .eq. 0) then
        open(ctl_file_code, file=fname_pvr_ctl, status='old')
!
        call load_ctl_label_and_line
        call read_pvr_update_flag(hd_pvr_ctl, pvr_ctl_struct)
        close(ctl_file_code)
      end if
!
      call bcast_pvr_update_flag(pvr_ctl_struct)
!
      end subroutine read_control_pvr_update
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_modelview(i_pvr, pvr_ctl_struct)
!
      use calypso_mpi
      use m_error_IDs
      use t_ctl_data_4_view_transfer
!
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_ctl), intent(inout) :: pvr_ctl_struct
!
!
      if(my_rank .gt. 0) return
!
      if(pvr_ctl_struct%view_file_ctl .eq. 'NO_FILE') then
        write(*,*)  'Modelview control:', i_pvr, ' is included'
        return
      end if
!
      write(*,*) 'Modelview control:', i_pvr,':  ',                     &
     &               trim(pvr_ctl_struct%view_file_ctl)
!
      open(ctl_file_code,                                               &
     &        file=pvr_ctl_struct%view_file_ctl, status='old')
!
      call load_ctl_label_and_line
!
      if(right_begin_flag(hd_view_transform) .gt. 0) then
        call read_view_transfer_ctl                                     &
     &        (hd_view_transform, pvr_ctl_struct%mat)
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
      subroutine read_control_colormap(i_pvr, pvr_ctl_struct)
!
      use calypso_mpi
      use m_error_IDs
      use t_ctl_data_pvr_colormap
!
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_ctl), intent(inout) :: pvr_ctl_struct
!
!
      if(my_rank .gt. 0) return
!
      if(pvr_ctl_struct%color_file_ctl .eq. 'NO_FILE') then
        write(*,*)  'Colormap control:', i_pvr, ' is included'
        return
      end if
!
      write(*,*) 'Colormap control:', i_pvr,':  ',                      &
     &                 trim(pvr_ctl_struct%color_file_ctl)
!
      open(ctl_file_code,                                               &
     &     file=pvr_ctl_struct%color_file_ctl,  status='old')
!
      call load_ctl_label_and_line
!
      if(right_begin_flag(hd_pvr_colordef) .gt. 0) then
        call read_pvr_colordef_ctl                                      &
     &     (hd_pvr_colordef, pvr_ctl_struct%color)
      else if(right_begin_flag(hd_colormap) .gt. 0) then
        call read_pvr_colordef_ctl                                      &
     &     (hd_colormap, pvr_ctl_struct%color)
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
