!
!      module set_pvr_control
!
!     Written by H. Matsui on May., 2006
!
!!      subroutine set_each_pvr_control                                 &
!!     &       (ele_grp, surf_grp, num_nod_phys, phys_nod_name,         &
!!     &        pvr_control, file_params, fld_params, view_params,      &
!!     &        field_pvr, color_params, cbar_params)
!!      subroutine read_control_pvr_update(i_pvr)
!!      subroutine read_control_pvr(i_pvr)
!!      subroutine flush_each_pvr_control                               &
!!     &         (color_params, fld_params, field_pvr)
!
      module set_pvr_control
!
      use m_precision
      use calypso_mpi
!
      use m_control_data_pvrs
      use t_control_data_4_pvr
!
      implicit none
!
      integer(kind = kint), parameter :: pvr_ctl_file_code = 11
!
      character(len=kchara) :: hd_pvr_ctl = 'volume_rendering'
      character(len=kchara) :: hd_view_transform = 'view_transform_ctl'
      character(len=kchara) :: hd_pvr_colordef =  'pvr_color_ctl'
!
      private :: hd_pvr_ctl
      private :: hd_view_transform, hd_pvr_colordef
!
!  ---------------------------------------------------------------------
!
      contains
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
      subroutine read_control_pvr(i_pvr)
!
      use calypso_mpi
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: i_pvr
!
      if(fname_pvr_ctl(i_pvr) .eq. 'NO_FILE') return
!
      if(my_rank .eq. 0) then
         write(*,*) 'PVR control:', i_pvr,':  ',                        &
     &                      trim( fname_pvr_ctl(i_pvr) )
!
        open(pvr_ctl_file_code, file=fname_pvr_ctl(i_pvr),              &
     &       status='old')
        call load_ctl_label_and_line
        call read_vr_psf_ctl(hd_pvr_ctl, pvr_ctl_struct(i_pvr))
        close(pvr_ctl_file_code)
      end if
!
      call bcast_vr_psf_ctl(pvr_ctl_struct(i_pvr))
!
      end subroutine read_control_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_pvr_update(i_pvr)
!
      use calypso_mpi
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: i_pvr
!
!
      if(fname_pvr_ctl(i_pvr) .eq. 'NO_FILE') return
      if(my_rank .eq. 0) then
        open(pvr_ctl_file_code, file=fname_pvr_ctl(i_pvr),              &
     &       status='old')
!
        call load_ctl_label_and_line
        call read_pvr_update_flag(hd_pvr_ctl, pvr_ctl_struct(i_pvr))
        close(pvr_ctl_file_code)
      end if
!
      call bcast_pvr_update_flag(pvr_ctl_struct(i_pvr))
!
      end subroutine read_control_pvr_update
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_modelview(i_pvr)
!
      use calypso_mpi
      use m_error_IDs
      use t_ctl_data_4_view_transfer
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: i_pvr
!
      if(pvr_ctl_struct(i_pvr)%view_file_ctl .eq. 'NO_FILE') then
        if(my_rank .eq. 0) write(*,*)  'Modelview control:', i_pvr,     &
     &                               ' is included'
        return
      end if
!
      if(my_rank .eq. 0) then
        write(*,*) 'Modelview control:', i_pvr,':  ',                   &
     &                 trim(pvr_ctl_struct(i_pvr)%view_file_ctl)
!
        open(pvr_ctl_file_code,                                         &
     &        file=pvr_ctl_struct(i_pvr)%view_file_ctl, status='old')
!
        call load_ctl_label_and_line
!
        if(right_begin_flag(hd_view_transform) .gt. 0) then
          call read_view_transfer_ctl                                   &
     &        (hd_view_transform, pvr_ctl_struct(i_pvr)%mat)
        else
          call calypso_mpi_abort(ierr_PVR, 'Set view matrix file')
        end if
!
        close(pvr_ctl_file_code)
      end if
!
      call bcast_view_transfer_ctl(pvr_ctl_struct(i_pvr)%mat)
!
      end subroutine read_control_modelview
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_colormap(i_pvr)
!
      use calypso_mpi
      use m_error_IDs
      use t_ctl_data_pvr_colormap
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: i_pvr
!
      if(pvr_ctl_struct(i_pvr)%color_file_ctl .eq. 'NO_FILE') then
        if(my_rank .eq. 0) write(*,*)  'Colormap control:', i_pvr,      &
     &                               ' is included'
        return
      end if
!
      if(my_rank .eq. 0) then
        write(*,*) 'Colormap control:', i_pvr,':  ',                    &
     &                 trim(pvr_ctl_struct(i_pvr)%color_file_ctl)
!
        open(pvr_ctl_file_code,                                         &
     &     file=pvr_ctl_struct(i_pvr)%color_file_ctl,  status='old')
!
        call load_ctl_label_and_line
!
        if(right_begin_flag(hd_pvr_colordef) .gt. 0) then
          call read_pvr_colordef_ctl                                    &
     &       (hd_pvr_colordef, pvr_ctl_struct(i_pvr)%color)
        else
          call calypso_mpi_abort(ierr_PVR, 'Set correct colormap file')
        end if
!
        close(pvr_ctl_file_code)
      end if
!
      call bcast_pvr_colordef_ctl(pvr_ctl_struct(i_pvr)%color)
!
      end subroutine read_control_colormap
!
!  ---------------------------------------------------------------------
!
      end module set_pvr_control
