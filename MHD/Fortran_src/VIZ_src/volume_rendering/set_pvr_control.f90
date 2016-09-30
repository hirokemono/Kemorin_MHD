!
!      module set_pvr_control
!
!     Written by H. Matsui on May., 2006
!
!!      subroutine set_each_pvr_control                                 &
!!     &       (ele_grp, surf_grp, num_nod_phys, phys_nod_name,         &
!!     &        pvr_control, file_params, fld_params, view_params,      &
!!     &        field_pvr, color_params, cbar_params)
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
      integer(kind = kint) :: i_psf
!
!
      do i_psf = 1, pvr_control%num_pvr_sect_ctl
        call read_control_pvr_section_def                               &
     &     (pvr_control%pvr_sect_ctl(i_psf))
      end do
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
      subroutine read_control_pvr(i_pvr)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: i_pvr
!
      if(fname_pvr_ctl(i_pvr) .eq. 'NO_FILE') return
!
      call reset_pvr_control_flags(pvr_ctl_struct(i_pvr))
      if(my_rank .eq. 0) write(*,*) 'PVR control:', i_pvr,':  ',        &
     &                      trim( fname_pvr_ctl(i_pvr) )
!
      open(pvr_ctl_file_code, file=fname_pvr_ctl(i_pvr), status='old')
      call read_control_data_pvr(pvr_ctl_struct(i_pvr))
      close(pvr_ctl_file_code)
!
      end subroutine read_control_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_modelview(i_pvr)
!
      use calypso_mpi
      use t_ctl_data_4_view_transfer
!
      integer(kind = kint), intent(in) :: i_pvr
!
      if(pvr_ctl_struct(i_pvr)%view_file_ctl .eq. 'NO_FILE') then
        if(my_rank .eq. 0) write(*,*)  'Modelview control:', i_pvr,     &
     &                               ' is included'
        return
      end if
!
      if(my_rank .eq. 0) write(*,*) 'Modelview control:', i_pvr,':  ',  &
     &                 trim(pvr_ctl_struct(i_pvr)%view_file_ctl)
!
      open(pvr_ctl_file_code, file=pvr_ctl_struct(i_pvr)%view_file_ctl, &
     &     status='old')
      call read_control_data_modelview(pvr_ctl_struct(i_pvr)%mat)
      close(pvr_ctl_file_code)
!
      end subroutine read_control_modelview
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_colormap(i_pvr)
!
      use calypso_mpi
      use t_ctl_data_pvr_colormap
!
      integer(kind = kint), intent(in) :: i_pvr
!
      if(pvr_ctl_struct(i_pvr)%color_file_ctl .eq. 'NO_FILE') then
        if(my_rank .eq. 0) write(*,*)  'Colormap control:', i_pvr,      &
     &                               ' is included'
        return
      end if
!
      if(my_rank .eq. 0) write(*,*) 'Colormap control:', i_pvr,':  ',   &
     &                 trim(pvr_ctl_struct(i_pvr)%color_file_ctl)
!
      open(pvr_ctl_file_code,                                           &
     &     file=pvr_ctl_struct(i_pvr)%color_file_ctl,  status='old')
      call read_control_data_colormap(pvr_ctl_struct(i_pvr)%color)
      close(pvr_ctl_file_code)
!
      end subroutine read_control_colormap
!
!  ---------------------------------------------------------------------
!
      end module set_pvr_control
