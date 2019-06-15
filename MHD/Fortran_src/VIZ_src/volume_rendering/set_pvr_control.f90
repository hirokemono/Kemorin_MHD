!>@file   set_pvr_control.f90
!!@brief  module set_pvr_control
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine read_pvr_controls(hd_pvr_ctl, hd_pvr_colordef,       &
!!     &          num_pvr_ctl, fname_pvr_ctl, pvr_ctl, cflag_update)
!!        integer(kind = kint), intent(in) :: num_pvr_ctl
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl(num_pvr_ctl)
!!      subroutine s_set_pvr_controls(group, nod_fld,                   &
!!     &          num_pvr, pvr_ctl_type, pvr_param)
!!        integer(kind = kint), intent(in) :: num_pvr
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type(num_pvr)
!!        type(PVR_control_params), intent(inout) :: pvr_param(num_pvr)
!!
!!      subroutine read_control_pvr_update                              &
!!     &         (hd_pvr_ctl, fname_pvr_ctl, pvr_ctl_type)
!!      subroutine flush_each_pvr_control(pvr_param)
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!      subroutine read_control_modelview(i_pvr, pvr_ctl_type)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!!      subroutine read_control_colormap                                &
!!     &         (hd_pvr_colordef, i_pvr, pvr_ctl_type)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!!@endverbatim
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
!
      private :: hd_view_transform
!
      private :: read_control_pvr
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_controls(hd_pvr_ctl, hd_pvr_colordef,         &
     &          num_pvr_ctl, fname_pvr_ctl, pvr_ctl, cflag_update)
!
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
      character(len = kchara), intent(in)  :: hd_pvr_ctl
      character(len = kchara), intent(in) :: hd_pvr_colordef
      character(len = kchara), intent(in)                               &
     &                        :: fname_pvr_ctl(num_pvr_ctl)
!
      character(len=kchara), intent(inout) :: cflag_update
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl(num_pvr_ctl)
!
      integer(kind = kint) :: i_pvr, i_psf
!
!
      if(pvr_ctl(1)%updated_ctl%iflag .gt. 0) then
        cflag_update = pvr_ctl(1)%updated_ctl%charavalue
      end if
!
      ctl_file_code = pvr_ctl_file_code
      do i_pvr = 1, num_pvr_ctl
        call read_control_pvr(i_pvr, hd_pvr_ctl, hd_pvr_colordef,       &
     &    fname_pvr_ctl(i_pvr), pvr_ctl(i_pvr))
        call read_control_modelview(i_pvr, pvr_ctl(i_pvr))
        call read_control_colormap                                      &
     &     (hd_pvr_colordef, i_pvr, pvr_ctl(i_pvr))
!
        do i_psf = 1, pvr_ctl(i_pvr)%num_pvr_sect_ctl
          call read_control_pvr_section_def                             &
     &     (pvr_ctl(i_pvr)%pvr_sect_ctl(i_psf))
        end do
!
        call bcast_vr_psf_ctl(pvr_ctl(i_pvr))
      end do
!
      end subroutine read_pvr_controls
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_pvr_controls(group, nod_fld,                     &
     &          num_pvr, pvr_ctl_type, pvr_param)
!
      use t_group_data
      use t_phys_data
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
      use t_control_data_pvr_sections
      use set_control_each_pvr
      use set_field_comp_for_viz
      use set_pvr_modelview_matrix
!
      integer(kind = kint), intent(in) :: num_pvr
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type(num_pvr)
!
      type(PVR_control_params), intent(inout) :: pvr_param(num_pvr)
!
      integer(kind = kint) :: i_pvr
      integer(kind = kint) :: icheck_ncomp(1)
!
!
      do i_pvr = 1, num_pvr
        call set_control_pvr_movie                                      &
     &     (pvr_ctl_type(i_pvr)%movie, pvr_param(i_pvr)%view)
        call set_pvr_stereo_control                                     &
     &     (pvr_ctl_type(i_pvr), pvr_param(i_pvr)%view)
!
        call check_pvr_field_control(pvr_ctl_type(i_pvr),               &
     &      nod_fld%num_phys, nod_fld%phys_name)
!
        call set_control_field_4_pvr                                    &
     &     (pvr_ctl_type(i_pvr)%pvr_field_ctl,                          &
     &      pvr_ctl_type(i_pvr)%pvr_comp_ctl,                           &
     &      nod_fld%num_phys, nod_fld%phys_name,                        &
     &      pvr_param(i_pvr)%field_def, icheck_ncomp)
        if (icheck_ncomp(1) .gt. 1)                                     &
     &     call calypso_MPI_abort(ierr_PVR, 'set scalar for rendering')
!
        if(iflag_debug .gt. 0) write(*,*) 'set_control_pvr'
        call set_control_pvr                                            &
     &     (pvr_ctl_type(i_pvr), group%ele_grp, group%surf_grp,         &
     &      pvr_param(i_pvr)%area_def, pvr_param(i_pvr)%field,          &
     &      pvr_param(i_pvr)%color, pvr_param(i_pvr)%colorbar)
!
!   set transfer matrix
!
        call s_set_pvr_modelview_matrix                                 &
     &     (pvr_ctl_type(i_pvr)%mat, pvr_param(i_pvr)%view)
      end do
!
      end subroutine s_set_pvr_controls
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine flush_each_pvr_control(pvr_param)
!
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
!
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
      call dealloc_pvr_element_group(pvr_param%area_def)
      call dealloc_pvr_color_parameteres(pvr_param%color)
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
      open(ctl_file_code, file=pvr_ctl_type%color_file_ctl,             &
     &     status='old')
!
      do
        call load_ctl_label_and_line
        if(right_begin_flag(hd_pvr_colordef) .gt. 0) then
          call read_pvr_cmap_cbar                                       &
     &       (hd_pvr_colordef, pvr_ctl_type%cmap_cbar_c)
          exit
        end if
      end do
      close(ctl_file_code)
!
      end subroutine read_control_colormap
!
!  ---------------------------------------------------------------------
!
      end module set_pvr_control
