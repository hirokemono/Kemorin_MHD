!>@file   set_pvr_control.f90
!!@brief  module set_pvr_control
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine read_control_pvr_file(id_control, fname_pvr_ctl,     &
!!     &          hd_pvr_ctl, pvr_ctl_type)
!!      subroutine bcast_pvr_controls                                   &
!!     &         (num_pvr_ctl, pvr_ctl, cflag_update)
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
!!     &         (id_control, fname_pvr_ctl, hd_pvr_ctl, pvr_ctl_type)
!!      subroutine flush_each_pvr_control(pvr_param)
!!        type(PVR_control_params), intent(inout) :: pvr_param
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_controls                                     &
     &         (num_pvr_ctl, pvr_ctl, cflag_update)
!
      use read_control_pvr_modelview
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
!
      character(len=kchara), intent(inout) :: cflag_update
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl(num_pvr_ctl)
!
      integer(kind = kint) :: i_pvr
!
!
      if(pvr_ctl(1)%updated_ctl%iflag .gt. 0) then
        cflag_update = pvr_ctl(1)%updated_ctl%charavalue
      end if
!
      do i_pvr = 1, num_pvr_ctl
        call bcast_vr_psf_ctl(pvr_ctl(i_pvr))
      end do
!
      end subroutine bcast_pvr_controls
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
      subroutine read_control_pvr_file(id_control, fname_pvr_ctl,       &
     &          hd_pvr_ctl, pvr_ctl_type)
!
      use read_pvr_control
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_pvr_ctl
      character(len = kchara), intent(in) :: hd_pvr_ctl
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!
      type(buffer_for_control) :: c_buf1
!
!
      write(*,*) 'PVR control:  ', trim(fname_pvr_ctl)
!
      open(id_control, file=fname_pvr_ctl, status='old')
      do
        call load_one_line_from_control(id_control, c_buf1)
        call read_pvr_ctl(id_control, hd_pvr_ctl,                       &
     &                    pvr_ctl_type, c_buf1)
        if(pvr_ctl_type%i_pvr_ctl .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_control_pvr_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_pvr_update                                &
     &         (id_control, fname_pvr_ctl, hd_pvr_ctl, pvr_ctl_type)
!
      use read_pvr_control
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in)  :: fname_pvr_ctl
      character(len = kchara), intent(in)  :: hd_pvr_ctl
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!
      type(buffer_for_control) :: c_buf1
!
      if(fname_pvr_ctl .eq. 'NO_FILE') return
      open(id_control, file=fname_pvr_ctl, status='old')
      pvr_ctl_type%i_pvr_ctl = 0
!
      do
        call load_one_line_from_control(id_control, c_buf1)
        call read_pvr_update_flag                                       &
     &     (id_control, hd_pvr_ctl, pvr_ctl_type, c_buf1)
        if(pvr_ctl_type%i_pvr_ctl .gt. 0) exit
      end do
      close(id_control)
!
      call bcast_pvr_update_flag(pvr_ctl_type)
!
      end subroutine read_control_pvr_update
!
!  ---------------------------------------------------------------------
!
      end module set_pvr_control
