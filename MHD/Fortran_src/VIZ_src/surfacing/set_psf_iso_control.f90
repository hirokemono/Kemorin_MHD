!>@file   set_psf_iso_control.f90
!!@brief  module set_psf_iso_control
!!
!!@author H. Matsui
!!@date Programmed in May., 2006
!!@n    Modified in  June, 1015
!
!>@brief Structure for parallel sectioned data
!!
!!@verbatim
!!      subroutine dealloc_psf_field_name(num_psf, psf_mesh)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine set_psf_control(num_psf, group, nod_fld,             &
!!     &          psf_ctls, psf_param, psf_def, psf_mesh, psf_file_IO)
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(section_controls), intent(inout) :: psf_ctls
!!        type(psf_parameters), intent(inout) :: psf_param(num_psf)
!!        type(section_define), intent(inout) :: psf_def(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine set_iso_control(num_iso, group, nod_fld,             &
!!     &          iso_ctls, iso_param, iso_def, iso_mesh, iso_file_IO)
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!        type(psf_parameters), intent(inout) :: iso_param(num_iso)
!!        type(isosurface_define), intent(inout) :: iso_def(num_iso)
!!        type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!!        type(field_IO_params), intent(inout) :: iso_file_IO(num_iso)
!!
!!      subroutine read_control_4_psf_file                              &
!!     &         (id_control, fname_psf_ctl, psf_ctl_struct)
!!      subroutine read_control_4_iso_file                              &
!!     &         (id_control, fname_iso_ctl, iso_ctl_struct)
!!@endverbatim
!
      module set_psf_iso_control
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!     Top level
      character(len=kchara), parameter                                  &
     &             :: hd_section_ctl = 'cross_section_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
!
!      Deprecated labels
      character(len=kchara), parameter                                  &
     &             :: hd_psf_ctl = 'surface_rendering'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_ctl = 'isosurf_rendering'
      private :: hd_section_ctl, hd_psf_ctl
      private :: hd_isosurf_ctl, hd_iso_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_field_name(num_psf, psf_mesh)
!
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_psf
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call dealloc_phys_name_type(psf_mesh(i)%field)
      end do
!
      end subroutine dealloc_psf_field_name
!
!  ---------------------------------------------------------------------
!
      subroutine set_psf_control(num_psf, group, nod_fld,               &
     &          psf_ctls, psf_param, psf_def, psf_mesh, psf_file_IO)
!
      use calypso_mpi
      use m_read_control_elements
      use t_control_data_sections
      use t_mesh_data
      use t_phys_data
      use t_control_data_4_psf
      use t_psf_patch_data
      use t_file_IO_parameter
      use t_control_params_4_psf
!
      use set_field_comp_for_viz
!
      integer(kind= kint), intent(in) :: num_psf
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      type(section_controls), intent(inout) :: psf_ctls
      type(psf_parameters), intent(inout) :: psf_param(num_psf)
      type(section_define), intent(inout) :: psf_def(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
      type(field_IO_params), intent(inout)  :: psf_file_IO(num_psf)
!
      integer(kind = kint) :: i, ierr
!
!
      do i = 1, num_psf
        call bcast_psf_control_data(psf_ctls%psf_ctl_struct(i))
      end do
      do i = 1, num_psf
        call count_control_4_psf(psf_ctls%psf_ctl_struct(i),            &
     &      group%ele_grp, nod_fld%num_phys, nod_fld%phys_name,         &
     &      psf_mesh(i)%field, psf_param(i), psf_file_IO(i), ierr)
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
      end do
!
      do i = 1, num_psf
        call alloc_phys_name_type(psf_mesh(i)%field)
        call set_control_4_psf                                          &
     &     (psf_ctls%psf_ctl_struct(i), group%ele_grp, group%surf_grp,  &
     &      nod_fld%num_phys, nod_fld%phys_name,                        &
     &      psf_mesh(i)%field,  psf_param(i), psf_def(i), ierr)
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
!
        call dealloc_cont_dat_4_psf(psf_ctls%psf_ctl_struct(i))
!
        call count_total_comps_4_viz(psf_mesh(i)%field)
      end do
!
      call dealloc_psf_ctl_stract(psf_ctls)
!
      end subroutine set_psf_control
!
!   --------------------------------------------------------------------
!
      subroutine set_iso_control(num_iso, group, nod_fld,               &
     &          iso_ctls, iso_param, iso_def, iso_mesh, iso_file_IO)
!
      use calypso_mpi
      use m_read_control_elements
      use t_control_data_isosurfaces
      use t_control_data_sections
      use t_control_params_4_iso
      use t_mesh_data
      use t_group_data
      use t_phys_data
      use t_control_data_4_iso
      use t_psf_patch_data
      use t_file_IO_parameter
!
      use set_field_comp_for_viz
!
      integer(kind= kint), intent(in) :: num_iso
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      type(isosurf_controls), intent(inout) :: iso_ctls
      type(psf_parameters), intent(inout) :: iso_param(num_iso)
      type(isosurface_define), intent(inout) :: iso_def(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
      type(field_IO_params), intent(inout) :: iso_file_IO(num_iso)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_iso
        call bcast_iso_control_data(iso_ctls%iso_ctl_struct(i))
      end do
      do i = 1, num_iso
        call count_control_4_iso(iso_ctls%iso_ctl_struct(i),            &
     &      group%ele_grp, nod_fld%num_phys, nod_fld%phys_name,         &
     &      iso_mesh(i)%field, iso_param(i), iso_def(i),                &
     &      iso_file_IO(i))
      end do
!
      do i = 1, num_iso
        call alloc_phys_name_type(iso_mesh(i)%field)
        call set_control_4_iso(iso_ctls%iso_ctl_struct(i),              &
     &      group%ele_grp, nod_fld%num_phys, nod_fld%phys_name,         &
     &      iso_mesh(i)%field, iso_param(i), iso_def(i))
        call dealloc_cont_dat_4_iso(iso_ctls%iso_ctl_struct(i))
!
        call count_total_comps_4_viz(iso_mesh(i)%field)
      end do
!
      call dealloc_iso_ctl_stract(iso_ctls)
!
!
      if(iflag_debug .gt. 0) then
        do i = 1, num_iso
          write(*,*) 'id_isosurf_data', i,                              &
     &        iso_def(i)%id_isosurf_data, iso_def(i)%id_isosurf_comp
        end do
      end if
!
      end subroutine set_iso_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_4_psf_file                                &
     &         (id_control, fname_psf_ctl, psf_ctl_struct)
!
      use t_read_control_elements
      use t_control_data_4_psf
!
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_psf_ctl
      type(psf_ctl), intent(inout) :: psf_ctl_struct
!
      type(buffer_for_control) :: c_buf1
!
!
      write(*,*) 'read section control file: ', trim(fname_psf_ctl)
      open(id_control, file=fname_psf_ctl, status='old')
!
      do
        call load_one_line_from_control(id_control, c_buf1)
        call read_psf_control_data(id_control, hd_section_ctl,          &
     &      psf_ctl_struct, c_buf1)
        call read_psf_control_data(id_control, hd_psf_ctl,              &
     &      psf_ctl_struct, c_buf1)
        if(psf_ctl_struct%i_psf_ctl .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_control_4_psf_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_iso_file                                &
     &         (id_control, fname_iso_ctl, iso_ctl_struct)
!
      use t_read_control_elements
      use t_control_data_4_iso
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_iso_ctl
      type(iso_ctl), intent(inout) :: iso_ctl_struct
!
      type(buffer_for_control) :: c_buf1
!
!
      write(*,*) 'read isosurface control file: ', trim(fname_iso_ctl)
      open(id_control, file=fname_iso_ctl, status='old')
!
      do
        call load_one_line_from_control(id_control, c_buf1)
        call read_iso_control_data                                      &
     &     (id_control, hd_isosurf_ctl, iso_ctl_struct, c_buf1)
        call read_iso_control_data                                      &
     &     (id_control, hd_iso_ctl, iso_ctl_struct, c_buf1)
        if(iso_ctl_struct%i_iso_ctl .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_control_4_iso_file
!
!  ---------------------------------------------------------------------
!
      end module set_psf_iso_control
