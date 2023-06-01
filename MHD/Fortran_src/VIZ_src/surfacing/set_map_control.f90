!>@file   set_map_control.f90
!!@brief  module set_map_control
!!
!!@author H. Matsui
!!@date Programmed in May., 2006
!!@n    Modified in  June, 1015
!
!>@brief Structure for parallel sectioned data
!!
!!@verbatim
!!      subroutine s_set_map_control(num_psf, group, nod_fld,           &
!!     &          map_ctls, psf_param, psf_def, psf_mesh, psf_file_IO)
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(map_rendering_controls), intent(inout) :: map_ctls
!!        type(psf_parameters), intent(inout) :: psf_param(num_psf)
!!        type(section_define), intent(inout) :: psf_def(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!@endverbatim
!
      module set_map_control
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_map_control(num_psf, group, nod_fld,             &
     &          map_ctls, psf_param, psf_def, psf_mesh, psf_file_IO)
!
      use calypso_mpi
      use t_read_control_elements
      use t_control_data_maps
      use t_mesh_data
      use t_phys_data
      use t_control_data_4_psf
      use t_psf_patch_data
      use t_file_IO_parameter
      use t_control_params_4_psf
!
      use set_field_comp_for_viz
      use mpi_abort_by_missing_zlib
!
      integer(kind= kint), intent(in) :: num_psf
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      type(map_rendering_controls), intent(inout) :: map_ctls
      type(psf_parameters), intent(inout) :: psf_param(num_psf)
      type(section_define), intent(inout) :: psf_def(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
      type(field_IO_params), intent(inout)  :: psf_file_IO(num_psf)
!
      integer(kind = kint) :: i, ierr
!
!
      do i = 1, num_psf
        call count_control_4_psf(my_rank, map_ctls%psf_ctl_struct(i),   &
     &      group%ele_grp, nod_fld%num_phys, nod_fld%phys_name,         &
     &      psf_mesh(i)%field, psf_param(i), psf_file_IO(i), ierr)
!
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
        call mpi_abort_by_no_zlib_in_fld(psf_file_IO(i)%file_prefix,    &
     &                                   psf_file_IO(i)%iflag_format)
      end do
!
      do i = 1, num_psf
        call alloc_phys_name(psf_mesh(i)%field)
        call set_control_4_psf                                          &
     &     (map_ctls%psf_ctl_struct(i), group%ele_grp, group%surf_grp,  &
     &      nod_fld%num_phys, nod_fld%phys_name,                        &
     &      psf_mesh(i)%field,  psf_param(i), psf_def(i), ierr)
        if(ierr.gt.0) call calypso_MPI_abort(ierr, e_message)
!
        call dealloc_cont_dat_4_psf(map_ctls%psf_ctl_struct(i))
!
        call count_total_comps_4_viz(psf_mesh(i)%field)
      end do
!
      call dealloc_map_ctl_stract(map_ctls)
!
      end subroutine s_set_map_control
!
!   --------------------------------------------------------------------
!
      end module set_map_control
