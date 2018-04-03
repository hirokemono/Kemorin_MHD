!>@file   t_lic_rendering.f90
!!@brief  module t_lic_rendering
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      integer(kind = kint), function check_LIC_update(lic_ctls, lic)
!!      subroutine LIC_initialize(femmesh, ele_mesh, nod_fld, lic)
!!      subroutine LIC_visualize                                        &
!!     &         (istep_pvr, femmesh, ele_mesh, jacs, nod_fld, lic)
!!      subroutine dealloc_LIC_data(lic)
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(lic_rendering_controls), intent(inout) :: lic_ctls
!!        type(volume_rendering_module), intent(inout) :: lic
!!@endverbatim
!
      module t_lic_rendering
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_work_time
!
      use t_mesh_data
      use t_phys_data
      use t_jacobians
!
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
      use t_control_data_LIC_pvrs
      use t_volume_rendering
!
      use each_volume_rendering
!
      implicit  none
!
!
!      integer(kind = kint), parameter :: IFLAG_THROUGH = 1
!      integer(kind = kint), parameter :: IFLAG_UPDATE =  0
!      integer(kind = kint), parameter :: IFLAG_TERMINATE = -1
!
!
!      type volume_rendering_module
!!>        Character flag to update volume rendering
!        character(len=kchara) :: cflag_update
!!>        Number of volume rendering
!        integer(kind = kint) :: num_pvr = 0
!!>        Structure of LIC control parameters
!        type(PVR_control_params), allocatable :: pvr_param(:)
!!>        Structure of LIC image generation
!        type(PVR_image_generator), allocatable :: pvr_data(:)
!      end type volume_rendering_module
!
      private :: alloc_components_4_LIC
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_LIC_update(lic_ctls, lic)
!
      use set_pvr_control
      use skip_comment_f
!
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(volume_rendering_module), intent(inout) :: lic
!
      character(len = kchara) :: tmpchara
!
!
      call calypso_mpi_barrier
      call read_control_pvr_update(hd_lic_ctl,                          &
     &    lic_ctls%fname_lic_ctl(1), lic_ctls%lic_ctl_struct(1))
!
      if(my_rank .eq. izero) then
        check_LIC_update = IFLAG_THROUGH
        if(lic_ctls%lic_ctl_struct(1)%updated_ctl%iflag .gt. 0) then
          tmpchara = lic_ctls%lic_ctl_struct(1)%updated_ctl%charavalue
          if(cmp_no_case(tmpchara, 'end')) then
            check_LIC_update = IFLAG_TERMINATE
          else if(lic%cflag_update .ne. tmpchara) then
            check_LIC_update = IFLAG_UPDATE
            lic%cflag_update = tmpchara
          end if
        end if
        call reset_pvr_update_flags(lic_ctls%lic_ctl_struct(1))
      end if
      call mpi_Bcast(check_LIC_update, ione, CALYPSO_INTEGER, izero,    &
     &    CALYPSO_COMM, ierr_MPI)
      call calypso_mpi_barrier
!
      end function check_LIC_update
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_initialize                                         &
     &         (femmesh, ele_mesh, nod_fld, lic_ctls, lic)
!
      use t_control_data_pvr_misc
      use set_pvr_control
      use find_pvr_surf_domain
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_pvr
!
!
      lic%num_pvr = lic_ctls%num_lic_ctl
      if(lic%num_pvr .le. 0) return
!
      call alloc_components_4_LIC(lic)
!
      do i_pvr = 1, lic%num_pvr
        call alloc_nod_vector_4_lic                                     &
     &     (femmesh%mesh%node%numnod, femmesh%mesh%ele%numele,          &
     &      femmesh%group%surf_grp%num_grp, lic%pvr_param(i_pvr)%field)
        call reset_pvr_view_parameteres(lic%pvr_data(i_pvr)%view)
      end do
!
      if(lic_ctls%lic_ctl_struct(1)%updated_ctl%iflag .gt. 0) then
        lic%cflag_update                                                &
     &         = lic_ctls%lic_ctl_struct(1)%updated_ctl%charavalue
      end if
!
      do i_pvr = 1, lic%num_pvr
        call read_set_each_pvr_controls                                 &
     &     (i_pvr, hd_lic_ctl, hd_pvr_colordef,                         &
     &      femmesh%group, nod_fld, lic_ctls%fname_lic_ctl(i_pvr),      &
     &      lic_ctls%lic_ctl_struct(i_pvr),                             &
     &      lic%pvr_param(i_pvr), lic%pvr_data(i_pvr))
        call calypso_mpi_barrier
      end do
!
      call s_find_pvr_surf_domain                                       &
     &   (lic%num_pvr, femmesh%mesh, femmesh%group, ele_mesh,           &
     &    lic%pvr_param, lic%pvr_data)
!
      do i_pvr = 1, lic%num_pvr
        call each_PVR_initialize                                        &
     &     (i_pvr, femmesh%mesh, femmesh%group, ele_mesh,               &
     &      lic%pvr_param(i_pvr), lic%pvr_data(i_pvr))
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine LIC_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize                                          &
     &         (istep_pvr, femmesh, ele_mesh, jacs, nod_fld, lic)
!
      use cal_pvr_modelview_mat
      use each_LIC_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_pvr
!
!
      if(lic%num_pvr.le.0 .or. istep_pvr.le.0) return
!
      call start_elapsed_time(76)
      do i_pvr = 1, lic%num_pvr
        call s_each_LIC_rendering(istep_pvr,                            &
     &      femmesh%mesh, femmesh%group, ele_mesh, jacs, nod_fld,       &
     &      lic%pvr_param(i_pvr), lic%pvr_data(i_pvr))
      end do
      call end_elapsed_time(76)
!
      end subroutine LIC_visualize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_components_4_LIC(lic)
!
      type(volume_rendering_module), intent(inout) :: lic
!
!
      allocate(lic%pvr_param(lic%num_pvr))
      allocate(lic%pvr_data(lic%num_pvr))
!
      end subroutine alloc_components_4_LIC
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_LIC_data(lic)
!
      use each_LIC_rendering
!
      integer(kind = kint) :: i_pvr
      type(volume_rendering_module), intent(inout) :: lic
!
!
      do i_pvr = 1, lic%num_pvr
        call dealloc_each_lic_data                                      &
     &     (lic%pvr_param(i_pvr), lic%pvr_data(i_pvr))
      end do
      deallocate(lic%pvr_param, lic%pvr_data)
!
      end subroutine dealloc_LIC_data
!
!  ---------------------------------------------------------------------
!
      end module t_lic_rendering
