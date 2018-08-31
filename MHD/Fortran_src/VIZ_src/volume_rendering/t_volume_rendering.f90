!>@file   t_volume_rendering.f90
!!@brief  module t_volume_rendering
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Main routines for volume renderings
!!
!!@verbatim
!!      integer(kind = kint), function check_PVR_update(pvr_ctls, pvr)
!!      subroutine PVR_initialize(femmesh, ele_mesh, nod_fld, pvr)
!!      subroutine PVR_visualize                                        &
!!     &         (istep_pvr, femmesh, ele_mesh, jacs, nod_fld, pvr)
!!      subroutine dealloc_pvr_data(pvr)
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(volume_rendering_controls), intent(inout) :: pvr_ctls
!!        type(volume_rendering_module), intent(inout) :: pvr
!!@endverbatim
!
      module t_volume_rendering
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
      use t_control_data_pvrs
      use t_pvr_image_data
!
      use each_volume_rendering
!
      implicit  none
!
!
      integer(kind = kint), parameter :: IFLAG_THROUGH = 1
      integer(kind = kint), parameter :: IFLAG_UPDATE =  0
      integer(kind = kint), parameter :: IFLAG_TERMINATE = -1
!
!
      type volume_rendering_module
!>        Character flag to update volume rendering
        character(len=kchara) :: cflag_update
!>        Number of volume rendering
        integer(kind = kint) :: num_pvr = 0
!
!>        Number of rendering for volume rendering
        integer(kind = kint) :: num_pvr_rendering = 0
!
!>        Structure of PVR field parameters
        type(PVR_field_params), allocatable :: pvr_fld(:)
!>        Structure of PVR control parameters
        type(PVR_control_params), allocatable :: pvr_param(:)
!>        Structure of PVR image generation
        type(PVR_image_generator), allocatable :: pvr_data(:)
!
!>        Structure for PVR images
        type(pvr_mul_image_data) :: pvr_images
      end type volume_rendering_module
!
      private :: alloc_components_4_pvr
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_PVR_update(pvr_ctls, pvr)
!
      use set_pvr_control
      use skip_comment_f
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(volume_rendering_module), intent(inout) :: pvr
!
      character(len = kchara) :: tmpchara
!
!
      call calypso_mpi_barrier
      call read_control_pvr_update(hd_pvr_ctl,                          &
     &    pvr_ctls%fname_pvr_ctl(1), pvr_ctls%pvr_ctl_type(1))
!
      if(my_rank .eq. izero) then
        check_PVR_update = IFLAG_THROUGH
        if(pvr_ctls%pvr_ctl_type(1)%updated_ctl%iflag .gt. 0) then
          tmpchara = pvr_ctls%pvr_ctl_type(1)%updated_ctl%charavalue
          if(cmp_no_case(tmpchara, 'end')) then
            check_PVR_update = IFLAG_TERMINATE
          else if(pvr%cflag_update .ne. tmpchara) then
            check_PVR_update = IFLAG_UPDATE
            pvr%cflag_update = tmpchara
          end if
        end if
        call reset_pvr_update_flags(pvr_ctls%pvr_ctl_type(1))
      end if
      call mpi_Bcast(check_PVR_update, ione, CALYPSO_INTEGER, izero,    &
     &    CALYPSO_COMM, ierr_MPI)
      call calypso_mpi_barrier
!
      end function check_PVR_update
!
!  ---------------------------------------------------------------------
!
      subroutine PVR_initialize                                         &
     &         (femmesh, ele_mesh, nod_fld, pvr_ctls, pvr)
!
      use t_control_data_pvr_misc
      use set_pvr_control
      use find_pvr_surf_domain
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(volume_rendering_module), intent(inout) :: pvr
!
      integer(kind = kint) :: i_pvr
!
!
      if(pvr_ctls%num_pvr_ctl .le. 0) then
        pvr%num_pvr = 0
        return
      end if
!
      call read_pvr_controls                                            &
     &   (hd_pvr_ctl, hd_pvr_colordef, pvr_ctls%num_pvr_ctl,            &
     &    pvr_ctls%fname_pvr_ctl, pvr_ctls%pvr_ctl_type,                &
     &    pvr%cflag_update)
!
      call s_num_rendering_and_images                                   &
     &   (nprocs, pvr_ctls%num_pvr_ctl, pvr_ctls%pvr_ctl_type,          &
     &    pvr%num_pvr, pvr%num_pvr_rendering, pvr%pvr_images)
!
!
      call alloc_components_4_pvr(pvr)
!
      do i_pvr = 1, pvr%num_pvr
        call allocate_nod_data_4_pvr                                    &
     &     (femmesh%mesh%node%numnod, femmesh%mesh%ele%numele,          &
     &      femmesh%group%surf_grp%num_grp, pvr%pvr_param(i_pvr)%field)
        call reset_pvr_view_parameteres(pvr%pvr_data(i_pvr)%view)
      end do
!
      call s_set_pvr_controls(femmesh%group, nod_fld,                   &
     &    pvr%num_pvr, pvr_ctls%pvr_ctl_type,                           &
     &    pvr%pvr_fld, pvr%pvr_param, pvr%pvr_data)
!
      do i_pvr = 1, pvr_ctls%num_pvr_ctl
        call deallocate_cont_dat_pvr(pvr_ctls%pvr_ctl_type(i_pvr))
      end do
!
      call s_find_pvr_surf_domain                                       &
     &   (pvr%num_pvr, femmesh%mesh, femmesh%group, ele_mesh,           &
     &    pvr%pvr_fld, pvr%pvr_param, pvr%pvr_data)
!
      do i_pvr = 1, pvr%num_pvr
        call each_PVR_initialize                                        &
     &     (i_pvr, pvr%pvr_images%img(i_pvr)%irank_image_file,          &
     &      femmesh%mesh, femmesh%group, ele_mesh,                      &
     &      pvr%pvr_param(i_pvr), pvr%pvr_data(i_pvr),                  &
     &      pvr%pvr_data(i_pvr)%rgb)
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine PVR_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine PVR_visualize                                          &
     &         (istep_pvr, femmesh, ele_mesh, jacs, nod_fld, pvr)
!
      use cal_pvr_modelview_mat
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(volume_rendering_module), intent(inout) :: pvr
!
      integer(kind = kint) :: i_pvr
!
!
      if(pvr%num_pvr.le.0 .or. istep_pvr.le.0) return
!
      call start_elapsed_time(71)
      do i_pvr = 1, pvr%num_pvr
        call each_PVR_rendering                                         &
     &     (istep_pvr, pvr%pvr_images%img(i_pvr)%irank_image_file,      &
     &      femmesh%mesh, femmesh%group, ele_mesh, jacs, nod_fld,       &
     &      pvr%pvr_fld(i_pvr), pvr%pvr_param(i_pvr),                   &
     &      pvr%pvr_data(i_pvr), pvr%pvr_data(i_pvr)%rgb)
      end do
      call end_elapsed_time(71)
!
      end subroutine PVR_visualize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_components_4_pvr(pvr)
!
      type(volume_rendering_module), intent(inout) :: pvr
!
!
      allocate(pvr%pvr_fld(pvr%num_pvr))
      allocate(pvr%pvr_param(pvr%num_pvr))
      allocate(pvr%pvr_data(pvr%num_pvr))
!
      end subroutine alloc_components_4_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_data(pvr)
!
      use set_pvr_control
!
      integer(kind = kint) :: i_pvr
      type(volume_rendering_module), intent(inout) :: pvr
!
!
      do i_pvr = 1, pvr%num_pvr
        call dealloc_each_pvr_data (pvr%pvr_fld(i_pvr),                 &
     &      pvr%pvr_param(i_pvr), pvr%pvr_data(i_pvr),                  &
     &      pvr%pvr_data(i_pvr)%rgb)
      end do
      deallocate(pvr%pvr_fld, pvr%pvr_param, pvr%pvr_data)
!
      call dealloc_istack_pvr_image_4_merge(pvr%pvr_images)
!
      end subroutine dealloc_pvr_data
!
!  ---------------------------------------------------------------------
!
      end module t_volume_rendering
