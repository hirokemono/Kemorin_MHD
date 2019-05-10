!>@file   t_volume_rendering.f90
!!@brief  module t_volume_rendering
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Main routines for volume renderings
!!
!!@verbatim
!!      integer(kind = kint), function check_PVR_update(pvr_ctls, pvr)
!!      subroutine PVR_initialize(fem, nod_fld, pvr)
!!      subroutine PVR_visualize(istep_pvr, fem, jacs, nod_fld, pvr)
!!      subroutine alloc_pvr_data(pvr)
!!      subroutine dealloc_pvr_data(pvr)
!!        type(mesh_data), intent(in) :: fem
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
!
!>        Number of volume rendering
        integer(kind = kint) :: num_pvr = 0
!>        Structure of PVR control parameters
        type(PVR_control_params), allocatable :: pvr_param(:)
!
!>        Number of rendering for volume rendering
        integer(kind = kint) :: num_pvr_rendering = 0
!>        Number of rendreing for volume rendering
        integer(kind = kint), allocatable :: istack_pvr_render(:)
!>        Structure for projection data
        type(PVR_projection_data), allocatable :: pvr_proj(:)
!
!>        Number of image files for volume rendering
        integer(kind = kint) :: num_pvr_images =    0
!>        Number of image files for volume rendering
        integer(kind = kint), allocatable :: istack_pvr_images(:)
!>        Structure for PVR images
        type(pvr_image_type), allocatable :: pvr_rgb(:)
      end type volume_rendering_module
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
      call mpi_Bcast(check_PVR_update, 1, CALYPSO_INTEGER, 0,           &
     &    CALYPSO_COMM, ierr_MPI)
      call calypso_mpi_barrier
!
      end function check_PVR_update
!
!  ---------------------------------------------------------------------
!
      subroutine PVR_initialize(fem, nod_fld, pvr_ctls, pvr)
!
      use t_control_data_pvr_misc
      use set_pvr_control
      use rendering_and_image_nums
!
      type(mesh_data), intent(in) :: fem
      type(phys_data), intent(in) :: nod_fld
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(volume_rendering_module), intent(inout) :: pvr
!
      integer(kind = kint) :: i_pvr, ist_rdr, ist_img
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
      call count_num_rendering_and_images                               &
     &   (pvr_ctls%num_pvr_ctl, pvr_ctls%pvr_ctl_type, pvr%num_pvr,     &
     &    pvr%num_pvr_rendering, pvr%num_pvr_images)
!
      call alloc_pvr_data(pvr)
!
      call s_num_rendering_and_images                                   &
     &   (nprocs, pvr%num_pvr, pvr_ctls%pvr_ctl_type,                   &
     &    pvr%istack_pvr_render,  pvr%istack_pvr_images,                &
     &    pvr%num_pvr_images, pvr%pvr_rgb)
!
!
      do i_pvr = 1, pvr%num_pvr
        call allocate_nod_data_4_pvr                                    &
     &     (fem%mesh%node%numnod, fem%mesh%ele%numele,                  &
     &      fem%group%surf_grp%num_grp, pvr%pvr_param(i_pvr)%field)
        call reset_pvr_view_parameteres(pvr%pvr_param(i_pvr)%view)
      end do
!
      call s_set_pvr_controls(fem%group, nod_fld, pvr%num_pvr,          &
     &    pvr_ctls%pvr_ctl_type, pvr%pvr_param)
!
      do i_pvr = 1, pvr_ctls%num_pvr_ctl
        call deallocate_cont_dat_pvr(pvr_ctls%pvr_ctl_type(i_pvr))
      end do
!
      do i_pvr = 1, pvr%num_pvr
        ist_rdr = pvr%istack_pvr_render(i_pvr-1) + 1
        ist_img = pvr%istack_pvr_images(i_pvr-1) + 1
        call each_PVR_initialize(i_pvr, fem%mesh, fem%group,            &
     &      pvr%pvr_param(i_pvr)%area_def,  pvr%pvr_param(i_pvr),       &
     &      pvr%pvr_proj(ist_rdr), pvr%pvr_rgb(ist_img))
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine PVR_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine PVR_visualize(istep_pvr, fem, jacs, nod_fld, pvr)
!
      use m_elapsed_labels_4_VIZ
      use cal_pvr_modelview_mat
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_data), intent(in) :: fem
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(volume_rendering_module), intent(inout) :: pvr
!
      integer(kind = kint) :: i_pvr, ist_rdr, ist_img
!
!
      if(pvr%num_pvr.le.0 .or. istep_pvr.le.0) return
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+1)
      do i_pvr = 1, pvr%num_pvr
        ist_rdr = pvr%istack_pvr_render(i_pvr-1) + 1
        ist_img = pvr%istack_pvr_images(i_pvr-1) + 1
        call each_PVR_rendering(istep_pvr, fem%mesh, jacs, nod_fld,     &
     &      pvr%pvr_param(i_pvr), pvr%pvr_proj(ist_rdr),                &
     &      pvr%pvr_rgb(ist_img))
      end do
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+1)
!
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+2)
      do i_pvr = 1, pvr%num_pvr
        ist_img = pvr%istack_pvr_images(i_pvr-1) + 1
        if(pvr%pvr_rgb(ist_img)%iflag_monitoring .gt. 0)                &
     &   then
          call sel_write_pvr_image_file                                 &
     &       (iminus, iminus,  pvr%pvr_rgb(ist_img))
        end if
      end do
      do i_pvr = 1, pvr%num_pvr_images
        call sel_write_pvr_image_file                                   &
     &     (iminus, istep_pvr, pvr%pvr_rgb(i_pvr))
      end do
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+2)
!
!      generate snapshot movie images
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+1)
      do i_pvr = 1, pvr%num_pvr
        if(pvr%pvr_param(i_pvr)%view%iflag_rotate_snap .gt. 0) then
          ist_rdr = pvr%istack_pvr_render(i_pvr-1) + 1
          ist_img = pvr%istack_pvr_images(i_pvr-1) + 1
          call each_PVR_rendering_w_rot(istep_pvr,                      &
     &        fem%mesh, fem%group, jacs, nod_fld,                       &
     &        pvr%pvr_param(i_pvr), pvr%pvr_proj(ist_rdr),              &
     &        pvr%pvr_rgb(ist_img))
        end if
      end do
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+1)
!
      end subroutine PVR_visualize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_data(pvr)
!
      type(volume_rendering_module), intent(inout) :: pvr
!
!
      allocate(pvr%istack_pvr_render(0:pvr%num_pvr))
      allocate(pvr%istack_pvr_images(0:pvr%num_pvr))
      pvr%istack_pvr_render = 0
      pvr%istack_pvr_images = 0
!
      allocate(pvr%pvr_param(pvr%num_pvr))
      allocate(pvr%pvr_proj(pvr%num_pvr_rendering))
      allocate(pvr%pvr_rgb(pvr%num_pvr_images))
!
      end subroutine alloc_pvr_data
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
        call dealloc_each_pvr_data(pvr%pvr_param(i_pvr))
      end do
      deallocate(pvr%pvr_param)
!
!
      do i_pvr = 1, pvr%num_pvr_images
        call dealloc_pvr_image_array_type(pvr%pvr_rgb(i_pvr))
      end do
      deallocate(pvr%pvr_rgb)
!
!
      do i_pvr = 1, pvr%num_pvr_rendering
        call flush_rendering_4_fixed_view(pvr%pvr_proj(i_pvr))
      end do
      deallocate(pvr%pvr_proj)
!
      deallocate(pvr%istack_pvr_render)
      deallocate(pvr%istack_pvr_images)
!
      end subroutine dealloc_pvr_data
!
!  ---------------------------------------------------------------------
!
      end module t_volume_rendering
