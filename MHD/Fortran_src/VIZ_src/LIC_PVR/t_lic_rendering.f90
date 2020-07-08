!>@file   t_lic_rendering.f90
!!@brief  module t_lic_rendering
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine check_LIC_update                                     &
!!     &         (id_control, lic_ctls, lic, iflag_update)
!!      subroutine read_ctl_lic_pvr_files_4_update(id_control, lic_ctls)
!!      subroutine LIC_initialize(fem, nod_fld, lic_ctls, lic)
!!      subroutine LIC_visualize(LIC_t, fem, jacs, nod_fld, lic)
!!      subroutine dealloc_LIC_data(lic)
!!        type(mesh_data), intent(in) :: fem
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(lic_rendering_controls), intent(inout) :: lic_ctls
!!        type(lic_volume_rendering_module), intent(inout) :: lic
!!      subroutine dealloc_LIC_data(lic)
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
      use t_IO_step_parameter
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_control_param_LIC_PVR
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
!      integer(kind = kint), parameter :: IFLAG_THROUGH =    1
!      integer(kind = kint), parameter :: IFLAG_DRAW =       0
!      integer(kind = kint), parameter :: IFLAG_TERMINATE = -1
!
!
      type lic_volume_rendering_module
!>        Structure of LIC field parameters
        type(LIC_field_params), allocatable :: lic_fld(:)
!
!>        Structure for LIC images
        type(volume_rendering_module) :: pvr
      end type lic_volume_rendering_module
!
      character(len=kchara), parameter                                  &
     &             :: hd_lic_ctl = 'LIC_rendering'
      private :: hd_lic_ctl
!
      private :: alloc_LIC_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine check_LIC_update                                       &
     &         (id_control, lic_ctls, lic, iflag_update)
!
      use set_pvr_control
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(lic_volume_rendering_module), intent(inout) :: lic
      integer(kind = kint), intent(inout) :: iflag_update
!
      character(len = kchara) :: tmpchara
!
!
      call calypso_mpi_barrier
      if(my_rank .eq. izero) then
        call read_control_pvr_update                                    &
     &     (id_control, lic_ctls%fname_lic_ctl(1),                      &
     &      hd_lic_ctl, lic_ctls%pvr_ctl_type(1))
!
        iflag_update = IFLAG_THROUGH
        if(lic_ctls%pvr_ctl_type(1)%updated_ctl%iflag .gt. 0) then
          tmpchara = lic_ctls%pvr_ctl_type(1)%updated_ctl%charavalue
          if(cmp_no_case(tmpchara, 'end')) then
            iflag_update = IFLAG_TERMINATE
          else if(lic%pvr%cflag_update .ne. tmpchara) then
            iflag_update = IFLAG_DRAW
            lic%pvr%cflag_update = tmpchara
          end if
        end if
        call reset_pvr_update_flags(lic_ctls%pvr_ctl_type(1))
      end if
      call mpi_Bcast(iflag_update, 1, CALYPSO_INTEGER, 0,               &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine check_LIC_update
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_lic_pvr_files_4_update(id_control, lic_ctls)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      type(lic_rendering_controls), intent(inout) :: lic_ctls
!
      integer(kind = kint) :: i_lic
!
!
      do i_lic = 1, lic_ctls%num_lic_ctl
        if(lic_ctls%fname_lic_ctl(i_lic) .ne. 'NO_FILE') then
          call read_control_lic_pvr_file                                &
     &     (id_control, lic_ctls%fname_lic_ctl(i_lic), hd_lic_ctl,      &
     &      lic_ctls%pvr_ctl_type(i_lic), lic_ctls%lic_ctl_type(i_lic))
        end if
      end do
!
      end subroutine read_ctl_lic_pvr_files_4_update
!
!   --------------------------------------------------------------------
!
      subroutine LIC_initialize(fem, nod_fld, lic_ctls, lic)
!
      use t_control_data_pvr_sections
      use set_pvr_control
      use each_LIC_rendering
      use rendering_and_image_nums
!
      type(mesh_data), intent(in) :: fem
      type(phys_data), intent(in) :: nod_fld
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(lic_volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_lic, ist_rdr, ist_img
!
!
      if(lic_ctls%num_lic_ctl .le. 0) then
        lic%pvr%num_pvr = 0
        return
      end if
!
      call bcast_lic_controls(lic_ctls%num_lic_ctl,                     &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%pvr%cflag_update)
!
      call count_num_rendering_and_images                               &
     &   (lic_ctls%num_lic_ctl, lic_ctls%pvr_ctl_type, lic%pvr%num_pvr, &
     &    lic%pvr%num_pvr_rendering, lic%pvr%num_pvr_images)
      call alloc_LIC_data(lic)
!
      call s_num_rendering_and_images                                   &
     &   (nprocs, lic%pvr%num_pvr, lic_ctls%pvr_ctl_type,               &
     &    lic%pvr%num_pvr_rendering, lic%pvr%num_pvr_images,            &
     &    lic%pvr%istack_pvr_render, lic%pvr%istack_pvr_images,         &
     &    lic%pvr%pvr_rgb)
!
      do i_lic = 1, lic%pvr%num_pvr
        call allocate_nod_data_4_pvr                                    &
     &     (fem%mesh%node%numnod, fem%mesh%ele%numele,                  &
     &      fem%group%surf_grp%num_grp, lic%pvr%pvr_param(i_lic)%field)
        call reset_pvr_view_parameteres(lic%pvr%pvr_param(i_lic)%view)
      end do
!
      call s_set_lic_controls(fem%group, nod_fld, lic%pvr%num_pvr,      &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%lic_fld, lic%pvr%pvr_param)
!
      do i_lic = 1, lic_ctls%num_lic_ctl
        if(lic_ctls%fname_lic_ctl(i_lic) .ne. 'NO_FILE'                 &
     &      .or. my_rank .ne. 0) then
          call dealloc_lic_count_data(lic_ctls%pvr_ctl_type(i_lic),     &
     &        lic_ctls%lic_ctl_type(i_lic))
        end if
      end do
!
      do i_lic = 1, lic%pvr%num_pvr
        call alloc_nod_vector_4_lic(fem%mesh%node%numnod,               &
     &      lic%lic_fld(i_lic)%lic_param%num_masking,                   &
     &      lic%pvr%pvr_param(i_lic)%field)
      end do
!
      do i_lic = 1, lic%pvr%num_pvr
        ist_rdr = lic%pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
        call each_PVR_initialize(i_lic, fem%mesh, fem%group,            &
     &     lic%pvr%pvr_param(i_lic)%area_def, lic%pvr%pvr_param(i_lic), &
     &     lic%pvr%pvr_proj(ist_rdr), lic%pvr%pvr_rgb(ist_img))
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine LIC_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize(LIC_t, fem, jacs, nod_fld, lic)
!
      use m_elapsed_labels_4_VIZ
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use write_PVR_image
!
      type(IO_step_param), intent(in) :: LIC_t
      type(mesh_data), intent(in) :: fem
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
!
      type(lic_volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_lic, ist_rdr, ist_img
!
!
      if(lic%pvr%num_pvr.le.0 .or. LIC_t%istep_file.le.0) return
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, lic%pvr%num_pvr
        ist_rdr = lic%pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
        call s_each_LIC_rendering                                       &
     &     (LIC_t%istep_file, fem%mesh, jacs, nod_fld,                  &
     &      lic%lic_fld(i_lic), lic%pvr%pvr_param(i_lic),               &
     &      lic%pvr%pvr_proj(ist_rdr), lic%pvr%pvr_rgb(ist_img))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      do i_lic = 1, lic%pvr%num_pvr
        ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
        if(lic%pvr%pvr_rgb(ist_img)%iflag_monitoring .gt. 0) then
          call sel_write_pvr_image_file                                 &
     &       (iminus, iminus, lic%pvr%pvr_rgb(ist_img))
        end if
      end do
      do i_lic = 1, lic%pvr%num_pvr_images
        call sel_write_pvr_image_file                                   &
     &     (iminus, LIC_t%istep_file, lic%pvr%pvr_rgb(i_lic))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, lic%pvr%num_pvr
        if(lic%pvr%pvr_param(i_lic)%view%iflag_rotate_snap .gt. 0) then
          ist_rdr = lic%pvr%istack_pvr_render(i_lic-1) + 1
          ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
          call s_each_LIC_rendering_w_rot(LIC_t%istep_file,             &
     &        fem%mesh, fem%group, jacs, nod_fld,                       &
     &        lic%lic_fld(i_lic), lic%pvr%pvr_param(i_lic),             &
     &        lic%pvr%pvr_proj(ist_rdr), lic%pvr%pvr_rgb(ist_img))
        end if
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      end subroutine LIC_visualize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_LIC_data(lic)
!
      type(lic_volume_rendering_module), intent(inout) :: lic
!
!
      call alloc_pvr_data(lic%pvr)
      allocate(lic%lic_fld(lic%pvr%num_pvr))
!
      end subroutine alloc_LIC_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_LIC_data(lic)
!
      use each_LIC_rendering
!
      integer(kind = kint) :: i_lic
      type(lic_volume_rendering_module), intent(inout) :: lic
!
!
      do i_lic = 1, lic%pvr%num_pvr
        call dealloc_each_lic_data                                      &
     &     (lic%lic_fld(i_lic), lic%pvr%pvr_param(i_lic))
      end do
      deallocate(lic%lic_fld)
!
      call dealloc_pvr_data(lic%pvr)
!
      end subroutine dealloc_LIC_data
!
!  ---------------------------------------------------------------------
!
      end module t_lic_rendering
