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
!!      subroutine LIC_initialize(increment_lic, geofem, next_tbl,      &
!!     &                          nod_fld, repart_ctl, lic_ctls, lic)
!!      subroutine anaglyph_LIC_initialize(increment_lic,               &
!!     &          geofem, next_tbl, nod_fld, repart_ctl, lic_ctls, lic)
!!      subroutine LIC_visualize(istep_lic, time, geofem, next_tbl,     &
!!     &                         nod_fld, lic, v_sol)
!!      subroutine anaglyph_LIC_visualize(istep_lic, time,              &
!!     &          geofem, next_tbl, nod_fld, lic, v_sol)
!!      subroutine dealloc_LIC_data(lic)
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(viz_repartition_ctl), intent(in) :: repart_ctl
!!        type(lic_rendering_controls), intent(inout) :: lic_ctls
!!        type(lic_volume_rendering_module), intent(inout) :: lic
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
      use t_next_node_ele_4_node
!
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
      use t_control_data_LIC_pvrs
      use t_ctl_data_volume_repart
      use t_volume_rendering
      use t_calypso_comm_table
      use t_control_param_vol_grping
      use t_vector_for_solver
      use t_LIC_re_partition
      use t_control_param_LIC
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
      type lic_volume_rendering_module
!>         Logical flag to make re-patitioning each rendering
        logical :: flag_each_repart = .FALSE.
!>         Structure for repartitiong parameter
        type(volume_partioning_param) :: repart_p
!>        Structure of repartition data
        type(lic_repartioned_mesh) :: repart_data
!
!>        Structure of LIC field parameters
        type(lic_parameters), allocatable :: lic_param(:)
!
!>        Structure for LIC images
        type(volume_rendering_module) :: pvr
      end type lic_volume_rendering_module
!
      character(len=kchara), parameter                                  &
     &             :: hd_lic_ctl = 'LIC_rendering'
      private :: hd_lic_ctl
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
      use calypso_mpi_int
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
      call calypso_mpi_bcast_one_int(iflag_update, 0)
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
      subroutine LIC_initialize(increment_lic, geofem, next_tbl,        &
     &                          nod_fld, repart_ctl, lic_ctls, lic)
!
      use t_control_data_pvr_sections
      use set_pvr_control
      use rendering_and_image_nums
      use set_lic_controls
      use select_LIC_rendering
!
      integer(kind = kint), intent(in) :: increment_lic
      type(mesh_data), intent(in), target :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(viz_repartition_ctl), intent(in) :: repart_ctl
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(lic_volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_lic, ist_img, num_img
!
!
      lic%pvr%num_pvr = lic_ctls%num_lic_ctl
      if(increment_lic .le. 0) lic%pvr%num_pvr = 0
!
      if(lic%pvr%num_pvr .le. 0) then
        lic%pvr%num_pvr = 0
        return
      end if
!
      call bcast_lic_controls(lic%pvr%num_pvr,                          &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%pvr%cflag_update)
!
      call set_ctl_param_vol_repart(repart_ctl, lic%repart_p)
!
      call alloc_pvr_data(lic%pvr)
!
      do i_lic = 1, lic%pvr%num_pvr
        call alloc_iflag_pvr_boundaries(geofem%group%surf_grp,          &
     &      lic%pvr%pvr_param(i_lic)%draw_param)
        call reset_pvr_view_parameteres(lic%pvr%pvr_param(i_lic)%view)
      end do
!
      allocate(lic%lic_param(lic%pvr%num_pvr))
      call s_set_lic_controls(geofem%group, nod_fld, lic%pvr%num_pvr,   &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%lic_param, lic%pvr%pvr_param, lic%flag_each_repart)
!
      call count_num_rendering_and_images                               &
     &   (lic%pvr%num_pvr, lic%pvr%pvr_param,                           &
     &    num_img, lic%pvr%istack_pvr_images)
      call alloc_pvr_images(num_img, num_img, lic%pvr)
!
      call set_rendering_and_image_pes(nprocs,                          &
     &    lic%pvr%num_pvr, lic%pvr%pvr_param, lic_ctls%pvr_ctl_type,    &
     &    lic%pvr%num_pvr_images, lic%pvr%istack_pvr_images,            &
     &    lic%pvr%pvr_rgb)
!
      do i_lic = 1, lic%pvr%num_pvr
        ist_img = lic%pvr%istack_pvr_images(i_lic-1)
        num_img = lic%pvr%istack_pvr_images(i_lic  ) - ist_img
        call init_each_PVR_image(num_img, lic%pvr%pvr_param(i_lic),     &
     &                           lic%pvr%pvr_rgb(ist_img+1))
      end do
!
      call LIC_init_nodal_field(geofem, lic%pvr%num_pvr, lic%lic_param, &
     &                          lic%repart_data)
!
      do i_lic = 1, lic%pvr%num_pvr
        if(lic_ctls%fname_lic_ctl(i_lic) .ne. 'NO_FILE'                 &
     &      .or. my_rank .ne. 0) then
          call dealloc_lic_count_data(lic_ctls%pvr_ctl_type(i_lic),     &
     &        lic_ctls%lic_ctl_type(i_lic))
        end if
      end do
!
      if(lic%flag_each_repart) return
      call LIC_initialize_w_shared_mesh(geofem, next_tbl,               &
     &    lic%repart_p, lic%repart_data, lic%pvr)
!
      end subroutine LIC_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_LIC_initialize(increment_lic,                 &
     &          geofem, next_tbl, nod_fld, repart_ctl, lic_ctls, lic)
!
      use t_control_data_pvr_sections
      use set_pvr_control
      use rendering_and_image_nums
      use set_lic_controls
      use select_LIC_rendering
!
      integer(kind = kint), intent(in) :: increment_lic
      type(mesh_data), intent(in), target :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(viz_repartition_ctl), intent(in) :: repart_ctl
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(lic_volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_lic
!
!
      lic%pvr%num_pvr = lic_ctls%num_lic_ctl
      if(increment_lic .le. 0) lic%pvr%num_pvr = 0
!
      if(lic%pvr%num_pvr .le. 0) then
        lic%pvr%num_pvr = 0
        return
      end if
!
      call bcast_lic_controls(lic%pvr%num_pvr,                          &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%pvr%cflag_update)
!
      call set_ctl_param_vol_repart(repart_ctl, lic%repart_p)
!
      call alloc_pvr_data(lic%pvr)
!
      do i_lic = 1, lic%pvr%num_pvr
        call alloc_iflag_pvr_boundaries(geofem%group%surf_grp,          &
     &      lic%pvr%pvr_param(i_lic)%draw_param)
        call reset_pvr_view_parameteres(lic%pvr%pvr_param(i_lic)%view)
      end do
!
      allocate(lic%lic_param(lic%pvr%num_pvr))
      call s_set_lic_controls(geofem%group, nod_fld, lic%pvr%num_pvr,   &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%lic_param, lic%pvr%pvr_param, lic%flag_each_repart)
!
      call alloc_pvr_images                                             &
     &   (lic%pvr%num_pvr, (2*lic%pvr%num_pvr), lic%pvr)
!
      call set_anaglyph_rendering_pes(nprocs, lic%pvr%num_pvr,          &
     &    lic_ctls%pvr_ctl_type, lic%pvr%pvr_rgb)
!
      do i_lic = 1, lic%pvr%num_pvr
        call init_each_PVR_image(ione, lic%pvr%pvr_param(i_lic),        &
     &                           lic%pvr%pvr_rgb(i_lic))
      end do
!
      call LIC_init_nodal_field(geofem, lic%pvr%num_pvr, lic%lic_param, &
     &                          lic%repart_data)
!
      do i_lic = 1, lic%pvr%num_pvr
        if(lic_ctls%fname_lic_ctl(i_lic) .ne. 'NO_FILE'                 &
     &      .or. my_rank .ne. 0) then
          call dealloc_lic_count_data(lic_ctls%pvr_ctl_type(i_lic),     &
     &        lic_ctls%lic_ctl_type(i_lic))
        end if
      end do
!
      if(lic%flag_each_repart) return
      call LIC_anaglyph_init_shared_mesh(geofem, next_tbl,              &
     &    lic%repart_p, lic%repart_data, lic%pvr)
!
      end subroutine anaglyph_LIC_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize(istep_lic, time, geofem, next_tbl,       &
     &                         nod_fld, lic, v_sol)
!
      use m_elapsed_labels_4_VIZ
      use select_LIC_rendering
!
      integer(kind = kint), intent(in) :: istep_lic
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: geofem
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(phys_data), intent(in) :: nod_fld
!
      type(lic_volume_rendering_module), intent(inout) :: lic
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      if(lic%pvr%num_pvr .le. 0) return
!
      if(lic%flag_each_repart) then
        call LIC_visualize_w_each_repart                                &
     &     (istep_lic, time, geofem, next_tbl, nod_fld, lic%repart_p,   &
     &      lic%repart_data, lic%pvr, lic%lic_param, v_sol)
      else
        call LIC_visualize_w_shared_mesh                                &
     &     (istep_lic, time, geofem, nod_fld, lic%repart_p,             &
     &      lic%repart_data, lic%pvr, lic%lic_param, v_sol)
      end if
!
      end subroutine LIC_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_LIC_visualize(istep_lic, time,                &
     &          geofem, next_tbl, nod_fld, lic, v_sol)
!
      use m_elapsed_labels_4_VIZ
      use select_LIC_rendering
!
      integer(kind = kint), intent(in) :: istep_lic
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: geofem
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(phys_data), intent(in) :: nod_fld
!
      type(lic_volume_rendering_module), intent(inout) :: lic
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      if(lic%pvr%num_pvr .le. 0) return
!
      if(lic%flag_each_repart) then
        call LIC_anaglyph_w_each_repart                                 &
     &     (istep_lic, time, geofem, next_tbl, nod_fld, lic%repart_p,   &
     &      lic%repart_data, lic%pvr, lic%lic_param, v_sol)
      else
        call LIC_anaglyph_w_shared_mesh                                 &
     &     (istep_lic, time, geofem, nod_fld, lic%repart_p,             &
     &      lic%repart_data, lic%pvr, lic%lic_param, v_sol)
      end if
!
      end subroutine anaglyph_LIC_visualize
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_LIC_data(lic)
!
      use each_LIC_rendering
      use set_lic_controls
!
      type(lic_volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_lic
!
      if(lic%pvr%num_pvr .le. 0) return
      call dealloc_LIC_each_mesh(lic%repart_p, lic%repart_p,            &
     &                           lic%repart_data)
      call dealloc_LIC_nodal_field(lic%pvr%num_pvr, lic%lic_param,      &
     &                             lic%repart_data)
!
      do i_lic = 1, lic%pvr%num_pvr
        call flush_each_lic_control(lic%lic_param(i_lic))
      end do
      deallocate(lic%lic_param)
!
      call dealloc_pvr_and_lic_data(lic%pvr)
!
      end subroutine dealloc_LIC_data
!
!  ---------------------------------------------------------------------
!
      end module t_lic_rendering
