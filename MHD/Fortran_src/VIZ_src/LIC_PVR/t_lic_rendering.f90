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
!!     &                          nod_fld, lic_ctls, repart_ctl, lic)
!!      subroutine LIC_visualize(istep_lic, time, geofem, next_tbl,     &
!!     &                         nod_fld, lic, v_sol)
!!      subroutine dealloc_LIC_data(lic)
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(lic_rendering_controls), intent(inout) :: lic_ctls
!!        type(viz_repartition_ctl), intent(inout) :: repart_ctl
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
     &                          nod_fld, lic_ctls, repart_ctl, lic)
!
      use t_control_data_pvr_sections
      use set_pvr_control
      use each_LIC_rendering
      use rendering_and_image_nums
      use set_lic_controls
!
      integer(kind = kint), intent(in) :: increment_lic
      type(mesh_data), intent(in), target :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(lic_rendering_controls), intent(inout) :: lic_ctls
      type(viz_repartition_ctl), intent(inout) :: repart_ctl
      type(lic_volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_lic, ist_rdr, ist_img, nmax_masking
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
      call dealloc_control_vol_repart(repart_ctl)
!
      call alloc_pvr_data(lic%pvr)
      allocate(lic%lic_param(lic%pvr%num_pvr))
!
      do i_lic = 1, lic%pvr%num_pvr
        call alloc_iflag_pvr_boundaries(geofem%group%surf_grp,          &
     &      lic%pvr%pvr_param(i_lic)%draw_param)
        call reset_pvr_view_parameteres(lic%pvr%pvr_param(i_lic)%view)
      end do
!
      call s_set_lic_controls(geofem%group, nod_fld, lic%pvr%num_pvr,   &
     &    lic_ctls%pvr_ctl_type, lic_ctls%lic_ctl_type,                 &
     &    lic%lic_param, lic%pvr%pvr_param, lic%flag_each_repart)
!
      call count_num_rendering_and_images                               &
     &   (lic%pvr%num_pvr, lic%pvr%pvr_param,                           &
     &    lic%pvr%num_pvr_rendering, lic%pvr%num_pvr_images)
!
      call alloc_pvr_images(lic%pvr)
      call s_num_rendering_and_images(nprocs,                           &
     &    lic%pvr%num_pvr, lic%pvr%pvr_param, lic_ctls%pvr_ctl_type,    &
     &    lic%pvr%num_pvr_rendering, lic%pvr%num_pvr_images,            &
     &    lic%pvr%istack_pvr_render, lic%pvr%istack_pvr_images,         &
     &    lic%pvr%pvr_rgb)
!
      do i_lic = 1, lic%pvr%num_pvr
        if(lic_ctls%fname_lic_ctl(i_lic) .ne. 'NO_FILE'                 &
     &      .or. my_rank .ne. 0) then
          call dealloc_lic_count_data(lic_ctls%pvr_ctl_type(i_lic),     &
     &        lic_ctls%lic_ctl_type(i_lic))
        end if
      end do
!
!
      if(lic%repart_p%flag_repartition) then
!  -----  Repartition
        call s_LIC_re_partition(lic%repart_p, geofem, next_tbl,         &
     &                          lic%repart_data)
       else
        lic%repart_data%viz_fem => geofem
      end if
!
      nmax_masking = 0
      do i_lic = 1, lic%pvr%num_pvr
        nmax_masking = max(nmax_masking, lic%lic_param(i_lic)%num_masking)
      end do
!
      allocate(lic%repart_data%nod_fld_lic(1))
      call alloc_nod_vector_4_lic(geofem%mesh%node, nmax_masking,       &
     &    lic%repart_data%nod_fld_lic(1))
!
      if(lic%repart_p%flag_repartition) then
        allocate(lic%repart_data%field_lic(1))
        call alloc_nod_vector_4_lic                                     &
     &     (lic%repart_data%viz_fem%mesh%node, nmax_masking,            &
     &      lic%repart_data%field_lic(1))
      else
        lic%repart_data%field_lic => lic%repart_data%nod_fld_lic
      end if
!
      do i_lic = 1, lic%pvr%num_pvr
        ist_rdr = lic%pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
        call init_each_PVR_image(lic%pvr%pvr_param(i_lic),              &
     &                           lic%pvr%pvr_rgb(ist_img))
        call each_PVR_initialize(i_lic,                                 &
     &     lic%repart_data%viz_fem%mesh, lic%repart_data%viz_fem%group, &
     &     lic%pvr%pvr_rgb(ist_img), lic%pvr%pvr_param(i_lic),          &
     &     lic%pvr%pvr_proj(ist_rdr))
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine LIC_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine LIC_visualize(istep_lic, time, geofem, next_tbl,       &
     &                         nod_fld, lic, v_sol)
!
      use m_elapsed_labels_4_VIZ
      use cal_pvr_modelview_mat
      use each_LIC_rendering
      use write_PVR_image
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
      integer(kind = kint) :: i_lic, ist_rdr, ist_img
!
!
      if(lic%pvr%num_pvr.le.0 .or. istep_lic.le.0) return
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, lic%pvr%num_pvr
        if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
        call cal_field_4_each_lic(geofem%mesh%node, nod_fld,            &
     &      lic%lic_param(i_lic), lic%repart_data%nod_fld_lic(1))
!
        if(lic%repart_p%flag_repartition) then
          call repartition_lic_field                                    &
     &       (geofem%mesh%node, lic%repart_data%viz_fem%mesh,           &
     &        lic%repart_data%mesh_to_viz_tbl, lic%repart_data%nod_fld_lic(1),           &
     &        lic%repart_data%field_lic(1), v_sol)
        end if
!
        ist_rdr = lic%pvr%istack_pvr_render(i_lic-1) + 1
        ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
        call s_each_LIC_rendering                                       &
     &     (istep_lic, time, lic%repart_data%viz_fem,                   &
     &      lic%repart_data%field_lic(1),                               &
     &      lic%lic_param(i_lic), lic%pvr%pvr_param(i_lic),             &
     &      lic%pvr%pvr_proj(ist_rdr), lic%pvr%pvr_rgb(ist_img))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      do i_lic = 1, lic%pvr%num_pvr
        ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
        if(lic%pvr%pvr_rgb(ist_img)%iflag_monitoring .gt. 0) then
          call sel_write_pvr_image_file                                 &
     &       ((-i_lic), iminus, lic%pvr%pvr_rgb(ist_img))
        end if
      end do
      do i_lic = 1, lic%pvr%num_pvr_images
        call sel_write_pvr_image_file                                   &
     &     ((-i_lic), istep_lic, lic%pvr%pvr_rgb(i_lic))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      do i_lic = 1, lic%pvr%num_pvr
        if(lic%pvr%pvr_param(i_lic)%view%iflag_movie_mode               &
     &                                   .ne. IFLAG_NO_MOVIE) then
          if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
          call cal_field_4_each_lic(geofem%mesh%node, nod_fld,          &
     &        lic%lic_param(i_lic), lic%repart_data%nod_fld_lic(1))
!
          if(lic%repart_p%flag_repartition) then
            call repartition_lic_field                                  &
     &         (geofem%mesh%node, lic%repart_data%viz_fem%mesh,         &
     &          lic%repart_data%mesh_to_viz_tbl,                        &
     &          lic%repart_data%nod_fld_lic(1),                         &
     &          lic%repart_data%field_lic(1), v_sol)
          end if
!
          ist_rdr = lic%pvr%istack_pvr_render(i_lic-1) + 1
          ist_img = lic%pvr%istack_pvr_images(i_lic-1) + 1
          call s_each_LIC_rendering_w_rot                               &
     &       (istep_lic, time, lic%repart_data%viz_fem,                 &
     &        lic%repart_data%field_lic(1),                             &
     &        lic%lic_param(i_lic), lic%pvr%pvr_param(i_lic),           &
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
      subroutine dealloc_LIC_data(lic)
!
      use each_LIC_rendering
!
      type(lic_volume_rendering_module), intent(inout) :: lic
!
      integer(kind = kint) :: i_lic
!
      if(lic%pvr%num_pvr .le. 0) return
      do i_lic = 1, lic%pvr%num_pvr
        call dealloc_each_lic_data(lic%repart_p, lic%lic_param(i_lic),  &
     &      lic%repart_data)
      end do
      deallocate(lic%lic_param)
!
      call dealloc_pvr_and_lic_data(lic%pvr)
!
      end subroutine dealloc_LIC_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_each_lic_data(repart_p, lic_param, repart_data)
!
      use set_pvr_control
      use set_lic_controls
!
      type(volume_partioning_param), intent(in) :: repart_p
      type(lic_parameters), intent(inout) :: lic_param
      type(lic_repartioned_mesh), intent(inout) :: repart_data
!
!
      if(repart_p%flag_repartition) then
        call dealloc_nod_data_4_lic(repart_data%field_lic(1))
      else
        nullify(repart_data%field_lic)
      end if
      call dealloc_nod_data_4_lic(repart_data%nod_fld_lic(1))
      call flush_each_lic_control(lic_param)
!
      end subroutine dealloc_each_lic_data
!
!  ---------------------------------------------------------------------
!
      end module t_lic_rendering
