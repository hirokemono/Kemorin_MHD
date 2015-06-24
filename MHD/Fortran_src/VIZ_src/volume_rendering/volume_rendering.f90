!
!      module volume_rendering
!
!      Written by H. Matsui on July, 2006
!
!      subroutine pvr_init(numnod, numele, numsurf,                     &
!     &          nnod_4_surf, inod_smp_stack, xx,                       &
!     &          e_multi, ie_surf, isf_4_ele, iele_4_surf,              &
!     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,   &
!     &          num_nod_phys, phys_nod_name)
!
!      subroutine pvr_main(istep_pvr, numnod, numele, numsurf,          &
!     &         nnod_4_ele, nnod_4_surf, inod_smp_stack, iele_smp_stack,&
!     &         xx, radius, a_radius, s_cylinder, a_s_cylinder, ie,     &
!     &         a_vol_ele, e_multi, ie_surf, isf_4_ele, iele_4_surf,    &
!     &         ntot_int_3d, dnx, xjac, num_nod_phys, num_tot_nod_phys, &
!     &         istack_nod_component, d_nod)
!
!
      module volume_rendering
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_control_params_4_pvr
      use m_geometry_constants
      use m_mesh_outline_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
      use set_default_pvr_params
!
      use m_geometries_in_pvr_screen
      use set_position_pvr_screen
      use find_pvr_surf_domain
      use set_pvr_ray_start_point
!
      implicit  none
!
      type(pvr_bounds_surf_ctl), allocatable, save :: pvr_bound(:)
!
      type(pvr_ray_start_type), save :: pvr_start
      type(pvr_image_type), save :: pvr_img
!
      type(pvr_projected_type), save :: projected
      type(pvr_pixel_position_type), allocatable, save :: pixel_xy(:)
!
      private :: pvr_start, pvr_img
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine pvr_init(numnod, numele, numsurf,                      &
     &          nnod_4_surf, inod_smp_stack, xx,                        &
     &          e_multi, ie_surf, isf_4_ele, iele_4_surf,               &
     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,    &
     &          num_nod_phys, phys_nod_name)
!
      use set_pvr_control
      use cal_pvr_modelview_mat
      use cal_pvr_projection_mat
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: e_multi(numele)
!
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer(kind=kint), intent(in) :: num_mat, num_mat_bc
      integer(kind=kint), intent(in) :: mat_istack(0:num_mat)
      integer(kind=kint), intent(in) :: mat_item(num_mat_bc)
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
!
      integer(kind = kint) :: i_pvr
!
!
      call s_set_pvr_control(num_mat, mat_name,                         &
     &    num_nod_phys, phys_nod_name)
!
      allocate(pvr_bound(num_pvr))
      allocate(pixel_xy(num_pvr))
!
      call allocate_node_position_pvr(numnod, numele, projected)
      call allocate_nod_data_4_pvr(num_pvr, projected)
      call s_find_pvr_surf_domain(numele, numsurf, e_multi,             &
     &          isf_4_ele, iele_4_surf, num_mat, num_mat_bc,            &
     &          mat_istack, mat_item, pvr_bound, projected)
!
      call copy_node_position_for_pvr(numnod, inod_smp_stack, xx,       &
     &    projected%nnod_pvr, projected%istack_nod_pvr,                 &
     &    projected%x_nod_sim)
!
      do i_pvr = 1, num_pvr
        call copy_node_position_pvr_domain(numnod, numele,              &
     &      numsurf, nnod_4_surf, xx, ie_surf, isf_4_ele,               &
     &      pvr_bound(i_pvr)%num_pvr_surf,                              &
     &      pvr_bound(i_pvr)%item_pvr_surf, pvr_bound(i_pvr)%xx_nod)
      end do
!
      call allocate_mesh_outline_pvr
!
!
      do i_pvr = 1, num_pvr
        call cal_mesh_outline_pvr(i_pvr, numnod, xx)
        call check_pvr_parameters(i_pvr, view_params(i_pvr))
!
        if(iflag_debug .gt. 0) write(*,*) 'set_pixel_on_pvr_screen'
        call set_pixel_on_pvr_screen                                    &
     &     (view_params(i_pvr)%n_pvr_pixel, pixel_xy(i_pvr))
!
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_matrix'
        call set_pvr_projection_matrix(i_pvr, view_params(i_pvr))
!        call set_pvr_orthogonal_params(i_pvr, view_params(i_pvr))
!
        if(iflag_debug .gt. 0) write(*,*) 'cal_pvr_modelview_matrix'
        call cal_pvr_modelview_matrix(i_pvr, izero, view_params(i_pvr), &
     &      color_params(i_pvr))
!
        if(view_params(i_pvr)%iflag_rotate_snap .eq. 0) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &               'cal_position_pvr_screen', i_pvr
          call cal_position_pvr_screen(view_params(i_pvr)%modelview_mat,&
     &        view_params(i_pvr)%projection_mat, projected%nnod_pvr,    &
     &        projected%istack_nod_pvr, projected%x_nod_sim,            &
     &        projected%x_nod_model, projected%x_nod_screen)
!
          if(iflag_debug .gt. 0) write(*,*)                             &
     &               'position_pvr_domain_on_screen', i_pvr
          call position_pvr_domain_on_screen(view_params(i_pvr)%modelview_mat,    &
     &        view_params(i_pvr)%projection_mat, pvr_bound(i_pvr)%num_pvr_surf,   &
     &        pvr_bound(i_pvr)%xx_nod, pvr_bound(i_pvr)%xx_model,       &
     &        pvr_bound(i_pvr)%xx_screen)
!
          call set_pvr_domain_surface_data(view_params(i_pvr)%n_pvr_pixel,        &
     &        numele, numsurf,nnod_4_surf, ie_surf, isf_4_ele,          &
     &        projected%nnod_pvr, projected%x_nod_model,                &
     &        projected%x_nod_screen, pvr_bound(i_pvr))
        end if
      end do
!
!      call check_surf_rng_pvr_domain(my_rank)
!      call check_surf_norm_pvr_domain(my_rank)
!
      end subroutine pvr_init
!
!  ---------------------------------------------------------------------
!
      subroutine pvr_main(istep_pvr, numnod, numele, numsurf,           &
     &         nnod_4_ele, nnod_4_surf, inod_smp_stack, iele_smp_stack, &
     &         xx, radius, a_radius, s_cylinder, a_s_cylinder, ie,      &
     &         a_vol_ele, e_multi, ie_surf, isf_4_ele, iele_4_surf,     &
     &         ntot_int_3d, dnx, xjac, num_nod_phys, num_tot_nod_phys,  &
     &         istack_nod_component, d_nod)
!
      use m_pvr_image_array
      use cal_pvr_modelview_mat
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_surf
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: a_radius(numnod)
      real(kind = kreal), intent(in) :: s_cylinder(numnod)
      real(kind = kreal), intent(in) :: a_s_cylinder(numnod)
!
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      real(kind = kreal), intent(in) :: a_vol_ele(numele)
      real(kind = kreal), intent(in) :: e_multi(numele)
!
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer (kind = kint), intent(in) :: ntot_int_3d
      real(kind=kreal),   intent(in)                                    &
     &                  :: dnx(numele,nnod_4_ele,ntot_int_3d,3)
      real(kind=kreal),   intent(in) :: xjac(numele,ntot_int_3d)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_component(0:num_nod_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,num_tot_nod_phys)
!
!
      integer(kind = kint) :: i_pvr
      integer(kind = kint) :: i_rot, ist_rot, ied_rot
!
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_field_4_pvr'
      call cal_field_4_pvr(numnod, numele, nnod_4_ele,                  &
     &          inod_smp_stack, iele_smp_stack, xx, radius,             &
     &          a_radius, s_cylinder, a_s_cylinder, ie, a_vol_ele,      &
     &          ntot_int_3d, dnx, xjac, num_nod_phys, num_tot_nod_phys, &
     &          istack_nod_component, d_nod, projected)
!
      do i_pvr = 1, num_pvr
        if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
        call set_default_pvr_data_params                                &
     &     (d_minmax_pvr(1,i_pvr), color_params(i_pvr))
!
        if(     view_params(i_pvr)%iprm_pvr_rot(1).eq.1                 &
     &    .or.  view_params(i_pvr)%iprm_pvr_rot(1).eq.2                 &
     &    .or.  view_params(i_pvr)%iprm_pvr_rot(1).eq.3) then
          ist_rot = 1
          ied_rot = view_params(i_pvr)%iprm_pvr_rot(2)
        else
          ist_rot = 0
          ied_rot = 0
        end if
!
        call alloc_pvr_image_array_type(view_params(i_pvr)%n_pvr_pixel, pvr_img)
!
        do i_rot = ist_rot, ied_rot
          if(view_params(i_pvr)%iflag_rotate_snap .gt. 0) then
            call cal_pvr_modelview_matrix                               &
     &         (i_pvr, i_rot, view_params(i_pvr), color_params(i_pvr))
!
            call cal_position_pvr_screen                                &
     &         (view_params(i_pvr)%modelview_mat, view_params(i_pvr)%projection_mat,       &
     &          projected%nnod_pvr, projected%istack_nod_pvr,           &
     &          projected%x_nod_sim, projected%x_nod_model,             &
     &          projected%x_nod_screen)
            call position_pvr_domain_on_screen(view_params(i_pvr)%modelview_mat,  &
     &          view_params(i_pvr)%projection_mat, pvr_bound(i_pvr)%num_pvr_surf, &
     &          pvr_bound(i_pvr)%xx_nod, pvr_bound(i_pvr)%xx_model,     &
     &          pvr_bound(i_pvr)%xx_screen)
!
            call set_pvr_domain_surface_data(view_params(i_pvr)%n_pvr_pixel,      &
     &          numele, numsurf, nnod_4_surf, ie_surf, isf_4_ele,       &
     &          projected%nnod_pvr, projected%x_nod_model,              &
     &          projected%x_nod_screen, pvr_bound(i_pvr))
          end if
!
          if(iflag_debug .gt. 0) write(*,*) 's_set_pvr_ray_start_point'
          call s_set_pvr_ray_start_point(numnod, numele, numsurf,       &
     &        nnod_4_surf, xx, ie_surf, isf_4_ele,                      &
     &        view_params(i_pvr)%viewpoint_vec, pvr_bound(i_pvr),       &
     &        projected, pixel_xy(i_pvr), pvr_start)
!          call check_pvr_ray_startpoints(my_rank, pvr_start)
!
          if(iflag_debug .gt. 0) write(*,*) 's_ray_trace_4_each_image'
          call ray_trace_local(i_pvr, numnod, numele, numsurf,          &
     &       nnod_4_surf, ie_surf, isf_4_ele, iele_4_surf, e_multi, xx, &
     &       view_params(i_pvr)%viewpoint_vec, projected, pvr_start,    &
     &       pvr_img)
!
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'blend_pvr_over_domains', i_pvr
          call blend_pvr_over_domains(i_pvr, pvr_img)
!
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                'write_pvr_image_file', i_pvr
          call write_pvr_image_file(i_pvr, i_rot, istep_pvr, pvr_img)
!
          call dealloc_pvr_image_array_type(pvr_img)
!
          call calypso_MPI_barrier
        end do
!
      end do
!
      end subroutine pvr_main
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering
