!>@file   ray_trace_4_each_image.f90
!!@brief  module ray_trace_4_each_image
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2011
!
!>@brief structure of control data for multiple volume rendering
!!
!!@verbatim
!!      subroutine s_ray_trace_4_each_image(mesh, group, sf_grp_4_sf,   &
!!     &          pvr_screen, field_pvr, draw_param, color_param,       &
!!     &          viewpoint_vec, modelview_mat, ray_vec4, num_pvr_ray,  &
!!     &          id_pixel_check, icount_pvr_trace, isf_pvr_ray_start,  &
!!     &          xi_pvr_start, xx4_pvr_start, xx4_pvr_ray_start,       &
!!     &          rgba_ray)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(pvr_field_data), intent(in) :: field_pvr
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_projected_position), intent(in) :: pvr_screen
!!@endverbatim
      module ray_trace_4_each_image
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
      use set_rgba_4_each_pixel
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surf_grp_list_each_surf
      use t_control_params_4_pvr
      use t_pvr_field_data
      use t_geometries_in_pvr_screen
!
      implicit  none
!
      private :: ray_trace_each_pixel
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_ray_trace_4_each_image(mesh, group, sf_grp_4_sf,     &
     &          pvr_screen, field_pvr, draw_param, color_param,         &
     &          viewpoint_vec, modelview_mat, ray_vec4, num_pvr_ray,    &
     &          id_pixel_check, icount_pvr_trace, isf_pvr_ray_start,    &
     &          xi_pvr_start, xx4_pvr_start, xx4_pvr_ray_start,         &
     &          rgba_ray)
!
      use t_geometries_in_pvr_screen
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!
      type(pvr_field_data), intent(in) :: field_pvr
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_projected_position), intent(in) :: pvr_screen
!
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: ray_vec4(4)
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in)                                  &
     &                    :: id_pixel_check(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                    :: icount_pvr_trace(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                    :: isf_pvr_ray_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xi_pvr_start(2,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xx4_pvr_start(4,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                    :: xx4_pvr_ray_start(4,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                    :: rgba_ray(4,num_pvr_ray)
!
      integer(kind = kint) :: inum, iflag_comm
      real(kind = kreal) :: rgba_tmp(4)
!
!
!$omp parallel do private(inum, iflag_comm,rgba_tmp)
      do inum = 1, num_pvr_ray
!        if(id_pixel_check(inum)*draw_param%num_sections .gt. 0) then
!          write(*,*) 'check section trace for ', my_rank, inum
!        end if
!
        rgba_tmp(1:4) = zero
        call ray_trace_each_pixel(mesh%node, mesh%ele, mesh%surf,       &
     &       group%surf_grp, sf_grp_4_sf, pvr_screen%x_nod_model,       &
     &       viewpoint_vec, modelview_mat,                              &
     &       field_pvr, draw_param, color_param,                        &
     &       ray_vec4, id_pixel_check(inum), isf_pvr_ray_start(1,inum), &
     &       xx4_pvr_ray_start(1,inum), xx4_pvr_start(1,inum),          &
     &       xi_pvr_start(1,inum), rgba_tmp(1), icount_pvr_trace(inum), &
     &       iflag_comm)
        rgba_ray(1:4,inum) = rgba_tmp(1:4)
      end do
!$omp end parallel do
!
      end subroutine s_ray_trace_4_each_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine ray_trace_each_pixel                                   &
     &         (node, ele, surf, surf_grp, sf_grp_4_sf,                 &
     &          x_nod_model, viewpoint_vec, modelview_mat, field_pvr,   &
     &          draw_param, color_param, ray_vec4, iflag_check,         &
     &          isurf_org, screen4_st, xx4_st, xi, rgba_ray,            &
     &          icount_line, iflag_comm)
!
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use set_coefs_of_sections
      use pvr_surface_enhancement
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      integer(kind = kint), intent(in) :: iflag_check
!
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
      real(kind = kreal), intent(in) :: x_nod_model(node%numnod,4)
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: ray_vec4(4)
!
      type(pvr_field_data), intent(in) :: field_pvr
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: icount_line, iflag_comm
      real(kind = kreal), intent(inout) :: screen4_st(4)
      real(kind = kreal), intent(inout) :: xx4_st(4), xi(2)
      real(kind = kreal), intent(inout) :: rgba_ray(4)
!
      integer(kind = kint) :: iflag_notrace
      integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org
      integer(kind = kint) :: i_iso, i_psf, iflag, iflag_hit
      real(kind = kreal) :: screen4_tgt(4), c_tgt(1), c_org(1)
      real(kind = kreal) :: grad_tgt(3), xx4_tgt(4), rflag, rflag2
      real(kind = kreal) :: opacity_bc
!
!
      if(isurf_org(1) .eq. 0) return
!
      iflag_notrace = 1
      iele =    isurf_org(1)
      isf_org = isurf_org(2)
      isurf_end = abs(surf%isf_4_ele(iele,isf_org))
      call cal_field_on_surf_vect4                                      &
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf,                  &
     &    surf%ie_surf, isurf_end, xi, node%xx, xx4_st)
      call cal_field_on_surf_scalar                                     &
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf,                  &
     &    surf%ie_surf, isurf_end, xi, field_pvr%d_pvr, c_org(1) )
!
      if(iflag_check .gt. 0) then
        iflag_hit = 0
      end if
!
!        Set color if starting surface is colourd
      if(ele%interior_ele(iele) .gt. 0) then
        opacity_bc = opacity_by_surf_grp(isurf_end, surf, surf_grp,     &
     &          sf_grp_4_sf, modelview_mat,                             &
     &          draw_param%iflag_enhanse, draw_param%enhansed_opacity)
        if(opacity_bc .gt. SMALL_RAY_TRACE) then
          grad_tgt(1:3) = surf%vnorm_surf(isurf_end,1:3)
          call plane_rendering_with_light(viewpoint_vec,                &
     &        xx4_st, grad_tgt, opacity_bc,  color_param, rgba_ray)
        end if
      end if
!
      do
        icount_line = icount_line + 1
        iele =    isurf_org(1)
        isf_org = isurf_org(2)
!
        if(draw_param%iflag_used_ele(iele).eq.0) then
          iflag_comm = 2
          exit
        end if
!
!   extend to surface of element
!
        call find_line_end_in_1ele(iflag_backward_line, node%numnod, &
     &      ele%numele, ele%nnod_4_ele, ele%ie, surf%node_on_sf,   &
     &      surf%numsurf, &
     &      surf%nnod_4_surf, surf%isf_4_ele, surf%ie_surf,             &
     &      x_nod_model, iele, isf_org, ray_vec4, screen4_st,           &
     &      isf_tgt, screen4_tgt, xi)
!        if(iflag_check .gt. 0) write(*,*) 'screen_tgt',                &
!     &         my_rank, screen4_tgt(1:4), ele%interior_ele(iele)
!
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
          exit
        end if
!
!   set backside element and surface 
!
        iflag_notrace = 0
        isurf_end = abs(surf%isf_4_ele(iele,isf_tgt))
!
        if(surf%isf_4_ele(iele,isf_tgt) .lt. 0) then
          isurf_org(1) = surf%iele_4_surf(isurf_end,1,1)
          isurf_org(2) = surf%iele_4_surf(isurf_end,1,2)
        else
          isurf_org(1) = surf%iele_4_surf(isurf_end,2,1)
          isurf_org(2) = surf%iele_4_surf(isurf_end,2,2)
        end if
!
        call cal_field_on_surf_vect4                                    &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf,                &
     &      surf%ie_surf, isurf_end, xi, node%xx, xx4_tgt)
        call cal_field_on_surf_scalar                                   &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf,                &
     &      surf%ie_surf, isurf_end, xi, field_pvr%d_pvr, c_tgt(1))
!
        if(ele%interior_ele(iele) .gt. 0) then
!    Set color if exit surface is colourd
          opacity_bc = opacity_by_surf_grp(isurf_end, surf, surf_grp,   &
     &          sf_grp_4_sf, modelview_mat,                             &
     &          draw_param%iflag_enhanse, draw_param%enhansed_opacity)
          if(opacity_bc .gt. SMALL_RAY_TRACE) then
            grad_tgt(1:3) = surf%vnorm_surf(isurf_end,1:3)
            call plane_rendering_with_light (viewpoint_vec,             &
     &          xx4_tgt, grad_tgt, opacity_bc,  color_param, rgba_ray)
          end if
!
          do i_psf = 1, draw_param%num_sections
            rflag                                                       &
     &        = side_of_plane(draw_param%coefs(1:10,i_psf), xx4_st(1))
            rflag2                                                      &
     &        = side_of_plane(draw_param%coefs(1:10,i_psf), xx4_tgt(1))
            if     (rflag .ge. -TINY9 .and. rflag2 .le. TINY9) then
              iflag = 1
              iflag_hit = 1
            else if(rflag .le. TINY9 .and. rflag2 .ge. -TINY9) then
              iflag = 1
              iflag_hit = 1
            else
              iflag = 0
            end if

            if(iflag .ne. 0) then
              call cal_normal_of_plane                                  &
     &           (draw_param%coefs(1:10,i_psf), xx4_tgt(1), grad_tgt)
              call color_plane_with_light                               &
     &           (viewpoint_vec, xx4_tgt, c_tgt(1), grad_tgt,           &
     &            draw_param%sect_opacity(i_psf), color_param,          &
     &            rgba_ray)
            end if
          end do
!
          do i_iso = 1, draw_param%num_isosurf
            rflag =  (c_org(1) - draw_param%iso_value(i_iso))           &
     &             * (c_tgt(1) - draw_param%iso_value(i_iso))
            if((c_tgt(1) - draw_param%iso_value(i_iso)) .eq. zero       &
     &        .or. rflag .lt. zero) then
              grad_tgt(1:3) = field_pvr%grad_ele(iele,1:3)              &
     &                       * draw_param%itype_isosurf(i_iso)
              call color_plane_with_light(viewpoint_vec, xx4_tgt,       &
     &            draw_param%iso_value(i_iso), grad_tgt,                &
     &            draw_param%iso_opacity(i_iso), color_param, rgba_ray)
            end if
          end do
!
          grad_tgt(1:3) = field_pvr%grad_ele(iele,1:3)
          c_tgt(1) = half*(c_tgt(1) + c_org(1))
          call s_set_rgba_4_each_pixel                                  &
     &       (viewpoint_vec, xx4_st, xx4_tgt,                           &
     &        c_tgt(1), grad_tgt, color_param, rgba_ray)
        end if
!
        if(isurf_org(1).eq.0) then
          iflag_comm = 0
          exit
        end if
!
        screen4_st(1:4) = screen4_tgt(1:4)
        xx4_st(1:4) = xx4_tgt(1:4)
        c_org(1) =   c_tgt(1)
      end do
!
!      if(iflag_check*draw_param%num_sections .gt. 0) then
!        if(iflag_hit .eq. 0) then
!          write(*,*) 'surface does not hit: ', my_rank, rgba_ray(1:4)
!        else
!          write(*,*) 'surface  hit in: ', my_rank, rgba_ray(1:4)
!        end if
!      end if
!
      end subroutine ray_trace_each_pixel
!
!  ---------------------------------------------------------------------
!
      end module ray_trace_4_each_image
