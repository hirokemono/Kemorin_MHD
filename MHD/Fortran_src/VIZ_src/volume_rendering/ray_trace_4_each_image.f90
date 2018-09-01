!ray_trace_4_each_image.f90
!
!      module ray_trace_4_each_image
!
!      Written by H. Matsui on Aug., 2011
!
!!
!!      subroutine s_ray_trace_4_each_image                             &
!!     &         (node, ele, surf, pvr_screen, field_pvr, color_param,  &
!!     &          viewpoint_vec, ray_vec, num_pvr_ray, id_pixel_check,  &
!!     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,    &
!!     &          xx_pvr_start, xx_pvr_ray_start, rgba_ray)
!!      subroutine blend_overlapped_area(num_pvr_ray,                   &
!!     &         id_pixel_start, xx_pvr_ray_start, rgba_ray,            &
!!     &         num_pixel_xy, iflag_mapped, rgba_lc, depth_lc)
!
      module ray_trace_4_each_image
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
      use set_rgba_4_each_pixel
!
      use t_control_params_4_pvr
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
      subroutine s_ray_trace_4_each_image                               &
     &         (node, ele, surf, pvr_screen, field_pvr, color_param,    &
     &          viewpoint_vec, ray_vec, num_pvr_ray, id_pixel_check,    &
     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,      &
     &          xx_pvr_start, xx_pvr_ray_start, rgba_ray)
!
      use t_geometry_data
      use t_surface_data
      use t_geometries_in_pvr_screen
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_projected_position), intent(in) :: pvr_screen
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: ray_vec(3)
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in)                                  &
     &                    :: id_pixel_check(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                    :: icount_pvr_trace(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &                    :: isf_pvr_ray_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xi_pvr_start(2,num_pvr_ray)
      real(kind = kreal), intent(inout) :: xx_pvr_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                    ::  xx_pvr_ray_start(3,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                    ::  rgba_ray(4,num_pvr_ray)
!
      integer(kind = kint) :: inum, iflag_comm
      real(kind = kreal) :: rgba_tmp(4)
!
!
!$omp parallel do private(inum, iflag_comm,rgba_tmp)
      do inum = 1, num_pvr_ray
!        if(id_pixel_check(inum)*field_pvr%num_sections .gt. 0) then
!          write(*,*) 'check section trace for ', my_rank, inum
!        end if
!
        rgba_tmp(1:4) = zero
        call ray_trace_each_pixel                                       &
     &      (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,   &
     &       surf%ie_surf, surf%isf_4_ele, surf%iele_4_surf,            &
     &       ele%interior_ele, node%xx, surf%vnorm_surf,                &
     &       pvr_screen%arccos_sf, pvr_screen%x_nod_model,              &
     &       viewpoint_vec, field_pvr, color_param, ray_vec,            &
     &       id_pixel_check(inum), isf_pvr_ray_start(1,inum),           &
     &       xx_pvr_ray_start(1,inum), xx_pvr_start(1,inum),            &
     &       xi_pvr_start(1,inum), rgba_tmp(1), icount_pvr_trace(inum), &
     &       iflag_comm)
        rgba_ray(1:4,inum) = rgba_tmp(1:4)
      end do
!$omp end parallel do
!
      end subroutine s_ray_trace_4_each_image
!
!  ---------------------------------------------------------------------
!
      subroutine blend_overlapped_area(num_pvr_ray,                     &
     &         id_pixel_start, xx_pvr_ray_start, rgba_ray,              &
     &         num_pixel_xy, iflag_mapped, rgba_lc, depth_lc)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      real(kind = kreal), intent(in)                                    &
     &                    ::  xx_pvr_ray_start(3,num_pvr_ray)
      real(kind = kreal), intent(in)                                    &
     &                    ::  rgba_ray(4,num_pvr_ray)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(inout) :: iflag_mapped(num_pixel_xy)
      real(kind = kreal), intent(inout) :: rgba_lc(4,num_pixel_xy)
      real(kind = kreal), intent(inout) :: depth_lc(num_pixel_xy)
!
      integer(kind = kint) :: inum, id_pixel
!
!
      iflag_mapped = 0
      rgba_lc =      0.0d0
      depth_lc =     0.0d0
      do inum = 1, num_pvr_ray
        id_pixel = id_pixel_start(inum)
        call composite_alpha_blending                                   &
     &     (rgba_ray(1,inum), rgba_lc(1,id_pixel) )
!
        if(iflag_mapped(id_pixel) .eq. 0) then
          depth_lc(id_pixel)  = xx_pvr_ray_start(3,inum)
          iflag_mapped(id_pixel) = 1
        else
          depth_lc(id_pixel)                                            &
     &      = min(depth_lc(id_pixel),xx_pvr_ray_start(3,inum))
        end if
      end do
!
      end subroutine blend_overlapped_area
!
!  ---------------------------------------------------------------------
!
      subroutine ray_trace_each_pixel                                   &
     &       (numnod, numele, numsurf, nnod_4_surf, ie_surf,            &
     &        isf_4_ele, iele_4_surf, interior_ele, xx, vnorm_surf,     &
     &        arccos_sf, x_nod_model, viewpoint_vec, field_pvr,         &
     &        color_param, ray_vec, iflag_check, isurf_org,             &
     &        screen_st, xx_st, xi, rgba_ray, icount_line, iflag_comm)
!
      use t_geometries_in_pvr_screen
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use set_coefs_of_sections
!
      integer(kind = kint), intent(in) :: iflag_check
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      integer(kind = kint), intent(in) :: interior_ele(numele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: vnorm_surf(numsurf,3)
!
      real(kind = kreal), intent(in) :: x_nod_model(numnod,4)
      real(kind = kreal), intent(in) :: arccos_sf(numsurf)
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: ray_vec(3)
!
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: icount_line, iflag_comm
      real(kind = kreal), intent(inout) :: screen_st(3)
      real(kind = kreal), intent(inout) :: xx_st(3), xi(2)
      real(kind = kreal), intent(inout) :: rgba_ray(4)
!
      integer(kind = kint), parameter :: iflag_back = 0
      integer(kind = kint) :: iflag_notrace
      integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org
      integer(kind = kint) :: i_iso, i_psf, iflag, iflag_hit
      real(kind = kreal) :: screen_tgt(3), c_tgt(1), c_org(1)
      real(kind = kreal) :: grad_tgt(3), xx_tgt(3), rflag, rflag2
!
!
      if(isurf_org(1) .eq. 0) return
!
      iflag_notrace = 1
      iele =    isurf_org(1)
      isf_org = isurf_org(2)
      isurf_end = abs(isf_4_ele(iele,isf_org))
      call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,       &
     &    ie_surf, isurf_end, xi, xx, xx_st)
      call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,       &
     &    ie_surf, isurf_end, xi, field_pvr%d_pvr, c_org(1) )
!
      if(iflag_check .gt. 0) then
        iflag_hit = 0
      end if
      do
        icount_line = icount_line + 1
        iele =    isurf_org(1)
        isf_org = isurf_org(2)
!
        if(field_pvr%iflag_used_ele(iele).eq.0) then
          iflag_comm = 2
          exit
        end if
!
!   extend to surface of element
!
        call find_line_end_in_1ele                                      &
     &     (iflag_back, numnod, numele, numsurf, nnod_4_surf,           &
     &      isf_4_ele, ie_surf, x_nod_model, iele, isf_org,             &
     &      ray_vec, screen_st, isf_tgt, screen_tgt, xi)
!        if(iflag_check .gt. 0) write(*,*) 'screen_tgt',                &
!     &                        my_rank, xx_st(1:3), interior_ele(iele)
!
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
          exit
        end if
!
!   set backside element and surface 
!
        iflag_notrace = 0
        isurf_end = abs(isf_4_ele(iele,isf_tgt))
!
        if(isf_4_ele(iele,isf_tgt) .lt. 0) then
          isurf_org(1) = iele_4_surf(isurf_end,1,1)
          isurf_org(2) = iele_4_surf(isurf_end,1,2)
        else
          isurf_org(1) = iele_4_surf(isurf_end,2,1)
          isurf_org(2) = iele_4_surf(isurf_end,2,2)
        end if
!
        call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf_end, xi, xx, xx_tgt)
        call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf_end, xi, field_pvr%d_pvr, c_tgt(1))
!
        if(interior_ele(iele) .gt. 0) then
          if(arccos_sf(isurf_end) .gt. SMALL_RAY_TRACE) then
            grad_tgt(1:3) = vnorm_surf(isurf_end,1:3)
            call plane_rendering_with_light                             &
     &         (viewpoint_vec, xx_tgt, grad_tgt,                        &
     &          arccos_sf(isurf_end),  color_param, rgba_ray)
          end if
!
          do i_psf = 1, field_pvr%num_sections
            rflag =  side_of_plane(field_pvr%coefs(1:10,i_psf), xx_st)
            rflag2 = side_of_plane(field_pvr%coefs(1:10,i_psf), xx_tgt)
            if     (rflag .ge. -TINY .and. rflag2 .le. TINY) then
              iflag = 1
              iflag_hit = 1
            else if(rflag .le. TINY .and. rflag2 .ge. -TINY) then
              iflag = 1
              iflag_hit = 1
            else
              iflag = 0
            end if

            if(iflag .ne. 0) then
              call cal_normal_of_plane                                  &
     &           (field_pvr%coefs(1:10,i_psf), xx_tgt, grad_tgt)
              call color_plane_with_light                               &
     &           (viewpoint_vec, xx_tgt, c_tgt(1), grad_tgt,            &
     &            field_pvr%sect_opacity(i_psf), color_param, rgba_ray)
            end if
          end do
!
          do i_iso = 1, field_pvr%num_isosurf
            rflag =  (c_org(1) - field_pvr%iso_value(i_iso))            &
     &             * (c_tgt(1) - field_pvr%iso_value(i_iso))
            if((c_tgt(1) - field_pvr%iso_value(i_iso)) .eq. zero        &
     &        .or. rflag .lt. zero) then
              grad_tgt(1:3) = field_pvr%grad_ele(iele,1:3)              &
     &                       * field_pvr%itype_isosurf(i_iso)
              call color_plane_with_light                               &
     &           (viewpoint_vec, xx_tgt, field_pvr%iso_value(i_iso),    &
     &            grad_tgt, field_pvr%iso_opacity(i_iso),               &
     &            color_param, rgba_ray)
            end if
          end do
!
          grad_tgt(1:3) = field_pvr%grad_ele(iele,1:3)
          c_tgt(1) = half*(c_tgt(1) + c_org(1))
          call s_set_rgba_4_each_pixel(viewpoint_vec, xx_st, xx_tgt,    &
     &        c_tgt(1), grad_tgt, c_tgt(1), color_param, rgba_ray)
        end if
!
        if(isurf_org(1).eq.0) then
          iflag_comm = 0
          exit
        end if
!
        screen_st(1:3) = screen_tgt(1:3)
        xx_st(1:3) = xx_tgt(1:3)
        c_org(1) =   c_tgt(1)
      end do
!
!      if(iflag_check*field_pvr%num_sections .gt. 0) then
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
