!>@file   ray_trace_LIC_image.f90
!!@brief  module ray_trace_LIC_image
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine ray_trace_each_lic_image(node, ele, surf,            &
!!     &          lic_p, pvr_screen, field_lic, draw_param, color_param,&
!!     &          viewpoint_vec, ray_vec4, num_pvr_ray, id_pixel_check, &
!!     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,    &
!!     &          xx4_pvr_start, xx4_pvr_ray_start, rgba_ray)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_projected_position), intent(in) :: pvr_screen
!!@endverbatim
!
      module ray_trace_LIC_image
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
      use lic_rgba_4_each_pixel
!
      use t_geometry_data
      use t_surface_data
      use t_control_params_4_pvr
      use t_control_param_LIC
      use t_geometries_in_pvr_screen
      use t_lic_field_data
      use m_machine_parameter
      use cal_lic_on_surf_viz
!
      implicit  none
!
      private :: lic_ray_trace_each_pixel
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine ray_trace_each_lic_image(node, ele, surf,              &
     &          lic_p, pvr_screen, field_lic, draw_param, color_param,  &
     &          viewpoint_vec, ray_vec4, num_pvr_ray, id_pixel_check,   &
     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,      &
     &          xx4_pvr_start, xx4_pvr_ray_start, rgba_ray)
!
      use t_noise_node_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_projected_position), intent(in) :: pvr_screen
!
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
     &                    ::  xx4_pvr_ray_start(4,num_pvr_ray)
      real(kind = kreal), intent(inout)                                 &
     &                    ::  rgba_ray(4,num_pvr_ray)
!
      integer(kind = kint) :: inum, iflag_comm
      real(kind = kreal) :: rgba_tmp(4)
!
!      type(noise_mask), allocatable :: n_mask
      integer(kind = kint) :: sample_cnt
      real(kind = kreal) :: range_max, range_min

      iflag_debug = 0
!
      sample_cnt = 0
!
!      range_min = 3.0
!      range_max = 14.0
!      allocate(n_mask)
!      call init_noise_mask(n_mask, range_min, range_max,              &
!     &    field_lic%s_lic, node%numnod)
!
!$omp parallel do private(inum, iflag_comm,rgba_tmp)
      do inum = 1, num_pvr_ray
!        if(id_pixel_check(inum)*draw_param%num_sections .gt. 0) then
!          write(*,*) 'check section trace for ', my_rank, inum
!        end if
!
        rgba_tmp(1:4) = zero
        call lic_ray_trace_each_pixel(node%numnod, ele%numele,          &
     &      surf, ele%interior_ele, node%xx,               &
     &      pvr_screen%arccos_sf, pvr_screen%x_nod_model,               &
     &      viewpoint_vec, lic_p, field_lic, draw_param, color_param,   &
     &      ray_vec4, id_pixel_check(inum), isf_pvr_ray_start(1,inum),  &
     &      xx4_pvr_ray_start(1,inum), xx4_pvr_start(1,inum),           &
     &      xi_pvr_start(1,inum), rgba_tmp(1), icount_pvr_trace(inum),  &
     &      node%xyz_min_gl, node%xyz_max_gl, iflag_comm)
        rgba_ray(1:4,inum) = rgba_tmp(1:4)
        sample_cnt = sample_cnt + icount_pvr_trace(inum)
      end do
!$omp end parallel do
      write(*,*) "pvr sampling cnt:", my_rank, sample_cnt
!
      end subroutine ray_trace_each_lic_image
!
!  ---------------------------------------------------------------------
!
      subroutine lic_ray_trace_each_pixel                               &
     &       (numnod, numele, surf,            &
     &        interior_ele, xx,     &
     &        arccos_sf, x_nod_model, viewpoint_vec,     &
     &        lic_p, field_lic, draw_param, color_param, ray_vec4,      &
     &        iflag_check, isurf_org, screen4_st, xx4_st, xi, rgba_ray, &
     &        icount_line, xyz_min_gl, xyz_max_gl, iflag_comm)
!
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use set_coefs_of_sections
      use set_rgba_4_each_pixel
      use t_noise_node_data
!
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in) :: iflag_check
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: interior_ele(numele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      real(kind = kreal), intent(in) :: x_nod_model(numnod,4)
      real(kind = kreal), intent(in) :: arccos_sf(surf%numsurf)
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: ray_vec4(4)
!
      real(kind = kreal), intent(in) :: xyz_min_gl(3)
      real(kind = kreal), intent(in) :: xyz_max_gl(3)
!
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
      type(rendering_parameter), intent(in) :: draw_param
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: icount_line, iflag_comm
      real(kind = kreal), intent(inout) :: screen4_st(4)
      real(kind = kreal), intent(inout) :: xx4_st(4), xi(2)
      real(kind = kreal), intent(inout) :: rgba_ray(4)
!
!      type(noise_mask), intent(inout) :: n_mask
!
      integer(kind = kint) :: iflag_notrace
      integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org, i_psf
      integer(kind = kint) :: iflag_hit, iflag
      real(kind = kreal) :: screen4_tgt(4)
      real(kind = kreal), allocatable :: r_org(:), r_tgt(:), r_mid(:)
      real(kind = kreal) :: xx4_tgt(4), grad_len, rflag, rflag2

      real(kind = kreal) :: rlic_grad(0:3), grad_tgt(3)
      real(kind = kreal) :: xx4_lic(4), xx4_lic_last(4)
      real(kind = kreal) :: scl_org(1), scl_tgt(1), scl_mid(1)
      real(kind = kreal) :: vec4_org(4), vec4_tgt(4), vec4_mid(4)
      integer(kind = kint) :: isurf_orgs(2,3), i, iflag_lic

      real(kind = kreal) :: ray_total_len = zero, ave_ray_len, step_size
      integer(kind = kint) :: icount_line_cur_ray = 0, step_cnt
      real(kind = kreal) :: ray_len_left, ray_left, ray_len, ratio
!
      if(isurf_org(1) .eq. 0) return
!
      step_size = lic_p%step_size
      ray_left = 0.0
!
      iflag_notrace = 1

      iflag_lic = 1
      iele =    isurf_org(1)
      isf_org = isurf_org(2)
      isurf_end = abs(surf%isf_4_ele(iele,isf_org))
!
      if(iflag_check .gt. 0) then
        iflag_hit = 0
      end if
!   get original value of sampling point
      call cal_field_on_surf_vect4                                      &
     &   (numnod, surf%numsurf, surf%nnod_4_surf,   &
     &    surf%ie_surf, isurf_end, xi, xx, xx4_st)
      call cal_field_on_surf_vect4                                      &
     &   (numnod, surf%numsurf, surf%nnod_4_surf,   &
     &    surf%ie_surf, isurf_end, xi, field_lic%v_lic, vec4_org)
      call cal_field_on_surf_scalar                                     &
     &   (numnod, surf%numsurf, surf%nnod_4_surf,  &
     &    surf%ie_surf, isurf_end, xi, field_lic%d_lic, scl_org)

      allocate(r_org(lic_p%num_masking))
      allocate(r_tgt(lic_p%num_masking))
      allocate(r_mid(lic_p%num_masking))
      do i = 1, lic_p%num_masking
        if(lic_p%masking(i)%mask_type .eq. iflag_fieldmask) then
          call cal_field_on_surf_scalar                                 &
     &       (numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,     &
     &        isurf_end, xi, field_lic%s_lic(1,i), r_org(i))
        end if
      end do
!
!
!        Set color if starting surface is colourd
      if(interior_ele(iele) .gt. 0) then
!   rendering boundery
        if(arccos_sf(isurf_end) .gt. SMALL_RAY_TRACE) then
          rlic_grad(1:3) = surf%vnorm_surf(isurf_end,1:3)
          call plane_rendering_with_light                               &
     &       (viewpoint_vec, xx4_st, rlic_grad(1),                      &
     &        arccos_sf(isurf_end),  color_param, rgba_ray)
        end if
      end if
!
!   start ray casting
      do
        icount_line = icount_line + 1
        icount_line_cur_ray = icount_line_cur_ray + 1
        iele =    isurf_org(1)
        isf_org = isurf_org(2)
!
        if(draw_param%iflag_used_ele(iele) .eq. 0) then
          iflag_comm = 2
          exit
        end if
!
!   extend to surface of element
!   find ray exit surface loacal id on current element isf_tgt
!
        call find_line_end_in_1ele                                      &
     &     (iflag_backward_line, numnod, numele,                        &
     &      surf%numsurf, surf%nnod_4_surf, surf%isf_4_ele,             &
     &      surf%ie_surf, x_nod_model, iele, isf_org,                   &
     &      ray_vec4, screen4_st, isf_tgt, screen4_tgt, xi)
!        if(iflag_check .gt. 0) write(*,*) 'screen4_tgt',               &
!     &                  my_rank, screen4_tgt(1:3), interior_ele(iele)
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
! element on one side share the surface
          isurf_org(1) = surf%iele_4_surf(isurf_end,1,1)
! the surface id of one side element(1-6)
          isurf_org(2) = surf%iele_4_surf(isurf_end,1,2)
        else
          isurf_org(1) = surf%iele_4_surf(isurf_end,2,1)
          isurf_org(2) = surf%iele_4_surf(isurf_end,2,2)
        end if
        ! new element surface info
        do i = 1, 2
          isurf_orgs(i,1) = surf%iele_4_surf(isurf_end,i,1)
          isurf_orgs(i,2) = surf%iele_4_surf(isurf_end,i,2)
          isurf_orgs(i,3) = isurf_org(3)
        end do
!   find 3D coordinate of exit point on exit surface
        call cal_field_on_surf_vect4                                    &
     &     (numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,       &
     &      isurf_end, xi, xx, xx4_tgt)
        call cal_field_on_surf_vect4                                    &
     &     (numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,       &
     &      isurf_end, xi, field_lic%v_lic, vec4_tgt)
        call cal_field_on_surf_scalar                                   &
     &     (numnod, surf%numsurf, surf%nnod_4_surf,                     &
     &      surf%ie_surf, isurf_end, xi, field_lic%d_lic, scl_tgt)

        do i = 1, lic_p%num_masking
          if(lic_p%masking(i)%mask_type .eq. iflag_fieldmask) then
            call cal_field_on_surf_scalar                               &
     &         (numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,   &
     &          isurf_end, xi, field_lic%s_lic(1,i), r_tgt(i))
          end if
        end do

        rlic_grad(0) = 0.0d0
!
        if(interior_ele(iele) .gt. 0) then
!   rendering boundery
          if(arccos_sf(isurf_end) .gt. SMALL_RAY_TRACE) then
            rlic_grad(1:3) = surf%vnorm_surf(isurf_end,1:3)
            call plane_rendering_with_light                             &
     &         (viewpoint_vec, xx4_tgt, rlic_grad(1),                   &
     &          arccos_sf(isurf_end),  color_param, rgba_ray)
          end if
!
!   3d lic calculation at current xx position
!   if sampling by fixed step size
          ray_len = sqrt( (xx4_tgt(1) - xx4_st(1))**2                   &
     &                  + (xx4_tgt(2) - xx4_st(2))**2                   &
     &                  + (xx4_tgt(3) - xx4_st(3))**2)
          if(lic_p%iflag_vr_sample_mode .eq. iflag_fixed_size) then
            ray_len_left = ray_left + ray_len

            xx4_lic_last(1:4) = xx4_st(1:4)
            step_cnt = 0
            do while(ray_len_left .gt. zero)
              ray_len_left = ray_len_left - step_size
              ratio = (step_size*step_cnt - ray_left) / ray_len
              xx4_lic(1:4) = xx4_st(1:4)                                &
     &                    + ratio * (xx4_tgt(1:4) - xx4_st(1:4))
!
              do i = 1, lic_p%num_masking
                if(lic_p%masking(i)%mask_type                           &
     &                                .eq. iflag_fieldmask) then
                  r_mid(i)                                              &
     &               = r_org(i) * (1.0d0 - ratio) + r_tgt(i)*ratio
                end if
!write(*,*) "org", r_org, "tgt", r_tgt, "ratio", ratio
              end do
! masking on sampling point
!              if(mask_flag(lic_p, r_mid)) then

              vec4_mid(1:4) = vec4_org(1:4) * (1.0d0 - ratio)           &
     &                       + vec4_tgt(1:4) * ratio
              call cal_lic_on_surf_vector                               &
     &           (numnod, surf%numsurf, numele, surf%nnod_4_surf,       &
     &            surf%isf_4_ele, surf%iele_4_surf, surf%interior_surf, &
     &            xx, isurf_orgs, surf%ie_surf, xi, lic_p,              &
     &            r_mid, vec4_mid, field_lic%s_lic,                     &
     &            field_lic%v_lic, xx4_lic, isurf_end,                  &
     &            xyz_min_gl, xyz_max_gl, iflag_lic, rlic_grad)
!
  !   normalize gradient
              grad_len = sqrt(rlic_grad(1)*rlic_grad(1)                 &
     &                      + rlic_grad(2)*rlic_grad(2)                 &
     &                      + rlic_grad(3)*rlic_grad(3))
              if(grad_len .ne. 0.0) then
                rlic_grad(1:3) = rlic_grad(1:3) / grad_len
              end if
! render section (clipping surface)

!
              if(lic_p%iflag_color_mode .eq. iflag_from_control) then
                scl_mid(1)                                              &
     &            = scl_org(1) * (1.0d0 - ratio) + scl_tgt(1) * ratio
                call s_lic_rgba_4_each_pixel                            &
     &             (viewpoint_vec, xx4_lic_last, xx4_lic,               &
     &              scl_mid(1), rlic_grad(1), rlic_grad(0),             &
     &              color_param, step_size, rgba_ray)
              else
                call s_lic_rgba_4_each_pixel                            &
     &             (viewpoint_vec, xx4_lic_last, xx4_lic,               &
     &              rlic_grad(0), rlic_grad(1), rlic_grad(0),           &
     &              color_param, step_size, rgba_ray)
              end if

              xx4_lic_last(1:4) = xx4_lic(1:4)
              step_cnt = step_cnt + 1
            end do
            ray_left = ray_len_left
!
! sampling by cell
          else
            ray_total_len = ray_total_len + ray_len
!   find mid point between xx_st and xx_tgt, this mid point will be actual sample point
            xx4_lic(1:4) = half*(xx4_st(1:4) + xx4_tgt(1:4))
!
!   reference data at origin of lic iteration
            do i = 1, lic_p%num_masking
              if(lic_p%masking(i)%mask_type .eq. iflag_fieldmask) then
                r_mid(i) = half*(r_org(i)+r_tgt(i))
              end if
            end do
!   the vector interpolate from entry and exit point
            vec4_mid(1:4) = half*(vec4_org(1:4) + vec4_tgt(1:4))
!   calculate lic value at current location, lic value will be used as intensity
!   as volume rendering
            call cal_lic_on_surf_vector                                 &
     &         (numnod, surf%numsurf, numele, surf%nnod_4_surf,         &
     &          surf%isf_4_ele, surf%iele_4_surf, surf%interior_surf,   &
     &          xx, isurf_orgs, surf%ie_surf, xi, lic_p,                &
     &          r_mid, vec4_mid, field_lic%s_lic,                       &
     &          field_lic%v_lic, xx4_lic, isurf_end,                    &
     &          xyz_min_gl, xyz_max_gl, iflag_lic, rlic_grad)
!
            ave_ray_len = ray_total_len / icount_line_cur_ray
!
!   normalize gradient
            grad_len = sqrt(rlic_grad(1)*rlic_grad(1)                   &
     &                    + rlic_grad(2)*rlic_grad(2)                   &
     &                    + rlic_grad(3)*rlic_grad(3))
            if(grad_len .ne. 0.0) then
              rlic_grad(1:3) = rlic_grad(1:3) / grad_len
            endif

            do i_psf = 1, draw_param%num_sections
              rflag =  side_of_plane(draw_param%coefs(1:10,i_psf),      &
     &                               xx4_st(1))
              rflag2 = side_of_plane(draw_param%coefs(1:10,i_psf),      &
     &                               xx4_tgt(1))
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
                call cal_normal_of_plane                                &
     &             (draw_param%coefs(1:10,i_psf), xx4_tgt(1), grad_tgt)
                call color_plane_with_light                             &
     &             (viewpoint_vec, xx4_tgt, rlic_grad(0), grad_tgt,     &
     &              draw_param%sect_opacity(i_psf), color_param,        &
     &              rgba_ray)
              end if
            end do
!
            if(lic_p%iflag_color_mode .eq. iflag_from_control) then
              scl_mid(1) = half*(scl_org(1) + scl_tgt(1))
              call s_lic_rgba_4_each_pixel                              &
     &           (viewpoint_vec, xx4_st, xx4_tgt,                       &
     &            scl_mid(1), rlic_grad(1), rlic_grad(0),               &
     &            color_param, ave_ray_len, rgba_ray)
            else
              call s_lic_rgba_4_each_pixel                              &
     &           (viewpoint_vec, xx4_st, xx4_tgt,                       &
     &            rlic_grad(0), rlic_grad(1), rlic_grad(0),             &
     &            color_param, ave_ray_len, rgba_ray)
            end if
          end if
        end if
!       write(*,*) 'rgba_ray end', rgba_ray
!
        if(isurf_org(1).eq.0) then
          iflag_comm = 0
          exit
        end if
!
        screen4_st(1:4) = screen4_tgt(1:4)
        xx4_st(1:4) =     xx4_tgt(1:4)
        do i = 1, lic_p%num_masking
          if(lic_p%masking(i)%mask_type .eq. iflag_fieldmask) then
            r_org(i) = r_tgt(i)
          end if
        end do
        scl_org(1) =    scl_tgt(1)
        vec4_org(1:4) = vec4_tgt(1:4)
      end do
      end subroutine lic_ray_trace_each_pixel
!
!  ---------------------------------------------------------------------
!
      end module ray_trace_LIC_image
