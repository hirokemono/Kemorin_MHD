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
!!     &          lic_p, pvr_screen, field_pvr, color_param,            &
!!     &          viewpoint_vec, ray_vec, num_pvr_ray, id_pixel_check,  &
!!     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,    &
!!     &          xx_pvr_start, xx_pvr_ray_start, rgba_ray)
!!       type(node_data), intent(in) :: node
!!       type(element_data), intent(in) :: ele
!!       type(surface_data), intent(in) :: surf
!!       type(lic_parameters), intent(in) :: lic_p
!!       type(pvr_projected_field), intent(in) :: field_pvr
!!       type(pvr_colormap_parameter), intent(in) :: color_param
!!       type(pvr_projected_position), intent(in) :: pvr_screen
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
      use t_control_params_4_pvr
      use t_control_param_LIC
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
     &          lic_p, pvr_screen, field_pvr, color_param,              &
     &          viewpoint_vec, ray_vec, num_pvr_ray, id_pixel_check,    &
     &          icount_pvr_trace, isf_pvr_ray_start, xi_pvr_start,      &
     &          xx_pvr_start, xx_pvr_ray_start, rgba_ray)
!
      use t_geometry_data
      use t_surface_data
      use t_geometries_in_pvr_screen
      use t_noise_node_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(lic_parameters), intent(in) :: lic_p
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
      !type(noise_mask), allocatable :: n_mask
      integer(kind = kint) :: sample_cnt
      real(kind = kreal) :: range_max, range_min

      iflag_debug = 0
!
      sample_cnt = 0

      !range_min = 3.0
      !range_max = 14.0
      !allocate(n_mask)
      !call init_noise_mask(n_mask, range_min, range_max, field_pvr%s_lic, node%numnod)
!
!$omp parallel do private(inum, iflag_comm,rgba_tmp)
      do inum = 1, num_pvr_ray
!        if(id_pixel_check(inum)*field_pvr%num_sections .gt. 0) then
!          write(*,*) 'check section trace for ', my_rank, inum
!        end if
!
        rgba_tmp(1:4) = zero
        call lic_ray_trace_each_pixel(node%numnod, ele%numele,          &
     &      surf%numsurf, surf%nnod_4_surf, surf%ie_surf,               &
     &      surf%isf_4_ele, surf%iele_4_surf, ele%interior_ele,         &
     &      node%xx, surf%vnorm_surf, surf%interior_surf,               &
     &      pvr_screen%arccos_sf, pvr_screen%x_nod_model,               &
     &      viewpoint_vec, lic_p, field_pvr, color_param, ray_vec,      &
     &      id_pixel_check(inum), isf_pvr_ray_start(1,inum),            &
     &      xx_pvr_ray_start(1,inum), xx_pvr_start(1,inum),             &
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
     &       (numnod, numele, numsurf, nnod_4_surf, ie_surf,            &
     &        isf_4_ele, iele_4_surf, interior_ele, xx, vnorm_surf,     &
     &        interior_surf, arccos_sf, x_nod_model, viewpoint_vec,     &
     &        lic_p, field_pvr, color_param, ray_vec, iflag_check,      &
     &        isurf_org, screen_st, xx_st, xi, rgba_ray, icount_line,   &
     &        xyz_min_gl, xyz_max_gl, iflag_comm)
!
      use t_geometries_in_pvr_screen
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use set_coefs_of_sections
      use set_rgba_4_each_pixel
      use t_noise_node_data
!
      integer(kind = kint), intent(in) :: iflag_check
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      integer(kind = kint), intent(in) :: interior_ele(numele)
      integer(kind = kint), intent(in) :: interior_surf(numsurf)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: vnorm_surf(numsurf,3)
!
      real(kind = kreal), intent(in) :: x_nod_model(numnod,4)
      real(kind = kreal), intent(in) :: arccos_sf(numsurf)
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: ray_vec(3)
!
      real(kind = kreal), intent(in) :: xyz_min_gl(3)
      real(kind = kreal), intent(in) :: xyz_max_gl(3)
!
      type(lic_parameters), intent(in) :: lic_p
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: icount_line, iflag_comm
      real(kind = kreal), intent(inout) :: screen_st(3)
      real(kind = kreal), intent(inout) :: xx_st(3), xi(2)
      real(kind = kreal), intent(inout) :: rgba_ray(4)
!
!      type(noise_mask), intent(inout) :: n_mask
!
      integer(kind = kint) :: iflag_notrace
      integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org
      integer(kind = kint) :: iflag_hit
      real(kind = kreal) :: screen_tgt(3)
      real(kind = kreal), allocatable :: r_org(:), r_tgt(:), r_mid(:)
      real(kind = kreal) :: grad_tgt(3), xx_tgt(3), grad_len

      real(kind = kreal) :: lic_tgt(1)
      real(kind = kreal) :: xx_lic(3), xx_lic_last(3)
      real(kind = kreal) :: scl_org(1), scl_tgt(1), scl_mid(1)
      real(kind = kreal) :: vec_org(3), vec_tgt(3), vec_mid(3)
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
      isurf_end = abs(isf_4_ele(iele,isf_org))
!
      if(iflag_check .gt. 0) then
        iflag_hit = 0
      end if
!   get original value of sampling point
      call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,       &
      &   ie_surf, isurf_end, xi, xx, xx_st)
      call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,       &
      &   ie_surf, isurf_end, xi, field_pvr%v_lic, vec_org)
      call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,       &
      &   ie_surf, isurf_end, xi, field_pvr%d_pvr, scl_org)

      allocate(r_org(lic_p%num_masking))
      allocate(r_tgt(lic_p%num_masking))
      allocate(r_mid(lic_p%num_masking))
      do i = 1, lic_p%num_masking
        if(lic_p%masking(i)%mask_type .eq. iflag_fieldmask) then
          call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,   &
       &      ie_surf, isurf_end, xi, field_pvr%s_lic(1,i), r_org(i) )
        end if
      end do
!
!
!        Set color if starting surface is colourd
      if(interior_ele(iele) .gt. 0) then
        if(arccos_sf(isurf_end) .gt. SMALL_RAY_TRACE) then
          grad_tgt(1:3) = vnorm_surf(isurf_end,1:3)
          call plane_rendering_with_light                               &
     &       (viewpoint_vec, xx_st, grad_tgt,                           &
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
        if(field_pvr%iflag_used_ele(iele).eq.0) then
          iflag_comm = 2
          exit
        end if
!
!   extend to surface of element
!   find ray exit surface loacal id on current element isf_tgt
!
        call find_line_end_in_1ele                                      &
     &     (iflag_backward_line, numnod, numele, numsurf, nnod_4_surf,  &
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
          isurf_org(1) = iele_4_surf(isurf_end,1,1) ! element on one side share the surface
          isurf_org(2) = iele_4_surf(isurf_end,1,2) ! the surface id of one side element(1-6)
        else
          isurf_org(1) = iele_4_surf(isurf_end,2,1)
          isurf_org(2) = iele_4_surf(isurf_end,2,2)
        end if
        ! new element surface info
        do i = 1, 2
          isurf_orgs(i,1) = iele_4_surf(isurf_end,i,1)
          isurf_orgs(i,2) = iele_4_surf(isurf_end,i,2)
          isurf_orgs(i,3) = isurf_org(3)
        end do
!   find 3D coordinate of exit point on exit surface
        call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf_end, xi, xx, xx_tgt)
        call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf_end, xi, field_pvr%v_lic, vec_tgt)
        call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf_end, xi, field_pvr%d_pvr, scl_tgt)

        do i = 1, lic_p%num_masking
          if(lic_p%masking(i)%mask_type .eq. iflag_fieldmask) then
            call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,   &
       &        ie_surf, isurf_end, xi, field_pvr%s_lic(1,i), r_tgt(i) )
          end if
        end do

        lic_tgt(1) = 0.0
!
        if(interior_ele(iele) .gt. 0) then
!   rendering boundery
          if(arccos_sf(isurf_end) .gt. SMALL_RAY_TRACE) then
            grad_tgt(1:3) = vnorm_surf(isurf_end,1:3)
            call plane_rendering_with_light                             &
     &         (viewpoint_vec, xx_tgt, grad_tgt,                        &
     &          arccos_sf(isurf_end),  color_param, rgba_ray)
          end if
!
!   3d lic calculation at current xx position
!   if sampling by fixed step size
          ray_len = sqrt( (xx_tgt(1) - xx_st(1))**2                     &
       &                + (xx_tgt(2) - xx_st(2))**2                     &
       &                + (xx_tgt(3) - xx_st(3))**2)
          if(lic_p%iflag_vr_sample_mode .eq. 0) then
            ray_len_left = ray_left + ray_len

            xx_lic_last = xx_st
            step_cnt = 0
            do while(ray_len_left .gt. zero)
              ray_len_left = ray_len_left - step_size
              ratio = (step_size*step_cnt - ray_left) / ray_len
              xx_lic = xx_st + ratio * (xx_tgt - xx_st)
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

              vec_mid(1:3)                                              &
     &          = vec_org(1:3) * (1.0d0 - ratio) + vec_tgt(1:3) * ratio
              call cal_lic_on_surf_vector                               &
     &           (numnod, numsurf, numele, nnod_4_surf,                 &
     &            isf_4_ele, iele_4_surf, interior_surf, xx,            &
     &            isurf_orgs, ie_surf, xi, lic_p,                       &
     &            r_mid, vec_mid, field_pvr%s_lic,                      &
     &            field_pvr%v_lic, xx_lic, isurf_end,                   &
     &            xyz_min_gl, xyz_max_gl, iflag_lic,                    &
     &            lic_tgt(1), grad_tgt)
!
  !   normalize gradient
              grad_len = sqrt(grad_tgt(1)*grad_tgt(1)                   &
     &                      + grad_tgt(2)*grad_tgt(2)                   &
     &                      + grad_tgt(3)*grad_tgt(3))
              if(grad_len .ne. 0.0) then
                grad_tgt(1:3) = grad_tgt(1:3) / grad_len
              endif
! render section (clipping surface)

!
              if(lic_p%iflag_color_mode .eq. iflag_from_control) then
                scl_mid(1)                                              &
     &          = scl_org(1) * (1.0d0 - ratio) + scl_tgt(1) * ratio
                call s_lic_rgba_4_each_pixel                            &
     &             (viewpoint_vec, xx_lic_last, xx_lic,                 &
     &              scl_mid(1), grad_tgt, lic_tgt(1),                   &
     &              color_param, step_size, rgba_ray)
              else
                call s_lic_rgba_4_each_pixel                            &
     &             (viewpoint_vec, xx_lic_last, xx_lic,                 &
     &              lic_tgt(1), grad_tgt, lic_tgt(1),                   &
     &              color_param, step_size, rgba_ray)
              end if

              xx_lic_last = xx_lic
              step_cnt = step_cnt + 1
            end do
            ray_left = ray_len_left
!
! sampling by cell
          else
            ray_total_len = ray_total_len + ray_len
!   find mid point between xx_st and xx_tgt, this mid point will be actual sample point
            xx_lic = half*(xx_st + xx_tgt)
!
!   reference data at origin of lic iteration
            do i = 1, lic_p%num_masking
              if(lic_p%masking(i)%mask_type .eq. iflag_fieldmask) then
                r_mid(i) = half*(r_org(i)+r_tgt(i))
              end if
            end do
!   the vector interpolate from entry and exit point
            vec_mid(1:3) = half*(vec_org(1:3) + vec_tgt(1:3))
!   calculate lic value at current location, lic value will be used as intensity
!   as volume rendering
            call cal_lic_on_surf_vector                                 &
     &         (numnod, numsurf, numele, nnod_4_surf,                   &
     &          isf_4_ele, iele_4_surf, interior_surf, xx,              &
     &          isurf_orgs, ie_surf, xi, lic_p,                         &
     &          r_mid, vec_mid, field_pvr%s_lic,                        &
     &          field_pvr%v_lic, xx_lic, isurf_end,                     &
     &          xyz_min_gl, xyz_max_gl, iflag_lic,                      &
     &          lic_tgt(1), grad_tgt)
!
            ave_ray_len = ray_total_len / icount_line_cur_ray
!
!   normalize gradient
            grad_len = sqrt(grad_tgt(1)*grad_tgt(1)                     &
     &                    + grad_tgt(2)*grad_tgt(2)                     &
     &                    + grad_tgt(3)*grad_tgt(3))
            if(grad_len .ne. 0.0) then
              grad_tgt(1:3) = grad_tgt(1:3) / grad_len
            endif

            if(lic_p%iflag_color_mode .eq. iflag_from_control) then
              scl_mid(1) = half*(scl_org(1) + scl_tgt(1))
              call s_lic_rgba_4_each_pixel                              &
     &           (viewpoint_vec, xx_st, xx_tgt,                         &
     &            scl_mid(1), grad_tgt, lic_tgt(1),                     &
     &            color_param, ave_ray_len, rgba_ray)
            else
              call s_lic_rgba_4_each_pixel                              &
     &           (viewpoint_vec, xx_st, xx_tgt,                         &
     &            lic_tgt(1), grad_tgt, lic_tgt(1),                     &
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
        screen_st(1:3) = screen_tgt(1:3)
        xx_st(1:3) = xx_tgt(1:3)
        do i = 1, lic_p%num_masking
          if(lic_p%masking(i)%mask_type .eq. iflag_fieldmask) then
            r_org(i) = r_tgt(i)
          end if
        end do
        scl_org(1) =   scl_tgt(1)
        vec_org(1:3) = vec_tgt(1:3)
      end do
      end subroutine lic_ray_trace_each_pixel
!
!  ---------------------------------------------------------------------
!
      end module ray_trace_LIC_image
