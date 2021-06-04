!>@file   lic_pixel_ray_trace_by_ele.f90
!!@brief  module lic_pixel_ray_trace_by_ele
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!>@brief structure of control data for multiple LIC rendering
!!
!!@verbatim
!!      subroutine s_lic_pixel_ray_trace_by_ele                         &
!!     &         (node, ele, surf, surf_grp, sf_grp_4_sf,               &
!!     &          x_nod_model, viewpoint_vec, modelview_mat, lic_p,     &
!!     &          field_lic, draw_param, color_param, ray_vec4,         &
!!     &          iflag_check, isurf_org, screen4_st, xx4_st, xi,       &
!!     &          rgba_ray, icount_line, elapse_trace, iflag_comm)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!@endverbatim
!
      module lic_pixel_ray_trace_by_ele
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
      use m_geometry_constants
      use calypso_mpi
      use lic_rgba_4_each_pixel
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surf_grp_list_each_surf
      use t_control_params_4_pvr
      use t_control_param_LIC
      use t_geometries_in_pvr_screen
      use t_lic_field_data
      use cal_lic_on_surf_viz
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_lic_pixel_ray_trace_by_ele                           &
     &         (node, ele, surf, surf_grp, sf_grp_4_sf,                 &
     &          x_nod_model, viewpoint_vec, modelview_mat, lic_p,       &
     &          field_lic, draw_param, color_param, ray_vec4,           &
     &          iflag_check, isurf_org, screen4_st, xx4_st, xi,         &
     &          rgba_ray, icount_line, elapse_trace, iflag_comm)
!
      use cal_field_on_surf_viz
      use cal_fline_in_cube
      use set_coefs_of_sections
      use set_rgba_4_each_pixel
      use pvr_surface_enhancement
      use t_noise_node_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      integer(kind = kint), intent(in) :: iflag_check
!
      real(kind = kreal), intent(in) :: x_nod_model(node%numnod,4)
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
      real(kind = kreal), intent(in) :: ray_vec4(4)
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
      real(kind = kreal), intent(inout) :: elapse_trace
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

      integer(kind = kint) :: icount_line_cur_ray = 0, step_cnt
      real(kind = kreal) :: ray_len_left, ray_left, ray_len, ratio
      real(kind = kreal) :: start_trace
      real(kind = kreal) :: opacity_bc
!
      if(isurf_org(1) .eq. 0) return
!
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
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf,                  &
     &    surf%ie_surf, isurf_end, xi, node%xx, xx4_st)
      call cal_field_on_surf_vect4                                      &
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf,                  &
     &    surf%ie_surf, isurf_end, xi, field_lic%v_lic, vec4_org)
      call cal_field_on_surf_scalar                                     &
     &   (node%numnod, surf%numsurf, surf%nnod_4_surf,                  &
     &    surf%ie_surf, isurf_end, xi, field_lic%d_lic, scl_org)

      allocate(r_org(lic_p%num_masking))
      allocate(r_tgt(lic_p%num_masking))
      allocate(r_mid(lic_p%num_masking))
      do i = 1, lic_p%num_masking
        call cal_field_on_surf_scalar                                   &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,  &
     &      isurf_end, xi, field_lic%s_lic(1,i), r_org(i))
      end do
!
!
      if(ele%interior_ele(iele) .gt. 0) then
!   rendering boundery
        opacity_bc = opacity_by_surf_grp(isurf_end, surf, surf_grp,     &
     &          sf_grp_4_sf, modelview_mat,                             &
     &          draw_param%iflag_enhanse, draw_param%enhansed_opacity)
        if(opacity_bc .gt. SMALL_RAY_TRACE) then
          rlic_grad(1:3) = surf%vnorm_surf(isurf_end,1:3)
          call plane_rendering_with_light(viewpoint_vec,                &
     &        xx4_st, rlic_grad(1), opacity_bc, color_param, rgba_ray)
        end if
      end if
!
!   start ray casting
      do
        iele =    isurf_org(1)
        isf_org = isurf_org(2)
!
        if(draw_param%iflag_used_ele(iele) .eq. 0) then
          iflag_comm = 2
          exit
        end if
!
        icount_line = icount_line + 1
        icount_line_cur_ray = icount_line_cur_ray + 1
!
!   extend to surface of element
!   find ray exit surface loacal id on current element isf_tgt
!
        call find_line_end_in_1ele                                      &
     &     (iflag_backward_line, node%numnod, ele%numele,               &
     &      surf%numsurf, surf%nnod_4_surf, surf%isf_4_ele,             &
     &      surf%ie_surf, x_nod_model, iele, isf_org,                   &
     &      ray_vec4, screen4_st, isf_tgt, screen4_tgt, xi)
!        if(iflag_check .gt. 0) write(*,*) 'screen4_tgt',               &
!     &      my_rank, screen4_tgt(1:3), ele%interior_ele(iele)
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
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,  &
     &      isurf_end, xi, node%xx, xx4_tgt)
        call cal_field_on_surf_vect4                                    &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,  &
     &      isurf_end, xi, field_lic%v_lic, vec4_tgt)
        call cal_field_on_surf_scalar                                   &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,  &
     &      isurf_end, xi, field_lic%d_lic, scl_tgt)

        do i = 1, lic_p%num_masking
          call cal_field_on_surf_scalar(node%numnod,                    &
     &        surf%numsurf, surf%nnod_4_surf, surf%ie_surf,             &
     &        isurf_end, xi, field_lic%s_lic(1,i), r_tgt(i))
        end do

        rlic_grad(0) = 0.0d0
!
        if(ele%interior_ele(iele) .gt. 0) then
!   rendering boundery
          opacity_bc = opacity_by_surf_grp(isurf_end, surf, surf_grp,   &
     &          sf_grp_4_sf, modelview_mat,                             &
     &          draw_param%iflag_enhanse, draw_param%enhansed_opacity)
          if(opacity_bc .gt. SMALL_RAY_TRACE) then
            rlic_grad(1:3) = surf%vnorm_surf(isurf_end,1:3)
            call plane_rendering_with_light(viewpoint_vec, xx4_tgt,     &
     &          rlic_grad(1), opacity_bc,  color_param, rgba_ray)
          end if
!
!   3d lic calculation at current xx position
!   if sampling by fixed step size
          ray_len = sqrt( (xx4_tgt(1) - xx4_st(1))**2                   &
     &                  + (xx4_tgt(2) - xx4_st(2))**2                   &
     &                  + (xx4_tgt(3) - xx4_st(3))**2)
            ray_len_left = ray_left + ray_len

            xx4_lic_last(1:4) = xx4_st(1:4)
            step_cnt = 0
            do while(ray_len_left .gt. zero)
              ray_len_left = ray_len_left - lic_p%step_size
              ratio = (lic_p%step_size*step_cnt - ray_left) / ray_len
              xx4_lic(1:4) = xx4_st(1:4)                                &
     &                    + ratio * (xx4_tgt(1:4) - xx4_st(1:4))
!
              do i = 1, lic_p%num_masking
                r_mid(i) = r_org(i) * (1.0d0 - ratio) + r_tgt(i)*ratio
!write(*,*) "org", r_org, "tgt", r_tgt, "ratio", ratio
              end do
! masking on sampling point
!              if(lic_mask_flag(lic_p, r_mid)) then

              start_trace =  MPI_WTIME()
              vec4_mid(1:4) = vec4_org(1:4) * (1.0d0 - ratio)           &
     &                       + vec4_tgt(1:4) * ratio
              call cal_lic_on_surf_vector(node%numnod,                  &
     &            surf%numsurf, ele%numele, surf%nnod_4_surf,           &
     &            surf%isf_4_ele, surf%iele_4_surf, surf%interior_surf, &
     &            node%xx, isurf_orgs, surf%ie_surf, xi, lic_p,         &
     &            r_mid, vec4_mid, field_lic%s_lic(1,1),                &
     &            field_lic%v_lic, xx4_lic, isurf_end,                  &
     &            iflag_lic, rlic_grad)
              elapse_trace =  elapse_trace + MPI_WTIME() - start_trace
!
  !   normalize gradient
              grad_len = sqrt(rlic_grad(1)*rlic_grad(1)                 &
     &                      + rlic_grad(2)*rlic_grad(2)                 &
     &                      + rlic_grad(3)*rlic_grad(3))
              if(grad_len .ne. 0.0) then
                rlic_grad(1:3) = rlic_grad(1:3) / grad_len
              end if
!
              do i_psf = 1, draw_param%num_sections
                rflag =  side_of_plane(draw_param%coefs(1:10,i_psf),    &
     &                               xx4_st(1))
                rflag2 = side_of_plane(draw_param%coefs(1:10,i_psf),    &
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
                  call cal_normal_of_plane                              &
     &               (draw_param%coefs(1:10,i_psf),                     &
     &                xx4_tgt(1), grad_tgt)
                  call color_plane_with_light                           &
     &               (viewpoint_vec, xx4_tgt, rlic_grad(0), grad_tgt,   &
     &                draw_param%sect_opacity(i_psf), color_param,      &
     &                rgba_ray)
                end if
              end do
!
! render section (clipping surface)
              if(lic_p%iflag_color_mode .eq. iflag_from_control) then
                scl_mid(1)                                              &
     &            = scl_org(1) * (1.0d0 - ratio) + scl_tgt(1) * ratio
                call s_lic_rgba_4_each_pixel                            &
     &             (viewpoint_vec, xx4_lic_last, xx4_lic,               &
     &              scl_mid(1), rlic_grad(1), rlic_grad(0),             &
     &              color_param, lic_p%step_size, rgba_ray)
              else
                call s_lic_rgba_4_each_pixel                            &
     &             (viewpoint_vec, xx4_lic_last, xx4_lic,               &
     &              rlic_grad(0), rlic_grad(1), rlic_grad(0),           &
     &              color_param, lic_p%step_size, rgba_ray)
              end if

              xx4_lic_last(1:4) = xx4_lic(1:4)
              step_cnt = step_cnt + 1
            end do
            ray_left = ray_len_left
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
        r_org(1:lic_p%num_masking) = r_tgt(1:lic_p%num_masking)
        scl_org(1) =    scl_tgt(1)
        vec4_org(1:4) = vec4_tgt(1:4)
      end do
!
      end subroutine s_lic_pixel_ray_trace_by_ele
!
!  ---------------------------------------------------------------------
!
      end module lic_pixel_ray_trace_by_ele
