!cal_lic_on_surf_viz.f90
!
!      module cal_lic_on_surf_viz
!
!      Written by Yangguang Liao 2018
!
!!      subroutine cal_lic_on_surf_vector                               &
!!     &         (nnod, nsurf, nelem, nnod_4_surf, isf_4_ele,           &
!!     &          iele_4_surf, interior_surf, xx,                       &
!!     &          isurf_orgs, ie_surf, xi, lic_p,                       &
!!     &          r_org, vec4_org, ref_nod,                             &
!!     &          v_nod, xx4_org, isurf, xyz_min, xyz_max, iflag_comm,  &
!!     &          o_tgt, n_grad)
!!
!!      subroutine cal_surf_field_value_2d(nd, xi, fd, ft)
!
      module cal_lic_on_surf_viz
!
      use m_precision
      use m_constants
      use t_control_param_LIC
      use cal_field_on_surf_viz
      use cal_fline_in_cube
!
      implicit  none
!
      private :: s_cal_lic_from_point
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
!   xx_org is the point 3D coord, xi is local 2D coord on the surface
!   isurf is current surface subdomain id
!   isurf_org is an array with element id and surface element id(1-6)
      subroutine cal_lic_on_surf_vector                                 &
     &         (nnod, nsurf, nelem, nnod_4_surf, isf_4_ele,             &
     &          iele_4_surf, interior_surf, xx,                         &
     &          isurf_orgs, ie_surf, xi, lic_p,                         &
     &          r_org, vec4_org, ref_nod,                               &
     &          v_nod, xx4_org, isurf, xyz_min, xyz_max, iflag_comm,    &
     &          o_tgt, n_grad)

        use m_geometry_constants
        use calypso_mpi
        use t_noise_node_data
        use cal_noise_value

        integer(kind = kint), intent(in) :: isurf_orgs(2,3)
        integer(kind = kint), intent(in) :: nnod, nsurf, nelem
        integer(kind = kint), intent(in) :: nnod_4_surf, isurf
        integer(kind = kint), intent(in)                                &
     &                :: isf_4_ele(nelem, nsurf_4_ele)
        integer(kind = kint), intent(in) :: iele_4_surf(nsurf, 2, 2)
        integer(kind = kint), intent(in) :: interior_surf(nsurf)
        integer(kind = kint), intent(in) :: ie_surf(nsurf,nnod_4_surf)
!
        type(lic_parameters), intent(in) :: lic_p
!
        real(kind = kreal), intent(inout) :: xi(2)
        real(kind = kreal), intent(in) :: v_nod(nnod,3), xx(nnod, 3)
        real(kind = kreal), intent(in) :: ref_nod(nnod,lic_p%num_masking)
        real(kind = kreal), intent(in) :: xx4_org(4), vec4_org(4)
        real(kind = kreal), intent(inout) :: o_tgt, n_grad(3), r_org(:)
        integer(kind = kint), intent(inout) :: iflag_comm
!        type(noise_mask), intent(inout) :: n_mask

        real(kind = kreal), intent(in) :: xyz_min(3)
        real(kind = kreal), intent(in) :: xyz_max(3)

        real(kind = kreal) :: step_vec4(4), new_pos4(4)
        integer(kind = kint) :: ilic_suf_org(3), icur_sf
        integer(kind = kint) :: i, isf_tgt, k_mid
        real(kind = kreal) :: lic_v, n_v, k_area
        integer(kind = kint) :: iflag_found_sf, iele, isf_org


        iflag_comm = 1
        k_area = 0.0

!     initial convolution integration at origin point
        lic_v = 0.0
        o_tgt = 0.0
        n_grad(1:3) = 0.0
        icur_sf = isurf
        n_v = 0.0
        do i = 1, 2
          iele = isurf_orgs(i,1)
          isf_org = isurf_orgs(i,2)
          if(i_debug .eq. 1) write(50+my_rank,*)                        &
     &              "ele: ", iele, "local surf: ", isf_org
          if(i_debug .eq. 1) write(50+my_rank,*)                        &
     &              "global surf: ", isurf, "surf of ele",              &
     &               isf_4_ele(iele, isf_org)
          if(iele .le. izero .or. iele .gt. nelem) then
            if(i_debug .eq. 1) write(50+my_rank,*)                      &
     &              "invalid element, end----------------------"
            iflag_comm = -5
            return
          end if
        end do

        do i = 1, lic_p%num_masking
          if(lic_p%masking(i)%mask_type .eq. iflag_geometrymask) then
            r_org(i) = get_geometry_reference(lic_p, i, xx4_org)
          end if
        end do
        if(mask_flag(lic_p, r_org)) then
          call interpolate_noise_at_node                                &
     &       (xx4_org(1), lic_p%noise_t, n_v, n_grad)
        end if
        k_mid = (lic_p%kernel_t%n_knl + 1) / 2
        o_tgt = o_tgt + n_v * lic_p%kernel_t%k_ary(k_mid)
        n_grad = n_grad + n_grad * lic_p%kernel_t%k_ary(k_mid)

        if(i_debug .eq. 1) write(50+my_rank,*)                          &
     &     "--------------------Forward iter begin----------------"
!   forward integration
        step_vec4(1:4) = vec4_org(1:4)
        new_pos4(1:4) =  xx4_org(1:4)
! if current surface is exterior surface, then return.
!        if((interior_surf(icur_sf) .eq. izero) .or. (icur_sf .eq. izero)) then
        if(icur_sf .eq. izero) then
          if(i_debug .eq. 1) write(50+my_rank,*)                        &
     &       "extorior surface, end-------------------------", icur_sf
          iflag_comm = -1
          return
        end if

        iflag_found_sf = 0

        do i = 1, 2
          iele = isurf_orgs(i,1)
          isf_org = isurf_orgs(i,2)
          call find_line_end_in_1ele                                    &
     &       (iflag_forward_line, nnod, nelem, nsurf,                   &
     &        nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,       &
     &        vec4_org, xx4_org, isf_tgt, new_pos4, xi)
          if(isf_tgt .gt. 0) then
            !write(50+my_rank, *) "find exit point in neighbor element."
            iflag_found_sf = 1
            ilic_suf_org(1:2) = isurf_orgs(i,1:2)
            exit
          end if
        end do

        if(iflag_found_sf .eq. 0) then
          if(i_debug .eq. 1) write(50+my_rank, *)                       &
     &      "not find exit point in neighbor element. end------------"
          iflag_comm = -2
        else
          new_pos4(1:4) = xx4_org(1:4)
          if(i_debug .eq. 1) write(50+my_rank, *)                       &
     &                          "start cal lic, ele and surf: ",        &
     &                          ilic_suf_org(1), ilic_suf_org(2)
          call s_cal_lic_from_point(nnod, nelem, nsurf,                 &
     &        nnod_4_surf, xx, ie_surf, isf_4_ele,                      &
     &        iele_4_surf, interior_surf, lic_p,                        &
     &        iflag_forward_line, xyz_min, xyz_max,                     &
     &        v_nod, ilic_suf_org, new_pos4, step_vec4, ref_nod,        &
     &        lic_v, n_grad, k_area, iflag_comm)
          o_tgt = o_tgt + lic_v
        end if
        if(i_debug .eq. 1) write(50+my_rank,*)                          &
     &     "-----------------------Forward iter end-------------with:", &
     &     iflag_comm

        if(i_debug .eq. 1) write(50+my_rank,*)                          &
     &     "-----------------------Backward iter begin---------------"
!   Backward iteration
        iflag_found_sf = 0
        step_vec4(1:4) = vec4_org(1:4)
        new_pos4(1:4) =  xx4_org(1:4)
        do i = 1, 2
          iele = isurf_orgs(i,1)
          isf_org = isurf_orgs(i,2)
          call find_line_end_in_1ele                                    &
     &     (iflag_backward_line, nnod, nelem, nsurf,                    &
     &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,         &
     &      vec4_org, xx4_org, isf_tgt, new_pos4, xi)
          if(isf_tgt .gt. 0) then
            !write(50+my_rank, *) "find exit point in neighbor element."
            iflag_found_sf = 1
            ilic_suf_org(1:2) = isurf_orgs(i,1:2)
            exit
          end if
        end do

        if(iflag_found_sf .eq. 0) then
          if(i_debug .eq. 1) write(50+my_rank, *)                       &
     &       "not find exit point in neighbor element ",                &
     &       " end-----------------"
          iflag_comm = -2
        else
          new_pos4(1:4) = xx4_org(1:4)
          if(i_debug .eq. 1) write(50+my_rank, *)                       &
     &       "start cal lic, ele and surf: ",                           &
     &       ilic_suf_org(1), ilic_suf_org(2)
          call s_cal_lic_from_point(nnod, nelem, nsurf,                 &
     &          nnod_4_surf, xx, ie_surf, isf_4_ele,                    &
     &          iele_4_surf, interior_surf, lic_p,                      &
     &          iflag_backward_line, xyz_min, xyz_max,                  &
     &          v_nod, ilic_suf_org, new_pos4, step_vec4,               &
     &          ref_nod, lic_v, n_grad, k_area, iflag_comm)
          o_tgt = o_tgt + lic_v
        end if
        if(i_debug .eq. 1) write(50+my_rank,*)                          &
     &     "-----------------------Backward iter end------------with:", &
     &     iflag_comm

        if(k_area .gt. 0.0) then
          o_tgt = o_tgt / k_area
        end if
        o_tgt = o_tgt * lic_p%factor_normal

        if(i_debug .eq. 1) write(50+my_rank,*) "Get lic value: ", o_tgt
        if(i_debug .eq. 1) write(50+my_rank, *)"   "

    end subroutine cal_lic_on_surf_vector
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_lic_from_point(numnod, numele, numsurf,          &
     &          nnod_4_surf, xx, ie_surf, isf_4_ele,                    &
     &          iele_4_surf, interior_surf, lic_p,                      &
     &          iflag_dir, xyz_min, xyz_max,                            &
     &          vect_nod, isurf_org, x4_start, v4_start, ref_nod,       &
     &          lic_v, grad_v, k_area, iflag_comm)

      use t_noise_node_data
      use cal_noise_value
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      real(kind = kreal), intent(in) :: xx(numnod,3)
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      integer(kind = kint), intent(in) :: interior_surf(numsurf)
!
      integer(kind = kint), intent(in) :: iflag_dir
      real(kind = kreal), intent(in) :: vect_nod(numnod,3)
      type(lic_parameters), intent(in) :: lic_p
      real(kind = kreal), intent(in) :: ref_nod(numnod,lic_p%num_masking)
!
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: iflag_comm
      real(kind = kreal), intent(inout) :: v4_start(4), x4_start(4)
!
!      type(noise_node), intent(in) :: n_node(n_size)
!      type(noise_mask), intent(inout) :: n_mask
!
      real(kind = kreal), intent(in) :: xyz_min(3)
      real(kind = kreal), intent(in) :: xyz_max(3)
!
      real(kind = kreal), intent(inout) :: lic_v, k_area, grad_v(3)
!
      integer(kind = kint) :: isf_tgt, isurf_end, isurf_start
      integer(kind = kint) :: iele, isf_org
      real(kind = kreal) :: v4_org(4), x4_org(4), xi(2)
      real(kind = kreal) :: v4_mid(4), x4_mid(4)
      real(kind = kreal) :: v4_tgt(4), x4_tgt(4)
      real(kind = kreal) :: step_len(2), step_rst(2)
      real(kind = kreal) :: step_unit(4,2), x4(4)
      integer(kind = kint) :: nstep_int(2)
      integer(kind = kint) :: i_iter, i_n, i
      real(kind = kreal) :: n_v, nv_sum, len_sum, s_int
      real(kind = kreal) ::  k_value, avg_stepsize
      real(kind = kreal) :: g_v(3), ref_value(lic_p%num_masking)
!
!
!init local variables
      i_iter = 0
      i_n = 0 ! index of noise value
      n_v = 0.0 ! noise value
      g_v(1:3) = 0.0
      nv_sum = 0.0
      lic_v = 0.0
      avg_stepsize = 0.01
      len_sum = 0.0

      iflag_comm = 1
      if(isurf_org(1) .eq. 0) then
        iflag_comm = 0
        return
      end if

      iele =    isurf_org(1)
      isf_org = isurf_org(2)
      isurf_start = abs(isf_4_ele(iele, isf_org))

!write(50 + my_rank, *) "start ele: ", isurf_org(1), "surf:", isurf_org(2)
      do
        step_len(1:2) = 0.0d0
        x4_org(1:4) = x4_start(1:4)
        v4_org(1:4) = v4_start(1:4)
        i_iter = i_iter + 1
        if(isurf_start .lt. 1 .or. isurf_start .gt. numsurf) then
          iflag_comm = -10
          return
        end if
!
!        if(interior_surf(isurf_start) .eq. izero) then
!          iflag_comm = 10
!          return
!        end if
!        isf_tgt = 0

        do i = 1, 2
          iele = iele_4_surf(isurf_start,i,1)
          isf_org = iele_4_surf(isurf_start,i,2)
          if(iele .gt. 0) then
            call find_line_end_in_1ele                                  &
     &         (iflag_dir, numnod, numele, numsurf,                     &
     &          nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,     &
     &          v4_org, x4_org, isf_tgt, x4_tgt, xi)
            if(isf_tgt .gt. 0) then
! find hit surface
              exit
            end if
          end if
        end do

        if(isf_tgt .eq. 0) then
          iflag_comm = -11
          exit
        end if
! find next point on one surface
        if(i_debug .eq. 1) write(50 + my_rank, *)                       &
     &         "From", isurf_start, "at elem", iele, "local", isf_org
        if(i_debug .eq. 1) write(50 + my_rank, *) "pos:", x4_org
!
        isurf_end = abs(isf_4_ele(iele,isf_tgt))
        call cal_field_on_surf_vect4(numnod, numsurf, nnod_4_surf,      &
     &      ie_surf, isurf_end, xi, vect_nod, v4_tgt)
!
        isf_org =  0
        x4_mid(1:4) = half * (x4_org(1:4) + x4_tgt(1:4))
        v4_mid(1:4) = half * (v4_mid(1:4) + v4_tgt(1:4))
        if(i_debug .eq. 1) write(50 + my_rank, *)                       &
     &      "first hit pos:", x4_tgt
        if(i_debug .eq. 1) write(50 + my_rank, *)                       &
     &      "middle pos:", x4_mid
!
!   extend to surface of element
!
        call find_line_end_in_1ele(iflag_dir, numnod, numele, numsurf,  &
     &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,         &
     &      v4_mid, x4_mid, isf_tgt, x4_tgt, xi)
!
        if(isf_tgt .eq. 0) then
          iflag_comm = -12
          exit
        end if
! exit point after 2nd field line trace
        isurf_end = abs(isf_4_ele(iele,isf_tgt))
        call cal_field_on_surf_vect4(numnod, numsurf, nnod_4_surf,      &
       &    ie_surf, isurf_end, xi, vect_nod, v4_tgt)
        step_len(1) = sqrt( (x4_mid(1) - x4_org(1))**2                  &
       &                  + (x4_mid(2) - x4_org(2))**2                  &
       &                  + (x4_mid(3) - x4_org(3))**2)
        step_len(2) = sqrt( (x4_tgt(1) - x4_mid(1))**2                  &
       &                  + (x4_tgt(2) - x4_mid(2))**2                  &
       &                  + (x4_tgt(3) - x4_mid(3))**2)
!
        step_unit(1:4,1) = (x4_mid(1:4) - x4_org(1:4)) / step_len(1)
        step_unit(1:4,2) = (x4_tgt(1:4) - x4_mid(1:4)) / step_len(2)
!
        nstep_int(1:2) = int(step_len(1:2)*lic_p%noise_t%adelta_noise)
        step_rst(1:2) = step_len(1:2) * lic_p%noise_t%adelta_noise      &
       &               - dble(nstep_int(1:2))
!
        if(i_debug .eq. 1) write(50 + my_rank, *) "To  ",               &
       &                  isurf_end, "at elem", iele, "local", isf_tgt
        if(i_debug .eq. 1) write(50 + my_rank, *) "pos:", x4_tgt
        x4_start(1:4) =  x4_tgt(1:4)
        v4_start(1:4) =  v4_tgt(1:4)
!
        n_v = 0.0
        g_v(1:3) = 0.0
        ref_value(:) = 0.0
        do i = 1, lic_p%num_masking
          if(lic_p%masking(i)%mask_type .eq. iflag_fieldmask) then
            call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf, &
      &         ie_surf, isurf_end, xi, ref_nod(1,i), ref_value(i))
          else
            ref_value(i) = get_geometry_reference(lic_p, i, x4_tgt)
          end if
        end do
!
        if(mask_flag(lic_p, ref_value)) then
          do i = 1, nstep_int(1)
            x4(1:4) = x4_org(1:4)                                       &
     &               + lic_p%noise_t%delta_noise * step_unit(1:4,1)
            s_int = len_sum + dble(i) * lic_p%noise_t%delta_noise
            call interpolate_noise_at_node                              &
     &         (x4(1), lic_p%noise_t, n_v, g_v)
            call interpolate_kernel                                     &
     &         (iflag_dir, s_int, lic_p%kernel_t, k_value)
            nv_sum = nv_sum + n_v * lic_p%noise_t%delta_noise/(step_len(1) + step_len(2))
            lic_v = lic_v + n_v * k_value * lic_p%noise_t%delta_noise/(step_len(1) + step_len(2))
            grad_v = grad_v + g_v * k_value * lic_p%noise_t%delta_noise/(step_len(1) + step_len(2))
            k_area = k_area + k_value * lic_p%noise_t%delta_noise/(step_len(1) + step_len(2))
          end do
!
          s_int = len_sum + step_len(1)
          call interpolate_noise_at_node                                &
     &       (x4_mid(1), lic_p%noise_t, n_v, g_v)
          call interpolate_kernel                                       &
     &       (iflag_dir, s_int, lic_p%kernel_t, k_value)
          nv_sum = nv_sum + n_v * step_rst(1)/(step_len(1) + step_len(2))
          lic_v = lic_v + n_v * k_value * step_rst(1)/(step_len(1) + step_len(2))
          grad_v = grad_v + g_v * k_value * step_rst(1)/(step_len(1) + step_len(2))
          k_area = k_area + k_value * step_rst(1)/(step_len(1) + step_len(2))
!
          do i = 1, nstep_int(2)
            x4(1:4) = x4_mid(1:4)                                       &
     &               + lic_p%noise_t%delta_noise * step_unit(1:4,2)
            s_int = len_sum + step_len(1)                               &
     &                      + dble(i) * lic_p%noise_t%delta_noise
            call interpolate_noise_at_node                              &
     &         (x4(1), lic_p%noise_t, n_v, g_v)
            call interpolate_kernel                                     &
     &         (iflag_dir, s_int, lic_p%kernel_t, k_value)
            nv_sum = nv_sum + n_v * lic_p%noise_t%delta_noise/(step_len(1) + step_len(2))
            lic_v = lic_v + n_v * k_value * lic_p%noise_t%delta_noise/(step_len(1) + step_len(2))
            grad_v = grad_v + g_v * k_value * lic_p%noise_t%delta_noise/(step_len(1) + step_len(2))
            k_area = k_area + k_value * lic_p%noise_t%delta_noise/(step_len(1) + step_len(2))
          end do
!
          s_int = len_sum + step_len(1) + step_len(2)
          call interpolate_noise_at_node                                &
     &       (x4_tgt(1), lic_p%noise_t, n_v, g_v)
          call interpolate_kernel                                       &
     &       (iflag_dir, s_int, lic_p%kernel_t, k_value)
          nv_sum = nv_sum + n_v * step_rst(2)/(step_len(1) + step_len(2))
          lic_v = lic_v + n_v * k_value * step_rst(2)/(step_len(1) + step_len(2))
          grad_v = grad_v + g_v * k_value * step_rst(2)/(step_len(1) + step_len(2))
          k_area = k_area + k_value * step_rst(2)/(step_len(1) + step_len(2))
!          nv_sum = nv_sum + n_v * step_rst(2)
!          lic_v = lic_v + n_v * k_value * step_rst(2)
!          grad_v = grad_v + g_v * k_value * step_rst(2)
!          k_area = k_area + k_value * step_rst(2)
        else
          s_int = len_sum + step_len(1) + step_len(2)
          call interpolate_kernel                                       &
     &       (iflag_dir, s_int, lic_p%kernel_t, k_value) 
          nv_sum = nv_sum + n_v
          lic_v = lic_v + n_v * k_value
          grad_v = grad_v + g_v * k_value
          k_area = k_area + k_value
        end if
        len_sum = s_int
!
        if(i_debug .eq. 1) write(50 + my_rank, *) "nv: ", n_v,          &
     &     "nv sum:", nv_sum, "kernel area: ", k_area, "lic_v: ", lic_v

!        if(interior_surf(isurf_end) .eq. izero) then
!          isurf_start = isurf_end
!          iflag_comm = 10
!          exit
!        else
!          isurf_start = isurf_end
!        end if
!       will use exterior surface(surface in ghost layer ) to cal lic
        isurf_start = isurf_end

        if(flag_lic_end(lic_p%kernel_t, len_sum, i_iter)) then
!        if(len_sum .ge.max_line_len) then
          iflag_comm = 1
          exit
        end if
        if(i_iter .gt. 200) then
          !write(*,*) 'iteration too large in 1: ', i_iter
          !write(*,*) 'total length: ', len_sum, 'kernel', k_value, 'step', step_len
          return
        end if
      end do
!
      return
!
      end subroutine s_cal_lic_from_point
!
!  ---------------------------------------------------------------------
!
      logical function flag_lic_end(knl, cur_len, cur_cnt)
!
      type(LIC_kernel), intent(in) :: knl
      integer(kind = kint), intent(in) :: cur_cnt
      real(kind = kreal), intent(in) :: cur_len
!
      flag_lic_end = .FALSE.
      if(knl%iflag_trace_type .eq. iflag_by_lengh) then
        if(cur_len .ge. knl%half_lengh) flag_lic_end = .TRUE.
      else
        if(cur_cnt .gt. knl%max_trace_count) flag_lic_end = .TRUE.
      end if
!
      end function flag_lic_end
!
!  ---------------------------------------------------------------------
!
      end module cal_lic_on_surf_viz
