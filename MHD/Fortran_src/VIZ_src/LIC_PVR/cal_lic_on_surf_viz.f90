!>@file  cal_lic_on_surf_viz.f90
!!       module cal_lic_on_surf_viz
!!
!!@author Yangguang Liao
!!@date   Programmed in 2018
!
!> @brief LIC tracing for each point
!!
!!@verbatim
!!      subroutine cal_lic_on_surf_vector(node, ele, surf,              &
!!     &          isurf_orgs, xi, lic_p, r_org, vec4_org, ref_nod,      &
!!     &          v_nod, xx4_org, isurf, iflag_comm, rlic_grad)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(lic_parameters), intent(in) :: lic_p
!!        integer(kind = kint), intent(in) :: isurf
!!        real(kind = kreal), intent(inout) :: xi(2)
!!        real(kind = kreal), intent(in) :: v_nod(node%numnod,3)
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: ref_nod(node%numnod,lic_p%num_masking)
!!        real(kind = kreal), intent(in) :: xx4_org(4), vec4_org(4)
!!        real(kind = kreal), intent(inout) :: rlic_grad(0:3), r_org(:)
!!        integer(kind = kint), intent(inout) :: iflag_comm
!!
!!      subroutine cal_surf_field_value_2d(nd, xi, fd, ft)
!!@endverbatim
!
      module cal_lic_on_surf_viz
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_surface_data
      use t_control_param_LIC
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
!>   xx_org is the point 3D coord, xi is local 2D coord on the surface
!!   isurf is current surface subdomain id
!!   isurf_org is an array with element id and surface element id(1-6)
      subroutine cal_lic_on_surf_vector(node, ele, surf,                &
     &          isurf_orgs, xi, lic_p, r_org, vec4_org, ref_nod,        &
     &          v_nod, xx4_org, isurf, iflag_comm, rlic_grad)

      use m_geometry_constants
      use calypso_mpi
      use t_noise_node_data
      use cal_noise_value
      use get_geometry_reference
      use cal_field_on_surf_viz
      use cal_fline_in_cube
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(lic_parameters), intent(in) :: lic_p
      integer(kind = kint), intent(in) :: isurf_orgs(2,3)
      integer(kind = kint), intent(in) :: isurf
!
      real(kind = kreal), intent(inout) :: xi(2)
      real(kind = kreal), intent(in) :: v_nod(node%numnod,3)
      real(kind = kreal), intent(in)                                    &
     &                   :: ref_nod(node%numnod,lic_p%num_masking)
      real(kind = kreal), intent(in) :: xx4_org(4), vec4_org(4)
      real(kind = kreal), intent(inout) :: rlic_grad(0:3), r_org(:)
      integer(kind = kint), intent(inout) :: iflag_comm
!        type(noise_mask), intent(inout) :: n_mask

      real(kind = kreal) :: xx4_ele_surf(4,num_linear_sf,nsurf_4_ele)
      real(kind = kreal) :: step_vec4(4), new_pos4(4)
      real(kind = kreal) :: rlic_grad_v(0:3)
      integer(kind = kint) :: ilic_suf_org(3), icur_sf
      integer(kind = kint) :: i, isf_tgt, k_mid
      real(kind = kreal) :: n_v, k_area
      integer(kind = kint) :: iflag_found_sf, iele, isf_org


        iflag_comm = 1
        k_area = 0.0

!     initial convolution integration at origin point
        rlic_grad_v(0) = 0.0
        rlic_grad(0:3) = 0.0
        icur_sf = isurf
        n_v = 0.0
        do i = 1, 2
          iele = isurf_orgs(i,1)
          isf_org = isurf_orgs(i,2)
          if(i_debug .eq. 1) write(50+my_rank,*)                        &
     &              "ele: ", iele, "local surf: ", isf_org
          if(i_debug .eq. 1) write(50+my_rank,*)                        &
     &              "global surf: ", isurf, "surf of ele",              &
     &               surf%isf_4_ele(iele, isf_org)
          if(iele .le. izero .or. iele .gt. ele%numele) then
            if(i_debug .eq. 1) write(50+my_rank,*)                      &
     &              "invalid element, end----------------------"
            iflag_comm = -5
            return
          end if
        end do

        if(lic_mask_flag(lic_p, r_org)) then
          call interpolate_noise_at_node                                &
     &       (xx4_org(1), lic_p%noise_t, rlic_grad_v)
        end if
        k_mid = (lic_p%kernel_t%n_knl + 1) / 2
        rlic_grad(0:3) = rlic_grad_v(0:3) * lic_p%kernel_t%k_ary(k_mid)

        if(i_debug .eq. 1) write(50+my_rank,*)                          &
     &     "--------------------Forward iter begin----------------"
!   forward integration
        step_vec4(1:4) = vec4_org(1:4)
        new_pos4(1:4) =  xx4_org(1:4)
! if current surface is exterior surface, then return.
!        if((surf%interior_surf(icur_sf) .eq. izero)                    &
!     &     .or. (icur_sf .eq. izero)) then
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
          call position_on_each_ele_surfs                               &
     &       (surf, node%numnod, node%xx, iele, xx4_ele_surf)
          call find_line_end_in_1ele(iflag_forward_line,                &
     &        isf_org, vec4_org, xx4_org, xx4_ele_surf,                 &
     &        isf_tgt, new_pos4, xi)
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
          call s_cal_lic_from_point(node, surf, lic_p,                  &
     &        iflag_forward_line, v_nod, ilic_suf_org, new_pos4,        &
     &        step_vec4, ref_nod, rlic_grad_v, k_area, iflag_comm)
          rlic_grad(0:3) = rlic_grad(0:3) + rlic_grad_v(0:3)
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
          call position_on_each_ele_surfs                               &
     &       (surf, node%numnod, node%xx, iele, xx4_ele_surf)
          call find_line_end_in_1ele(iflag_backward_line,               &
     &        isf_org, vec4_org, xx4_org, xx4_ele_surf,                 &
     &        isf_tgt, new_pos4, xi)
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
          call s_cal_lic_from_point(node, surf, lic_p,                  &
     &        iflag_backward_line, v_nod, ilic_suf_org,                 &
     &        new_pos4, step_vec4, ref_nod, rlic_grad_v, k_area,        &
     &        iflag_comm)
          rlic_grad(0:3) = rlic_grad(0:3) + rlic_grad_v(0:3)
        end if
        if(i_debug .eq. 1) write(50+my_rank,*)                          &
     &     "-----------------------Backward iter end------------with:", &
     &     iflag_comm

        if(k_area .gt. 0.0) then
          rlic_grad(0) = rlic_grad(0) / k_area
        end if
        rlic_grad(0) = rlic_grad(0) * lic_p%factor_normal

        if(i_debug .eq. 1) write(50+my_rank,*)                          &
     &                   "Get lic value: ", rlic_grad(0)
        if(i_debug .eq. 1) write(50+my_rank, *)"   "

    end subroutine cal_lic_on_surf_vector
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_lic_from_point(node, surf, lic_p,                &
     &          iflag_dir, vect_nod, isurf_org, x4_start, v4_start,     &
     &          ref_nod, rlic_grad_v, k_area, iflag_comm)

      use t_noise_node_data
      use cal_noise_value
      use cal_field_on_surf_viz
      use cal_fline_in_cube
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
!
      integer(kind = kint), intent(in) :: iflag_dir
      real(kind = kreal), intent(in) :: vect_nod(node%numnod,3)
      type(lic_parameters), intent(in) :: lic_p
      real(kind = kreal), intent(in)                                    &
     &                    :: ref_nod(node%numnod,lic_p%num_masking)
!
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: iflag_comm
      real(kind = kreal), intent(inout) :: v4_start(4), x4_start(4)
!
!      type(noise_node), intent(in) :: n_node(n_size)
!      type(noise_mask), intent(inout) :: n_mask
!
      real(kind = kreal), intent(inout) :: rlic_grad_v(0:3)
      real(kind = kreal), intent(inout) :: k_area
!
      integer(kind = kint) :: isf_tgt, isurf_end, isurf_start
      integer(kind = kint) :: iele, isf_org
      real(kind = kreal) :: v4_org(4), x4_org(4), xi(2)
      real(kind = kreal) :: v4_mid(4), x4_mid(4)
      real(kind = kreal) :: v4_tgt(4), x4_tgt(4)
      real(kind = kreal) :: step_seg(2), residual(2)
      real(kind = kreal) :: step_len, astep_len
      real(kind = kreal) :: step_unit(4,2), x4(4)
      integer(kind = kint) :: nstep_int(2)
      integer(kind = kint) :: i_iter, i_n, i
      real(kind = kreal) ::  nv_sum, len_sum, s_int
      real(kind = kreal) :: k_value, avg_stepsize
      real(kind = kreal) :: rnoise_grad(0:3)
      real(kind = kreal) :: ref_value(lic_p%num_masking)
      real(kind = kreal) :: xx4_ele_surf(4,num_linear_sf,nsurf_4_ele)
!
!
!init local variables
      i_iter = 0
! index of noise value
      i_n = 0
! noise value
      rnoise_grad(0:3) = 0.0
      rlic_grad_v(0:3) = 0.0
      nv_sum = 0.0
      avg_stepsize = 0.01
      len_sum = 0.0

      iflag_comm = 1
      if(isurf_org(1) .eq. 0) then
        iflag_comm = 0
        return
      end if

      iele =    isurf_org(1)
      isf_org = isurf_org(2)
      isurf_start = abs(surf%isf_4_ele(iele, isf_org))

!write(50 + my_rank, *) "start ele: ", isurf_org(1), "surf:", isurf_org(2)
      do
        step_seg(1:2) = 0.0d0
        x4_org(1:4) = x4_start(1:4)
        v4_org(1:4) = v4_start(1:4)
        i_iter = i_iter + 1
        if(isurf_start .lt. 1 .or. isurf_start .gt. surf%numsurf) then
          iflag_comm = -10
          return
        end if
!
!        if(surf%interior_surf(isurf_start) .eq. izero) then
!          iflag_comm = 10
!          return
!        end if
!        isf_tgt = 0

        do i = 1, 2
          iele = surf%iele_4_surf(isurf_start,i,1)
          isf_org = surf%iele_4_surf(isurf_start,i,2)
          if(iele .gt. 0) then
            call position_on_each_ele_surfs                             &
     &         (surf, node%numnod, node%xx, iele, xx4_ele_surf)
            call find_line_end_in_1ele(iflag_dir,                       &
     &          isf_org, v4_org, x4_org, xx4_ele_surf,                  &
     &          isf_tgt, x4_tgt, xi)
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
        isurf_end = abs(surf%isf_4_ele(iele,isf_tgt))
        call cal_field_on_surf_vect4                                    &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf,                &
     &      surf%ie_surf, isurf_end, xi, vect_nod, v4_tgt)
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
        call position_on_each_ele_surfs                                 &
     &     (surf, node%numnod, node%xx, iele, xx4_ele_surf)
        call find_line_end_in_1ele(iflag_dir,                           &
     &      isf_org, v4_mid, x4_mid, xx4_ele_surf,                      &
     &      isf_tgt, x4_tgt, xi)
!
        if(isf_tgt .eq. 0) then
          iflag_comm = -12
          exit
        end if
! exit point after 2nd field line trace
        isurf_end = abs(surf%isf_4_ele(iele,isf_tgt))
        call cal_field_on_surf_vect4                                    &
       &   (node%numnod, surf%numsurf, surf%nnod_4_surf,                &
       &    surf%ie_surf, isurf_end, xi, vect_nod, v4_tgt)
        step_seg(1) = sqrt( (x4_mid(1) - x4_org(1))**2                  &
       &                  + (x4_mid(2) - x4_org(2))**2                  &
       &                  + (x4_mid(3) - x4_org(3))**2)
        step_seg(2) = sqrt( (x4_tgt(1) - x4_mid(1))**2                  &
       &                  + (x4_tgt(2) - x4_mid(2))**2                  &
       &                  + (x4_tgt(3) - x4_mid(3))**2)
!
        step_len = step_seg(1) + step_seg(2)
        if(step_len .eq. 0.0d0) then
          astep_len = 1.0d7
        else
          astep_len = 1.0d0 / step_len
        end if
!
        step_unit(1:4,1) = (x4_mid(1:4) - x4_org(1:4)) / step_seg(1)
        step_unit(1:4,2) = (x4_tgt(1:4) - x4_mid(1:4)) / step_seg(2)
!
        nstep_int(1:2) = int(step_seg(1:2)*lic_p%noise_t%adelta_noise)
        residual(1:2) = step_seg(1:2)                                   &
       &           - lic_p%noise_t%delta_noise * dble(nstep_int(1:2))
!
        if(i_debug .eq. 1) write(50 + my_rank, *) "To  ",               &
       &                  isurf_end, "at elem", iele, "local", isf_tgt
        if(i_debug .eq. 1) write(50 + my_rank, *) "pos:", x4_tgt
        x4_start(1:4) =  x4_tgt(1:4)
        v4_start(1:4) =  v4_tgt(1:4)
!
        rnoise_grad(0:3) = 0.0
        ref_value(1:lic_p%num_masking) = 0.0
        do i = 1, lic_p%num_masking
          call cal_field_on_surf_scalar                                 &
      &      (node%numnod, surf%numsurf, surf%nnod_4_surf,              &
      &       surf%ie_surf, isurf_end, xi, ref_nod(1,i), ref_value(i))
        end do
!
        if(lic_mask_flag(lic_p, ref_value)) then
          do i = 1, nstep_int(1)
            x4(1:4) = x4_org(1:4)                                       &
     &               + lic_p%noise_t%delta_noise * step_unit(1:4,1)
            s_int = len_sum + dble(i) * lic_p%noise_t%delta_noise
            call interpolate_noise_at_node                              &
     &         (x4(1), lic_p%noise_t, rnoise_grad)
            call interpolate_kernel                                     &
     &         (iflag_dir, s_int, lic_p%kernel_t, k_value)
!
            nv_sum = nv_sum + rnoise_grad(0)                            &
     &              * lic_p%noise_t%delta_noise * astep_len
            rlic_grad_v(0:3) = rlic_grad_v(0:3)                         &
     &                      + rnoise_grad(0:3) * k_value                &
     &                       * lic_p%noise_t%delta_noise * astep_len
            k_area = k_area                                             &
     &         + k_value * lic_p%noise_t%delta_noise * astep_len
          end do
!
          s_int = len_sum + step_seg(1)
          call interpolate_noise_at_node                                &
     &       (x4_mid(1), lic_p%noise_t, rnoise_grad)
          call interpolate_kernel                                       &
     &       (iflag_dir, s_int, lic_p%kernel_t, k_value)
!
          nv_sum = nv_sum + rnoise_grad(0) * residual(1) * astep_len
          rlic_grad_v(0:3) = rlic_grad_v(0:3)                           &
     &                    + rnoise_grad(0:3) * k_value                  &
     &                     * residual(1) * astep_len
          k_area = k_area + k_value * residual(1) * astep_len
!
          do i = 1, nstep_int(2)
            x4(1:4) = x4_mid(1:4)                                       &
     &               + lic_p%noise_t%delta_noise * step_unit(1:4,2)
            s_int = len_sum + step_seg(1)                               &
     &                      + dble(i) * lic_p%noise_t%delta_noise
            call interpolate_noise_at_node                              &
     &         (x4(1), lic_p%noise_t, rnoise_grad)
            call interpolate_kernel                                     &
     &         (iflag_dir, s_int, lic_p%kernel_t, k_value)
!
            nv_sum = nv_sum + rnoise_grad(0)                            &
     &                     * lic_p%noise_t%delta_noise * astep_len
            rlic_grad_v(0:3) = rlic_grad_v(0:3)                         &
     &                      + rnoise_grad(0:3) * k_value                &
     &                       * lic_p%noise_t%delta_noise * astep_len
            k_area = k_area                                             &
     &          + k_value * lic_p%noise_t%delta_noise * astep_len
          end do
!
          s_int = len_sum + step_len
          call interpolate_noise_at_node                                &
     &       (x4_tgt(1), lic_p%noise_t, rnoise_grad)
          call interpolate_kernel                                       &
     &       (iflag_dir, s_int, lic_p%kernel_t, k_value)
          nv_sum = nv_sum + rnoise_grad(0) * residual(2) * astep_len
          rlic_grad_v(0:3) = rlic_grad_v(0:3)                           &
     &                    + rnoise_grad(0:3) * k_value                  &
     &                     * residual(2) * astep_len
          k_area = k_area + k_value * residual(2) * astep_len
        else
          s_int = len_sum + step_len
          call interpolate_kernel                                       &
     &       (iflag_dir, s_int, lic_p%kernel_t, k_value) 
          nv_sum = nv_sum + rnoise_grad(0)
          rlic_grad_v(0:3) = rlic_grad_v(0:3)                           &
     &                      + rnoise_grad(0:3) * k_value
          k_area = k_area + k_value
        end if
        len_sum = s_int
!
        if(i_debug .eq. 1) write(50 + my_rank, *)                       &
     &     "nv: ", rnoise_grad(0),  "nv sum:", nv_sum,                  &
     &    "kernel area: ", k_area, "lic_v: ", rlic_grad_v(0)

!        if(surf%interior_surf(isurf_end) .eq. izero) then
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
          !write(*,*) 'total length: ', len_sum, 'kernel', k_value,     &
!     &              'step', step_seg
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
