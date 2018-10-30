!cal_lic_on_surf_viz.f90
!
!      module cal_lic_on_surf_viz
!
!      Written by Yangguang Liao 2018
!
!!      subroutine cal_lic_on_surf_vector                               &
!!     &         (nnod, nsurf, nelem, nnod_4_surf, isf_4_ele,           &
!!     &          iele_4_surf, interior_surf, xx,                       &
!!     &          isurf_orgs, ie_surf, xi,                              &
!!     &          f_noise, factor_lic_magnify, noise_size,              &
!!     &          noise_nod, noise_grad, kernal_size, kernal_node,      &
!!     &          v_nod, xx_org, isurf, xyz_min, xyz_max, iflag_comm,   &
!!     &          o_tgt, n_grad)
!
!      subroutine cal_field_on_surf_vector(nnod, nsurf, nnod_sf,        &
!     &          ie_surf, isurf, xi, v_nod, v_tgt)
!      subroutine cal_field_on_surf_scalar(nnod, nsurf, nnod_sf,        &
!     &           ie_surf, isurf, xi, s_nod, s_tgt)
!
!      subroutine cal_surf_field_value_2d(nd, xi, fd, ft)
!
      module cal_lic_on_surf_viz
!
      use m_precision
      use m_constants
      use t_control_param_LIC
      use lic_noise_generator
      use lic_kernel_generator
      use cal_field_on_surf_viz
      use cal_fline_in_cube
!
      implicit  none
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
     &          r_org, vec_org, ref_nod,                                &
     &          kernal_size, kernal_node,                               &
     &          v_nod, xx_org, isurf, xyz_min, xyz_max, iflag_comm,     &
     &          o_tgt, n_grad)

        use m_geometry_constants
        use calypso_mpi
        use t_noise_node_data

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
        real(kind = kreal), intent(in)                                  &
     &             :: ref_nod(nnod,lic_p%num_masking)
        real(kind = kreal), intent(in)                                  &
     &             :: xx_org(3), r_org(:), vec_org(3)
        real(kind = kreal), intent(inout) :: o_tgt, n_grad(3)
        integer(kind = kint), intent(inout) :: iflag_comm
        integer(kind = kint), intent(in) :: kernal_size
        real(kind = kreal), intent(in) :: kernal_node(kernal_size)
        !type(noise_node), intent(in) :: noise_nod(noise_size)
        !type(noise_mask), intent(inout) :: n_mask

        real(kind = kreal), intent(in) :: xyz_min(3)
        real(kind = kreal), intent(in) :: xyz_max(3)

        integer(kind = kint) :: iflag_back

        real(kind = kreal) :: step_vec(3), new_pos(3)
        integer(kind = kint) :: ilic_suf_org(3), icur_sf
        integer(kind = kint) :: i, isf_tgt
        real(kind = kreal) :: lic_v, n_v, k_area
        integer(kind = kint) :: iflag_found_sf, iele, isf_org
        Integer(kind = kint) :: iflag_debug


        iflag_comm = 1
        iflag_debug = 0
        k_area = 0.0

        !   initial convolution integration at origin point
        lic_v = 0.0
        o_tgt = 0.0
        n_grad(1:3) = 0.0
        icur_sf = isurf
        n_v = 0.0

        do i = 1, 2
          iele = isurf_orgs(i,1)
          isf_org = isurf_orgs(i,2)
          if(iflag_debug .eq. 1) write(50+my_rank,*)                    &
     &              "ele: ", iele, "local surf: ", isf_org
          if(iflag_debug .eq. 1) write(50+my_rank,*)                    &
     &              "global surf: ", isurf, "surf of ele",              &
     &               isf_4_ele(iele, isf_org)
          if(iele .le. izero .or. iele .gt. nelem) then
            if(iflag_debug .eq. 1) write(50+my_rank,*)                  &
     &              "invalid element, end----------------------"
            iflag_comm = -5
            return
          end if
        end do

        if(mask_flag(lic_p, r_org)) then
          call noise_sampling                                           &
     &       (lic_p%noise_size, lic_p%freq_noise, lic_p%noise_data,     &
     &       xx_org, xyz_min, xyz_max, n_v)
          call noise_grad_sampling                                      &
     &      (lic_p%noise_size, lic_p%freq_noise, lic_p%noise_grad_data, &
     &       xx_org, xyz_min, xyz_max, n_grad)
        end if
        o_tgt = o_tgt + n_v * kernal_node(kernal_size/2.0)
        n_grad = n_grad + n_grad * kernal_node(kernal_size/2.0)

        if(iflag_debug .eq. 1) write(50+my_rank,*)                      &
     &     "--------------------Forward iter begin----------------"
!   forward integration
        iflag_back = 1
        step_vec(1:3) = vec_org(1:3)
        new_pos(1:3) = xx_org(1:3)
! if current surface is exterior surface, then return.
!        if((interior_surf(icur_sf) .eq. izero) .or. (icur_sf .eq. izero)) then
        if(icur_sf .eq. izero) then
          if(iflag_debug .eq. 1) write(50+my_rank,*)                    &
     &       "extorior surface, end-------------------------", icur_sf
          iflag_comm = -1
          return
        end if

        iflag_found_sf = 0

        do i = 1, 2
          iele = isurf_orgs(i,1)
          isf_org = isurf_orgs(i,2)
          call find_line_end_in_1ele(iflag_back, nnod, nelem, nsurf,    &
     &        nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,       &
     &        vec_org, xx_org, isf_tgt, new_pos, xi)
          if(isf_tgt .gt. 0) then
            !write(50+my_rank, *) "find exit point in neighbor element."
            iflag_found_sf = 1
            ilic_suf_org(1:2) = isurf_orgs(i,1:2)
            exit
          end if
        end do

        if(iflag_found_sf .eq. 0) then
          if(iflag_debug .eq. 1) write(50+my_rank, *)                   &
     &      "not find exit point in neighbor element. end------------"
          iflag_comm = -2
        else
          new_pos(1:3) = xx_org(1:3)
          if(iflag_debug .eq. 1) write(50+my_rank, *)                   &
     &                          "start cal lic, ele and surf: ",        &
     &                          ilic_suf_org(1), ilic_suf_org(2)
          call s_cal_lic_from_point(nnod, nelem, nsurf,                 &
     &        nnod_4_surf, xx, ie_surf, isf_4_ele,                      &
     &        iele_4_surf, interior_surf, lic_p,                        &
     &        iflag_back, xyz_min, xyz_max,                             &
     &        v_nod, ilic_suf_org, new_pos, step_vec,                   &
     &        kernal_size, kernal_node, ref_nod,                        &
     &        lic_v, n_grad, k_area, iflag_comm)
          o_tgt = o_tgt + lic_v
        end if
        if(iflag_debug .eq. 1) write(50+my_rank,*) "-----------------------Forward iter end-------------with:", iflag_comm

        if(iflag_debug .eq. 1) write(50+my_rank,*) "-----------------------Backward iter begin--------------------"
        !   Backward iteration
        iflag_found_sf = 0
        iflag_back = -1
        step_vec(1:3) = vec_org(1:3)
        new_pos(1:3) = xx_org(1:3)
        do i = 1, 2
          iele = isurf_orgs(i,1)
          isf_org = isurf_orgs(i,2)
          call find_line_end_in_1ele(iflag_back, nnod, nelem, nsurf,    &
     &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,         &
     &      vec_org, xx_org, isf_tgt, new_pos, xi)
          if(isf_tgt .gt. 0) then
            !write(50+my_rank, *) "find exit point in neighbor element."
            iflag_found_sf = 1
            ilic_suf_org(1:2) = isurf_orgs(i,1:2)
            exit
          end if
        end do

        if(iflag_found_sf .eq. 0) then
          if(iflag_debug .eq. 1) write(50+my_rank, *) "not find exit point in neighbor element. end-----------------"
          iflag_comm = -2
        else
          new_pos(1:3) = xx_org(1:3)
          if(iflag_debug .eq. 1) write(50+my_rank, *) "start cal lic, ele and surf: ", ilic_suf_org(1), ilic_suf_org(2)
          call s_cal_lic_from_point(nnod, nelem, nsurf,                 &
          &          nnod_4_surf, xx, ie_surf, isf_4_ele,               &
          &          iele_4_surf, interior_surf, lic_p,                 &
          &          iflag_back, xyz_min, xyz_max,                      &
          &          v_nod, ilic_suf_org, new_pos, step_vec,            &
          &          kernal_size, kernal_node, ref_nod,                 &
          &          lic_v, n_grad, k_area, iflag_comm)
          o_tgt = o_tgt + lic_v
        end if
        if(iflag_debug .eq. 1) write(50+my_rank,*) "-----------------------Backward iter end------------with:", iflag_comm

        if(k_area .gt. 0.0) then
          o_tgt = o_tgt / k_area
        end if
        o_tgt = o_tgt * lic_p%factor_normal

        if(iflag_debug .eq. 1) write(50+my_rank,*) "Get lic value: ", o_tgt
        if(iflag_debug .eq. 1) write(50+my_rank, *)"   "
    end subroutine cal_lic_on_surf_vector

!
!  ---------------------------------------------------------------------
!
    subroutine s_cal_lic_from_point(numnod, numele, numsurf,           &
    &          nnod_4_surf, xx, ie_surf, isf_4_ele,                    &
    &          iele_4_surf, interior_surf, lic_p,                      &
    &          iflag_back, xyz_min, xyz_max,                           &
    &          vect_nod, isurf_org, x_start, v_start,                  &
    &          k_size, k_node, ref_nod,                                &
    &          lic_v, grad_v, k_area, iflag_comm)

      use t_noise_node_data

    !
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      real(kind = kreal), intent(in) :: xx(numnod,3)
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      integer(kind = kint), intent(in) :: interior_surf(numsurf)
    !
      integer(kind = kint), intent(in) :: iflag_back
      real(kind = kreal), intent(in) :: vect_nod(numnod,3)
      type(lic_parameters), intent(in) :: lic_p
      real(kind = kreal), intent(in) :: ref_nod(numnod,lic_p%num_masking)
    !
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: iflag_comm
      real(kind = kreal), intent(inout) ::   v_start(3), x_start(3)
    !
      integer(kind = kint), intent(in) :: k_size
      real(kind = kreal), intent(in) :: k_node(k_size)
    !type(noise_node), intent(in) :: n_node(n_size)
    !type(noise_mask), intent(inout) :: n_mask
    !
      real(kind = kreal), intent(in) :: xyz_min(3)
      real(kind = kreal), intent(in) :: xyz_max(3)
    !
      real(kind = kreal), intent(inout) :: lic_v, k_area, grad_v(3)
    !
      integer(kind = kint) :: isf_tgt, isurf_end, isurf_start, iele, isf_org
      real(kind = kreal) :: x_org(3), x_tgt(3), v_tgt(3), xi(2)
      integer(kind = kint) :: i_iter, i_k, i_n, iflag_debug, i
      real(kind = kreal) :: n_v, nv_sum, step_len, len_sum, k_value, k_pos, avg_stepsize
      real(kind = kreal) :: g_v(3), ref_value(lic_p%num_masking)
    !
    !
    !init local variables
      i_iter = 0
      i_k = k_size / 2.0 ! index of kernel value
      i_n = 0 ! index of noise value
      n_v = 0.0 ! noise value
      g_v(1:3) = 0.0
      nv_sum = 0.0
      lic_v = 0.0
      iflag_debug = 0
      step_len = 0.0
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
        step_len = 0.0
        x_org(1:3) = x_start(1:3)
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
            call find_line_end_in_1ele(iflag_back, numnod, numele, numsurf,         &
            &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,              &
            &      v_start, x_start, isf_tgt, x_tgt, xi)
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
        if(iflag_debug .eq. 1) write(50 + my_rank, *) "From", isurf_start, "at elem", iele, "local", isf_org
        if(iflag_debug .eq. 1) write(50 + my_rank, *) "pos:", x_start
      !
        isurf_end = abs(isf_4_ele(iele,isf_tgt))
        call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,     &
        &      ie_surf, isurf_end, xi, vect_nod, v_tgt)
      !
        isf_org =  0
        x_start(1:3) = half * (x_start(1:3) + x_tgt(1:3))
        v_start(1:3) = half * (v_start(1:3) + v_tgt(1:3))
        if(iflag_debug .eq. 1) write(50 + my_rank, *) "first hit pos:", x_tgt
        if(iflag_debug .eq. 1) write(50 + my_rank, *) "middle pos:", x_start
      !
      !   extend to surface of element
      !
        call find_line_end_in_1ele(iflag_back, numnod, numele, numsurf, &
     &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,         &
     &      v_start, x_start, isf_tgt, x_tgt, xi)
      !
        if(isf_tgt .eq. 0) then
          iflag_comm = -12
          exit
        end if
      ! exit point after 2nd field line trace
        isurf_end = abs(isf_4_ele(iele,isf_tgt))
        call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,     &
       &      ie_surf, isurf_end, xi, vect_nod, v_start)
        step_len = sqrt( (x_tgt(1) - x_org(1))**2                       &
       &               + (x_tgt(2) - x_org(2))**2                       &
       &               + (x_tgt(3) - x_org(3))**2)
        if(iflag_debug .eq. 1) write(50 + my_rank, *) "To  ", isurf_end, "at elem", iele, "local", isf_tgt
        if(iflag_debug .eq. 1) write(50 + my_rank, *) "pos:", x_tgt
        x_start(1:3) =  x_tgt(1:3)
      !call cal_pos_idx_volume(n_size, x_tgt, xyz_min, xyz_max, i_n)
        n_v = 0.0
        g_v(1:3) = 0.0
        ref_value(:) = 0.0
        do i = 1, lic_p%num_masking
          call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,     &
          &      ie_surf, isurf_end, xi, ref_nod(1,i), ref_value(i))
        end do
        if(mask_flag(lic_p, ref_value)) then
          call noise_sampling(lic_p%noise_size, lic_p%freq_noise, lic_p%noise_data,               &
          &     x_tgt, xyz_min, xyz_max, n_v)
          call noise_grad_sampling(lic_p%noise_size, lic_p%freq_noise, lic_p%noise_grad_data,     &
          &     x_tgt, xyz_min, xyz_max, g_v)
        end if
        nv_sum = nv_sum + n_v
        len_sum = len_sum + step_len
        len_sum = min(len_sum, lic_p%trace_length)
        k_pos = 0.0
        if(iflag_back .eq. ione) then
          k_pos =  0.5 + 0.5 * len_sum/(lic_p%trace_length)
        else
          k_pos =  0.5 - 0.5 * len_sum/(lic_p%trace_length)
        end if
        k_value = 0.0
        call kernal_sampling(k_size, k_node, k_pos, k_value)
        if(iflag_debug .eq. 1) write(50 + my_rank, *) "--step: ",i_iter, step_len, len_sum, "k_pos:", k_pos, "k_v: ", k_value
        !lic_v = lic_v + n_v * k_node(i_k)
        lic_v = lic_v + n_v * k_value
        grad_v = grad_v + g_v * k_value
        k_area = k_area + k_value
        if(iflag_debug .eq. 1) write(50 + my_rank, *) "nv: ", n_v, "nv sum:", nv_sum, "kernel area: ", k_area, "lic_v: ", lic_v

!        if(interior_surf(isurf_end) .eq. izero) then
!          isurf_start = isurf_end
!          iflag_comm = 10
!          exit
!        else
!          isurf_start = isurf_end
!        end if
!       will use exterior surface(surface in ghost layer ) to cal lic
        isurf_start = isurf_end

        if(flag_lic_end(lic_p, len_sum, i_iter) .eq. ione) then
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

!      if(flag_lic_end(lic_p, len_sum, i_iter) .eq. izero) then
      if(flag_lic_end(lic_p, len_sum, i_iter) .eq. 2) then ! never enter this loop
        avg_stepsize = len_sum / i_iter
        if (avg_stepsize .lt. 0.005) then
          avg_stepsize = 0.005
        end if
        if(iflag_debug .eq. 1) write(50 + my_rank, *) "----dis is short for", i_iter, "iteration"
        do
          if(i_iter .gt. 200) then
            !write(*,*) 'iteration too large in 2: ', i_iter
            !write(*,*) 'total length: ', len_sum, 'kernel', k_value, 'step', step_len
            return
          end if
          i_iter = i_iter + 1
          len_sum = len_sum + avg_stepsize
          len_sum = min(len_sum, lic_p%trace_length)
          k_pos = 0.0
          x_tgt = x_start + avg_stepsize * v_start                      &
     &                     / sqrt(v_start(1)*v_start(1)                 &
     &                          + v_start(2)*v_start(2)                 &
     &                          + v_start(3)*v_start(3))
          n_v = 0.0
          g_v(1:3) = 0.0
          do i = 1, lic_p%num_masking
            call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,     &
            &      ie_surf, isurf_end, xi, ref_nod(1,i), ref_value(i))
          end do
          if(mask_flag(lic_p, ref_value)) then
            call noise_sampling(lic_p%noise_size, lic_p%freq_noise, lic_p%noise_data,               &
            &     x_tgt, xyz_min, xyz_max, n_v)
            call noise_grad_sampling(lic_p%noise_size, lic_p%freq_noise, lic_p%noise_grad_data,     &
            &     x_tgt, xyz_min, xyz_max, g_v)
          end if
          !call noise_nd_sampling(n_size, n_node, x_tgt, xyz_min, xyz_max, n_v)
          if(iflag_back .eq. ione) then
            k_pos =  0.5 + 0.5 * len_sum/(lic_p%trace_length)
          else
            k_pos =  0.5 - 0.5 * len_sum/(lic_p%trace_length)
          end if
          k_value = 0.0
          call kernal_sampling(k_size, k_node, k_pos, k_value)
          if(iflag_debug .eq. 1) write(50 + my_rank, *) "--step: ",i_iter, step_len, len_sum, "k_pos:", k_pos, "k_v: ", k_value
          lic_v = lic_v + n_v*k_value
          grad_v = grad_v + g_v * k_value
          k_area = k_area + k_value
          x_start = x_tgt
          if(iflag_debug .eq. 1) write(50 + my_rank, *) "nv: ", n_v, "nv sum:", nv_sum, "kernel area: ", k_area, "lic_v: ", lic_v
          if(flag_lic_end(lic_p, len_sum, i_iter) .eq. ione) then
            iflag_comm = 1
            exit
          end if
        end do
      end if

      if(iflag_debug .eq. 1) write(50 + my_rank, *) "---------","lic_v: ", lic_v
      if(iflag_debug .eq. 1) write(50 + my_rank, *) "len_sum", len_sum
    !
    end subroutine s_cal_lic_from_point
!
!  ---------------------------------------------------------------------
!

    integer(kind = kint) function flag_lic_end(lic_p, cur_len, cur_cnt)
!
    type(lic_parameters), intent(in) :: lic_p
    integer(kind = kint), intent(in) :: cur_cnt
    real(kind = kreal), intent(in) :: cur_len
!
    if(lic_p%iflag_trace_length_type .eq. iflag_by_lengh) then
      if(cur_len .ge. lic_p%trace_length) then
        flag_lic_end = ione
      else
        flag_lic_end = izero
      end if
    else
      if(cur_cnt .gt. lic_p%trace_count) then
        flag_lic_end = ione
      else
        flag_lic_end = izero
      end if
    end if

    end function flag_lic_end
!
!  ---------------------------------------------------------------------
!
      end module cal_lic_on_surf_viz
