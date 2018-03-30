!cal_lic_on_surf_viz.f90
!
!      module cal_lic_on_surf_viz
!
!      Written by Yangguang Liao 2018
!
!      subroutine element_ave_4_viz(nnod, nele, ie, v_nod, s_nod,       &
!     &          iele, v_ave, s_ave)
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
      use lic_noise_generator
      use lic_kernel_generator
      use cal_field_on_surf_viz
      use cal_fline_in_cube
!
      implicit  none
!
      real(kind = kreal), parameter :: factor_lic_magnify = 120.0d0
      private :: factor_lic_magnify
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
      subroutine cal_lic_on_surf_vector(nnod, nsurf, nelem, nnod_4_surf,        &
     &          isf_4_ele, iele_4_surf, interior_surf, xx,                      &
     &          vnorm_surf, isurf_orgs, ie_surf, xi,                            &
     &          noise_size, noise_nod, noise_grad, kernal_size, kernal_node,    &
     &          v_nod, xx_org, isurf, xyz_min, xyz_max, iflag_comm,     &
     &          o_tgt, n_grad)

        use m_geometry_constants
        use calypso_mpi
        use t_noise_node_data

        integer(kind = kint), intent(in) :: isurf_orgs(2,3)
        integer(kind = kint), intent(in) :: nnod, nsurf, nelem, nnod_4_surf, isurf
        integer(kind = kint), intent(in) :: isf_4_ele(nelem, nsurf_4_ele)
        integer(kind = kint), intent(in) :: iele_4_surf(nsurf, 2, 2)
        integer(kind = kint), intent(in) :: interior_surf(nsurf)
        real(kind = kreal), intent(in) :: vnorm_surf(nsurf, 3)
        integer(kind = kint), intent(in) :: ie_surf(nsurf,nnod_4_surf)

        real(kind = kreal), intent(inout) :: xi(2)
        real(kind = kreal), intent(in) :: v_nod(nnod,3), xx(nnod, 3)
        real(kind = kreal), intent(in) :: xx_org(3)
        real(kind = kreal), intent(inout) :: o_tgt, n_grad(3)
        integer(kind = kint), intent(inout) :: iflag_comm
        integer(kind = kint), intent(in) :: noise_size, kernal_size
        real(kind = kreal), intent(in) :: kernal_node(kernal_size)
        character(kind = 1), intent(in):: noise_grad(noise_size*3)
        character(kind = 1), intent(in):: noise_nod(noise_size)
        !type(noise_node), intent(in) :: noise_nod(noise_size)

        real(kind = kreal), intent(in) :: xyz_min(3)
        real(kind = kreal), intent(in) :: xyz_max(3)

        integer(kind = kint) :: nforward_step, nbackward_stap, iflag_back, istep

        real(kind = kreal) :: dir
        real(kind = kreal) :: org_vec(3), step_vec(3), new_pos(3), old_pos(3)
        integer(kind = kint) :: ilic_suf_org(3), icur_sf
        integer(kind = kint) :: pos_idx, kernal_idx, i, isf_tgt, iele_sf_org(2,2)
        real(kind = kreal) :: lic_v, flux, n_v, forward_len, backward_len, k_area
        integer(kind = kint) :: iflag_found_sf, iele, isf_org, iflag_debug


        iflag_comm = 1
        iflag_debug = 0
        nforward_step = 32
        nbackward_stap = 32
        forward_len = 0.32
        backward_len = 0.32
        k_area = 0.0

        !   initial convolution integration at origin point
        lic_v = 0.0
        o_tgt = 0.0
        n_grad(1:3) = 0.0
        icur_sf = isurf

        !call cal_pos_idx_volume(noise_size, xx_org, xyz_min, xyz_max, pos_idx)
        !o_tgt = o_tgt + ichar(noise_nod(pos_idx)) / 255.0 * kernal_node(kernal_size/2)
        !o_tgt = o_tgt + get_noise_value(noise_size, noise_nod, pos_idx) * kernal_node(kernal_size/2)
        n_v = 0.0
        call noise_sampling(noise_size, noise_nod, xx_org, xyz_min, xyz_max, n_v)
        !call noise_nd_sampling(noise_size, noise_nod, xx_org, xyz_min, xyz_max, n_v)
        o_tgt = o_tgt + n_v * kernal_node(kernal_size/2)
        call noise_grad_sampling(noise_size, noise_grad, xx_org, xyz_min, xyz_max, n_grad)
        n_grad = n_grad + n_grad * kernal_node(kernal_size/2)
        !if(iflag_debug .eq. 1) write(50+my_rank,*) "xx", xx_org, "min", xyz_min, "max", xyz_max
        !if(iflag_debug .eq. 1) write(50+my_rank,*) "n_size",noise_size, "nid", pos_idx, "n_v", o_tgt
        call cal_field_on_surf_vector(nnod, nsurf, nnod_4_surf, ie_surf, icur_sf, xi, v_nod, org_vec)


        do i = 1, 2
          iele = isurf_orgs(i,1)
          isf_org = isurf_orgs(i,2)
          if(iflag_debug .eq. 1) write(50+my_rank,*) "ele: ", iele, "local surf: ", isf_org
          if(iflag_debug .eq. 1) write(50+my_rank,*) "global surf: ", isurf, "surf of ele", isf_4_ele(iele, isf_org)
          if(iele .le. izero .or. iele .gt. nsurf) then
            if(iflag_debug .eq. 1) write(50+my_rank,*) "invalid element, end----------------------"
            iflag_comm = -5
            return
          end if
        end do


        !write(50+my_rank,*) "isurf: ", isurf
        !write(50+my_rank,*) "isurf's element: ", iele_4_surf(isurf,1,1), iele_4_surf(isurf,2,1)
        !write(50+my_rank,*) "isurf_org old: ", isurf_orgs(1,1:2), "isurf_org new: ", isurf_orgs(2,1:2)



        !write(50+my_rank,*) "cal lic at: ", xx_org, "with v: ", org_vec
        if(iflag_debug .eq. 1) write(50+my_rank,*) "------------------------Forward iter begin--------------------"
        !   forward integration
        iflag_back = 1
        step_vec(1:3) = org_vec(1:3)
        new_pos(1:3) = xx_org(1:3)
        ! if current surface is exterior surface, then return.
        if((interior_surf(icur_sf) .eq. izero) .or. (icur_sf .eq. izero)) then
          if(iflag_debug .eq. 1) write(50+my_rank,*) "extorior surface, end----------------------------------------", icur_sf
          iflag_comm = -1
          return
        end if

        iflag_found_sf = 0

        do i = 1, 2
          iele = isurf_orgs(i,1)
          isf_org = isurf_orgs(i,2)
          call find_line_end_in_1ele(iflag_back, nnod, nelem, nsurf,         &
          &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,         &
          &      org_vec, xx_org, isf_tgt, new_pos, xi)
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
          call s_cal_lic_from_point(nnod, nelem, nsurf,                      &
          &          nnod_4_surf, xx, ie_surf, isf_4_ele,                    &
          &          iele_4_surf, interior_surf, vnorm_surf,                 &
          &          forward_len, iflag_back, xyz_min, xyz_max,              &
          &          v_nod, ilic_suf_org, new_pos, step_vec,                 &
          &          kernal_size, kernal_node, noise_size, noise_nod,        &
          &          noise_grad, lic_v, n_grad, k_area, iflag_comm)
          o_tgt = o_tgt + lic_v
        end if
        if(iflag_debug .eq. 1) write(50+my_rank,*) "-----------------------Forward iter end-------------with:", iflag_comm

        if(iflag_debug .eq. 1) write(50+my_rank,*) "-----------------------Backward iter begin--------------------"
        !   Backward iteration
        iflag_found_sf = 0
        iflag_back = -1
        step_vec(1:3) = org_vec(1:3)
        new_pos(1:3) = xx_org(1:3)
        do i = 1, 2
          iele = isurf_orgs(i,1)
          isf_org = isurf_orgs(i,2)
          call find_line_end_in_1ele(iflag_back, nnod, nelem, nsurf,         &
          &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,         &
          &      org_vec, xx_org, isf_tgt, new_pos, xi)
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
          call s_cal_lic_from_point(nnod, nelem, nsurf,                      &
          &          nnod_4_surf, xx, ie_surf, isf_4_ele,                    &
          &          iele_4_surf, interior_surf, vnorm_surf,                 &
          &          backward_len, iflag_back, xyz_min, xyz_max,             &
          &          v_nod, ilic_suf_org, new_pos, step_vec,                 &
          &          kernal_size, kernal_node, noise_size, noise_nod,        &
          &          noise_grad, lic_v, n_grad, k_area, iflag_comm)
          o_tgt = o_tgt + lic_v
        end if
        if(iflag_debug .eq. 1) write(50+my_rank,*) "-----------------------Backward iter end------------with:", iflag_comm

        if(k_area .gt. 0.0) then
          o_tgt = o_tgt / k_area
        end if
        o_tgt = o_tgt * factor_lic_magnify

        !write(50+my_rank, *) iflag_comm, o_tgt
        if(iflag_debug .eq. 1) write(50+my_rank,*) "Get lic value: ", o_tgt
        if(iflag_debug .eq. 1) write(50+my_rank, *)"   "
    end subroutine cal_lic_on_surf_vector
!
!  ---------------------------------------------------------------------
!

      subroutine s_trace_lic_element( numnod, numele, numsurf,           &
     &          nnod_4_surf, xx, ie_surf, isf_4_ele, iele_4_surf,        &
     &          interior_surf, vnorm_surf, iflag_back, vect_nod,         &
     &          n_size, n_data, isurf_org, isurf, x_start, v_start,      &
     &          noise_v, xyz_min, xyz_max, iflag_comm)

    !
    integer(kind = kint), intent(in) :: numnod, numele, numsurf
    integer(kind = kint), intent(in) :: nnod_4_surf
    real(kind = kreal), intent(in) :: xx(numnod,3)
    integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
    integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
    integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
    integer(kind = kint), intent(in) :: interior_surf(numsurf)
    real(kind = kreal), intent(in) :: vnorm_surf(numsurf,3)
    !
    real(kind = kreal), intent(in) :: xyz_min(3)
    real(kind = kreal), intent(in) :: xyz_max(3)
    !
    integer(kind = kint), intent(in) :: iflag_back, n_size
    real(kind = kreal), intent(in) :: vect_nod(numnod,3)
    character(kind = 1), intent(in) :: n_data(n_size)
    !
    integer(kind = kint), intent(inout) :: isurf_org(3), isurf
    integer(kind = kint), intent(inout) :: iflag_comm
    real(kind = kreal), intent(inout) ::   v_start(3), x_start(3), noise_v
    !
    integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org, n_idx, i
    real(kind = kreal) :: x_tgt(3), v_tgt(3), c_tgt(1), xi(2), flux
    !
    !initialize input and local variables

    noise_v = 0.0
    iflag_comm = 1

    if(isurf .eq. 0) then
      iflag_comm = 0
      return
    end if

    ! current surface will have two neigbor element, now choose one first
    ! current element
    iele =    iele_4_surf(isurf,1,1)
    ! current local surface id
    isf_org = iele_4_surf(isurf,1,2)
write(50+my_rank,*) "point start: ", x_start
!write(*,*) "param: ", iele, isf_org, v_start, x_start
!
    call find_line_end_in_1ele(iflag_back, numnod, numele, numsurf,     &
     &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,         &
     &      v_start, x_start, isf_tgt, x_tgt, xi)
    !
write(50+my_rank,*) "ray: ", v_start
write(50+my_rank,*) "hit at: ", x_tgt
    ! if there is no hit point in current element, we will choose another element
    if(isf_tgt .eq. 0) then
      ! if it is exterior surface, there is only one element
      if(interior_surf(isurf) .eq. izero) then
        iflag_comm = -1
        return
      end if
      ! current element
      iele =    iele_4_surf(isurf,2,1)
      ! current local surface id
      isf_org = iele_4_surf(isurf,2,2)
      call find_line_end_in_1ele(iflag_back, numnod, numele, numsurf,    &
      &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,         &
      &      v_start, x_start, isf_tgt, x_tgt, xi)
      ! still no exit surface, then return
      if(isf_tgt .eq. 0) then
        iflag_comm = -1
        return
      end if
    end if
    !
    isurf_end = abs(isf_4_ele(iele,isf_tgt))
    call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,         &
     &      ie_surf, isurf_end, xi, vect_nod, v_tgt)
    !   get middle point which is within current element
    !isf_org =  0
    x_start(1:3) = half * (x_start(1:3) + x_tgt(1:3))
    v_start(1:3) = half * (v_start(1:3) + v_tgt(1:3))
write(50+my_rank,*) "mid point: ", x_start
write(50+my_rank,*) "with v: ", v_start
    !c_field(1) =   half * (c_field(1) + c_tgt(1))
    !
    !   extend to surface of element
    !
    call find_line_end_in_1ele(iflag_back, numnod, numele, numsurf,     &
     &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,         &
     &      v_start, x_start, isf_tgt, x_tgt, xi)
    !
    if(isf_tgt .eq. 0) then
      iflag_comm = -11
      return
    end if
    !
    isurf_end = abs(isf_4_ele(iele,isf_tgt))
    call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,         &
     &      ie_surf, isurf_end, xi, vect_nod, v_start)
    call cal_field_on_surf_vector(numnod, numsurf, nnod_4_surf,         &
    &      ie_surf, isurf_end, xi, xx, x_start)
write(50+my_rank,*) "secd exit at: ", x_tgt, "new x_start:", x_start
!if(my_rank .eq. 0) write(*,*) "secd exit at: ", x_tgt
write(50+my_rank,*) "with v: ", v_start
    !   get noise(c_field) value from noise data set according to current surface and xi
    call cal_pos_idx_volume(n_size, x_tgt, xyz_min, xyz_max, n_idx)
    noise_v = ichar(n_data(n_idx)) / 1.0

    ! if next point is on exterior surface
    if(interior_surf(isurf_end) .eq. izero) then
      isurf_org(1) = iele
      isurf_org(2) = isf_tgt
      isurf_org(3) = ie_surf(isurf_end,1)
      iflag_comm = 10
      return
    end if
    !update current surface id for next iteration
    isurf = isurf_end

    if(isurf_org(1).eq.0 ) then
      iflag_comm = -10
      return
    end if

    end subroutine s_trace_lic_element
!
!  ---------------------------------------------------------------------
!
subroutine s_cal_lic_from_point(numnod, numele, numsurf,           &
&          nnod_4_surf, xx, ie_surf, isf_4_ele,                    &
&          iele_4_surf, interior_surf, vnorm_surf,                 &
&          max_line_len, iflag_back, xyz_min, xyz_max,             &
&          vect_nod, isurf_org, x_start, v_start,                  &
&          k_size, k_node, n_size, n_node, grad_node,              &
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
real(kind = kreal), intent(in) :: vnorm_surf(numsurf,3)
!
integer(kind = kint), intent(in) :: iflag_back
real(kind = kreal), intent(in) :: vect_nod(numnod,3), max_line_len
!
integer(kind = kint), intent(inout) :: isurf_org(3)
integer(kind = kint), intent(inout) :: iflag_comm
real(kind = kreal), intent(inout) ::   v_start(3), x_start(3)
!
integer(kind = kint), intent(in) :: n_size, k_size
real(kind = kreal), intent(in) :: k_node(k_size)
character(kind = 1), intent(in):: grad_node(n_size*3)
character(kind = 1), intent(in):: n_node(n_size)
!type(noise_node), intent(in) :: n_node(n_size)
!
real(kind = kreal), intent(in) :: xyz_min(3)
real(kind = kreal), intent(in) :: xyz_max(3)
!
real(kind = kreal), intent(inout) :: lic_v, k_area, grad_v(3)
!
integer(kind = kint) :: isf_tgt, isurf_end, isurf_start, iele, isf_org
real(kind = kreal) :: x_org(3), x_tgt(3), v_tgt(3), c_tgt(1), xi(2), flux
integer(kind = kint) :: i_iter, i_k, i_n, iflag_debug, i
real(kind = kreal) :: n_v, nv_sum, step_len, len_sum, k_value, k_pos, avg_stepsize
real(kind = kreal) :: g_v(3)
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
len_sum = 0.0
avg_stepsize = 0.01

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
    exit
  end if

  if(interior_surf(isurf_start) .eq. izero) then
    iflag_comm = 10
    return
  end if
  isf_tgt = 0

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
  step_len = NORM2(x_tgt(1:3) - x_org(1:3))
if(iflag_debug .eq. 1) write(50 + my_rank, *) "To  ", isurf_end, "at elem", iele, "local", isf_tgt
if(iflag_debug .eq. 1) write(50 + my_rank, *) "pos:", x_tgt
  x_start(1:3) =  x_tgt(1:3)
  !call cal_pos_idx_volume(n_size, x_tgt, xyz_min, xyz_max, i_n)
  n_v = 0.0
  call noise_sampling(n_size, n_node, x_tgt, xyz_min, xyz_max, n_v)
  !call noise_nd_sampling(n_size, n_node, x_tgt, xyz_min, xyz_max, n_v)
  g_v(1:3) = 0.0
  call noise_grad_sampling(n_size, grad_node, x_tgt, xyz_min, xyz_max, g_v)
  nv_sum = nv_sum + n_v
  len_sum = len_sum + step_len
  len_sum = min(len_sum, max_line_len)
  k_pos = 0.0
  if(iflag_back .eq. ione) then
    k_pos =  0.5 + 0.5 * len_sum/max_line_len
  else
    k_pos =  0.5 - 0.5 * len_sum/max_line_len
  end if
k_value = 0.0
call kernal_sampling(k_size, k_node, k_pos, k_value)
if(iflag_debug .eq. 1) write(50 + my_rank, *) "--step: ",i_iter, step_len, len_sum, "k_pos:", k_pos, "k_v: ", k_value
  !lic_v = lic_v + n_v * k_node(i_k)
  lic_v = lic_v + n_v * k_value
  grad_v = grad_v + g_v * k_value
  k_area = k_area + k_value
if(iflag_debug .eq. 1) write(50 + my_rank, *) "nv: ", n_v, "nv sum:", nv_sum, "kernel area: ", k_area, "lic_v: ", lic_v

  if(interior_surf(isurf_end) .eq. izero) then
    isurf_start = isurf_end
    iflag_comm = 10
    exit
  else
    isurf_start = isurf_end
  end if

  if(len_sum .ge.max_line_len) then
    iflag_comm = 1
    exit
  end if
end do

if(len_sum .gt. max_line_len) then
  avg_stepsize = len_sum / i_iter
  if (avg_stepsize .lt. 0.005) then
    avg_stepsize = 0.005
  end if
  if(iflag_debug .eq. 1) write(50 + my_rank, *) "----dis is short for", i_iter, "iteration"
  do
    i_iter = i_iter + 1
    len_sum = len_sum + avg_stepsize
    len_sum = min(len_sum, max_line_len)
    k_pos = 0.0
    x_tgt = x_start + v_start / norm2(v_start) * avg_stepsize
    n_v = 0.0
    call noise_sampling(n_size, n_node, x_tgt, xyz_min, xyz_max, n_v)
    !call noise_nd_sampling(n_size, n_node, x_tgt, xyz_min, xyz_max, n_v)
    if(iflag_back .eq. ione) then
      k_pos =  0.5 + 0.5 * len_sum/max_line_len
    else
      k_pos =  0.5 - 0.5 * len_sum/max_line_len
    end if
    k_value = 0.0
    call kernal_sampling(k_size, k_node, k_pos, k_value)
if(iflag_debug .eq. 1) write(50 + my_rank, *) "--step: ",i_iter, step_len, len_sum, "k_pos:", k_pos, "k_v: ", k_value
    lic_v = lic_v + n_v*k_value
    k_area = k_area + k_value
    x_start = x_tgt
if(iflag_debug .eq. 1) write(50 + my_rank, *) "nv: ", n_v, "nv sum:", nv_sum, "kernel area: ", k_area, "lic_v: ", lic_v
    if(len_sum .ge.max_line_len) then
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
      end module cal_lic_on_surf_viz
