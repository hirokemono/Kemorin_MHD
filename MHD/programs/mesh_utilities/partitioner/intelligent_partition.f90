!>@file  intelligent_partition.f90
!!       intelligent_partition
!!
!!@author Yangguang Liao
!!@date   Programmed 2018
!
!> @brief According to vector field feature to partition data set to make a better load balance
!!
!!@verbatim
!!      function field_istack_nod_buffer(nprocs, istack_nod)
!!@endverbatim
!
module intelligent_partition
!
use m_precision
use m_constants
use m_geometry_constants
use t_geometry_data
use t_surface_data
use m_domain_group_4_partition
use m_ctl_param_partitioner
!
use cal_fline_in_cube
use cal_field_on_surf_viz
!
implicit none
!
!> Structure of FEM vector field data used for partition
type vector_field
!>        number of nodes for field data
  integer(kind = kint_gl) :: nnod
!>        number of component in this field data
  integer(kind = kint_gl) :: ncomp
!>        field name
  character (len=kchara) :: phys_name
!>        field data for partition
  real (kind=kreal), pointer :: d_ucd(:,:)
end type vector_field
!
type simulate_particle
  real(kind = kreal) :: pos(3)
  real(kind = kreal) :: vec(3)
  real(kind = kreal) :: line_len
  integer(kind = kint) :: ele_id
  integer(kind = kint) :: group_id
end type simulate_particle
!
type subdomain_load
  integer(kind = kint_gl) :: id_sub
  type(simulate_particle) :: sample_particles(10)
  real(kind = kreal) :: average_load
end type subdomain_load
!
type ratio_ptr
  real(kind = kreal), pointer::ratio(:)
end type ratio_ptr
!
type dimension_part_tbl
  type(ratio_ptr), dimension(3)::part_dim(3)
end type dimension_part_tbl
!
type time_esti
  real(kind = kreal) :: total_time
  real(kind = kreal) :: ave_time
  integer(kind = kint_gl) :: cnt
end type time_esti
!
! -------------------------------------------------------------------
!
contains
!
! -------------------------------------------------------------------
!
subroutine allocate_dim_part_tbl(part_dim_tbl, ndivide_eb)
!
  type(dimension_part_tbl), intent(inout) :: part_dim_tbl
  integer(kind=kint), intent(in) :: ndivide_eb(3)
!
  integer(kind=kint) :: i
!
  do i = 1, 3
    allocate(part_dim_tbl%part_dim(i)%ratio(ndivide_eb(i)))
  end do
end subroutine allocate_dim_part_tbl
!
! -------------------------------------------------------------------
!
subroutine simulate_field_line_integral(nnod, nele, nsurf,                &
&           nnod_4_surf, isf_4_ele, iele_4_surf, ie_surf, interior_surf,  &
&           iflag_dir, xx, field_vec, isurf_org, group_id,                &
&           x_start, v_start, line_len, itr_num, iflag_comm)
!

!
  integer(kind = kint), intent(in) :: nnod, nele, nsurf
  integer(kind = kint), intent(in) :: nnod_4_surf, group_id
  integer(kind = kint), intent(in) :: ie_surf(nsurf,nnod_4_surf)
  integer(kind = kint), intent(in) :: isf_4_ele(nele,nsurf_4_ele)
  integer(kind = kint), intent(in) :: iele_4_surf(nsurf, 2, 2)
  real(kind = kreal), intent(in) :: xx(nnod,3), field_vec(nnod, 3)
  integer(kind = kint), intent(in) :: interior_surf(nsurf)
  real(kind = kreal), intent(in) :: line_len
  real(kind = kreal), intent(inout) :: x_start(3), v_start(3)
  integer(kind = kint), intent(in) :: isurf_org(3)
  integer(kind=kint), intent(inout) :: itr_num
  integer(kind=kint), intent(inout) :: iflag_comm, iflag_dir
!
  integer(kind = kint) :: i, node_id
  integer(kind = kint) ::isurf_start, isurf_end, isf_org, isf_tgt, iele
  real(kind = kreal) :: xi(2)
  real(kind = kreal) :: step_len, integral_len, x_org(3), x_tgt(3), v_tgt(3)
!
  !itr_num = 0
  integral_len = 0.0

  iele = isurf_org(1)
  isf_org = isurf_org(2)
  isurf_start = abs(isf_4_ele(iele, isf_org))
  do
    itr_num = itr_num + 1
    step_len = 0.0
    x_org(1:3) = x_start(1:3)
    if(isurf_start .lt. 1 .or. isurf_start .gt. nsurf) then
      iflag_comm = -10
      return
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
        call find_line_end_in_1ele(iflag_dir, nnod, nele, nsurf,                &
        &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,              &
        &      v_start, x_start, isf_tgt, x_tgt, xi)
        if(isf_tgt .gt. 0) then
        ! find hit surface
          exit
        end if
      end if
    end do
    if(isf_tgt .eq. 0) then
      !write(*,*) 'can not find hit surface at', isurf_start
      iflag_comm = -11
      return
    end if
    isurf_end = abs(isf_4_ele(iele,isf_tgt))
    call cal_field_on_surf_vector(nnod, nsurf, nnod_4_surf,         &
    &      ie_surf, isurf_end, xi, field_vec, v_tgt)
    isf_org =  0
    x_start(1:3) = half * (x_start(1:3) + x_tgt(1:3))
    v_start(1:3) = half * (v_start(1:3) + v_tgt(1:3))
    call find_line_end_in_1ele(iflag_dir, nnod, nele, nsurf,            &
    &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, isf_org,          &
    &      v_start, x_start, isf_tgt, x_tgt, xi)
    if(isf_tgt .eq. 0) then
      iflag_comm = -12
      return
    end if
    ! exit point after 2nd field line trace
    isurf_end = abs(isf_4_ele(iele,isf_tgt))
    call cal_field_on_surf_vector(nnod, nsurf, nnod_4_surf,         &
    &      ie_surf, isurf_end, xi, field_vec, v_start)
    step_len = sqrt( (x_tgt(1) - x_org(1))**2                       &
    &               + (x_tgt(2) - x_org(2))**2                      &
    &               + (x_tgt(3) - x_org(3))**2)
    integral_len = integral_len + step_len
    x_start(1:3) =  x_tgt(1:3)

    do i = 1, nnod_4_surf
      node_id = ie_surf(isurf_end,i)
      if(IGROUP_nod(node_id) .ne. group_id) then
        iflag_comm = 10
        return
      end if
    end do
    isurf_start = isurf_end

    if(integral_len .ge. line_len) then
      iflag_comm = 1
      return
    end if
    if(itr_num .gt. 200) then
      !write(*,*) 'iteration too large in 1: ', itr_num
      !write(*,*) 'total length: ', len_sum, 'kernel', k_value, 'step', step_len
      return
    end if
  end do

end subroutine simulate_field_line_integral
!
! -------------------------------------------------------------------
!
subroutine seed_particles(nnod, nele, nsurf, nnod_4_surf,      &
&           isf_4_ele, ie_surf, iele_4_surf, interior_surf,    &
&           xx, field,                                         &
&           particles, num_particle, time_cost)
!

!
  integer(kind = kint), intent(in) :: nnod, nele, nsurf
  integer(kind = kint), intent(in) :: nnod_4_surf
  integer(kind = kint), intent(in) :: ie_surf(nsurf,nnod_4_surf)
  integer(kind = kint), intent(in) :: isf_4_ele(nele,nsurf_4_ele)
  integer(kind = kint), intent(in) :: iele_4_surf(nsurf, 2, 2)
  integer(kind = kint), intent(in) :: interior_surf(nsurf)
  real(kind = kreal), intent(in) :: field(nnod,3), xx(nnod,3)
  integer(kind = kint), intent(in) :: num_particle
  type(simulate_particle), intent(in) :: particles(num_particle)
  !real(kind = kreal), intent(inout) :: time_cost(num_domain)
  type(time_esti), intent(inout) :: time_cost(num_domain)
!
  integer(kind = kint) :: isurf_org(3), isurf_hit
  integer(kind = kint) :: i, iele, iflag_dir, iflag_found_sf, iflag_comm
  integer(kind = kint) :: isf_tgt, itr_num, cnt
  real(kind = kreal) :: xx_org(3), vec_org(3), new_pos(3), new_vec(3), xi(2)
  real(kind = kreal) :: time_cost_cnt(num_domain), aver_time_cost
!
  time_cost(:)%total_time = 0.0
  time_cost(:)%cnt = 0
  do i = 1, num_particle
    iele = particles(i)%ele_id
    if(iele .le. izero .or. iele .gt. nele) then
      write(*,*) 'invalide element'
      continue
    end if
    itr_num = 0
    ! forward integral
    iflag_dir = 1
    xx_org(1:3) = particles(i)%pos(1:3)
    vec_org(1:3) = particles(i)%vec(1:3)

    call find_line_end_in_1ele(iflag_dir, nnod, nele, nsurf,         &
    &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, izero,         &
    &      vec_org, xx_org, isf_tgt, new_pos, xi)
    if(isf_tgt .gt. 0) then
      iflag_found_sf = 1
    else
      write(*,*) 'no surface hit when particle in the start element'
      continue
    end if
    isurf_org(1) = iele
    isurf_org(2) = isf_tgt
    isurf_hit = abs(isf_4_ele(iele,isf_tgt))
    call cal_field_on_surf_vector(nnod, nsurf, nnod_4_surf,                   &
    &      ie_surf, isurf_hit, xi, field, new_vec)
    call simulate_field_line_integral(nnod, nele, nsurf,                      &
    &           nnod_4_surf, isf_4_ele, iele_4_surf, ie_surf, interior_surf,  &
    &           iflag_dir, xx, field, isurf_org, particles(i)%group_id,       &
    &           new_pos, new_vec, particles(i)%line_len, itr_num, iflag_comm)
    !write(*,*) 'foward iter_num ', itr_num, 'forward res ', iflag_comm
    ! backward integral
    iflag_dir = -1
    xx_org(1:3) = particles(i)%pos(1:3)
    vec_org(1:3) = particles(i)%vec(1:3)
    call find_line_end_in_1ele(iflag_dir, nnod, nele, nsurf,         &
    &      nnod_4_surf, isf_4_ele, ie_surf, xx, iele, izero,         &
    &      vec_org, xx_org, isf_tgt, new_pos, xi)
    if(isf_tgt .gt. 0) then
      iflag_found_sf = 1
    else
      write(*,*) 'no surface hit when particle in the start element'
      continue
    end if
    isurf_org(1) = iele
    isurf_org(2) = isf_tgt
    isurf_hit = abs(isf_4_ele(iele,isf_tgt))
    call cal_field_on_surf_vector(nnod, nsurf, nnod_4_surf,                   &
    &      ie_surf, isurf_hit, xi, field, new_vec)
    call simulate_field_line_integral(nnod, nele, nsurf,                      &
    &           nnod_4_surf, isf_4_ele, iele_4_surf, ie_surf, interior_surf,  &
    &           iflag_dir, xx, field, isurf_org, particles(i)%group_id,       &
    &           new_pos, new_vec, particles(i)%line_len, itr_num, iflag_comm)
    !write(*,*) 'total iter_num ', itr_num, 'backward res ', iflag_comm
    time_cost(particles(i)%group_id)%cnt = time_cost(particles(i)%group_id)%cnt + 1
    time_cost(particles(i)%group_id)%total_time =       &
    &           time_cost(particles(i)%group_id)%total_time + itr_num
  end do
  cnt = 0
  aver_time_cost = 0.0
  do i = 1, num_domain
    if(time_cost(i)%cnt .ne. 0) then
      time_cost(i)%ave_time = time_cost(i)%total_time / time_cost(i)%cnt
      aver_time_cost = aver_time_cost + time_cost(i)%ave_time
      cnt = cnt + 1
    end if
  end do
  if(aver_time_cost .ne. 0.0) then
    aver_time_cost = aver_time_cost / cnt
  else
    aver_time_cost = 1.0
  end if
  do i = 1, num_domain
    if(time_cost(i)%cnt .eq. 0.0) then
      time_cost(i)%ave_time = aver_time_cost
    end if
  end do
end subroutine seed_particles
!
! -------------------------------------------------------------------
!
subroutine get_ele_group(eles, ele_id, group_id)
!
  type(element_data), intent(in) :: eles
  integer(kind = kint), intent(in) :: ele_id
  integer(kind = kint), intent(inout) :: group_id
!
  integer(kind = kint) :: nod_1_id

  nod_1_id = eles%ie(ele_id,1)
  group_id = IGROUP_nod(nod_1_id)

end subroutine get_ele_group
!
! -------------------------------------------------------------------
!
logical function is_ele_in_group(ele_idx, eles)
!
  integer(kind = kint), intent(in):: ele_idx
  type(element_data), intent(in) :: eles
!
  integer(kind = kint) :: i
  integer(kind = kint) :: nodes_ele(eles%nnod_4_ele)

  nodes_ele(:) = eles%ie(ele_idx, :)
  is_ele_in_group = .true.
  do i = 2, eles%nnod_4_ele
! if the two node is not in same group
    if (IGROUP_nod(nodes_ele(i)) .ne. IGROUP_nod(nodes_ele(i-1))) then
      is_ele_in_group = .false.
      exit
    end if
  end do
end function is_ele_in_group
!
! -------------------------------------------------------------------
!
subroutine next_ele_in_group(eles, eles_idx, eleidx)
!
  type(element_data), intent(in) :: eles
  integer(kind = kint), intent(in):: eles_idx(eles%numele)
  integer(kind = kint), intent(inout) :: eleidx
!
  do while(is_ele_in_group(eles_idx(eleidx), eles) .eqv. .false.)
    eleidx = eleidx + 1
    if(eleidx .ge. eles%numele) then
      exit
    end if
  end do
end subroutine next_ele_in_group
!
! -------------------------------------------------------------------
!
subroutine get_particle_ele(eles, field_vec, ele_id, particle)
!
  type(simulate_particle), intent(inout) :: particle
  type(element_data), intent(in) :: eles
  type(vector_field), intent(in) :: field_vec
  integer(kind = kint), intent(in) :: ele_id
!
  integer(kind = kint) :: nodes_ele(eles%nnod_4_ele)
  integer(kind = kint) :: i
  real(kind = kreal) :: vec_tmp(3), ptcl_vec(3)

  particle%ele_id = ele_id
  particle%pos(:) = eles%x_ele(ele_id,:)
  call get_ele_group(eles, ele_id, particle%group_id)
! the line length one field line will trace is decided by ctl file
  particle%line_len = 0.8

  nodes_ele(:) = eles%ie(ele_id, :)
  ptcl_vec(:) = 0
  do i = 1, eles%nnod_4_ele
    vec_tmp(:) = field_vec%d_ucd(nodes_ele(i),:)
    ptcl_vec(:) = ptcl_vec(:) + vec_tmp(:)
  end do
  ptcl_vec(:) = ptcl_vec(:) / eles%nnod_4_ele
  particle%vec(:) = ptcl_vec(:)
end subroutine get_particle_ele
!
! -------------------------------------------------------------------
!
subroutine alloc_vector_field(v_field)
!
  type(vector_field), intent(inout) :: v_field
  if(v_field%nnod .gt. izero) then
    allocate(v_field%d_ucd(v_field%nnod, v_field%ncomp))
  end if
end subroutine alloc_vector_field
!
! -------------------------------------------------------------------
!
subroutine choose_particles_from_eles(elements, field_vec, particles, num_ptcls)
!
  use quicksort
  use cal_minmax_and_stacks
!
  type(element_data), intent(in) :: elements
  type(vector_field), intent(in) :: field_vec
  type(simulate_particle), pointer, intent(inout) :: particles(:)
  integer(kind = kint), intent(inout) :: num_ptcls
!
  integer(kind = kint) :: eleidx(elements%numele)
  real(kind = kreal) :: elexx(elements%numele,3)
  integer(kind = kint) :: i, num, irest, nl, nr, ncou
!
  do i= 1, elements%numele
    eleidx(i)= i
    elexx(i,:)= elements%x_ele(i,:)
  end do

  !allocate(particles(num_ptcls))

  call quicksort_coord_w_index(elements%numele,elexx,ione, elements%numele, eleidx)
  call cal_divide_and_rest(num, irest, elements%numele, num_ptcls)

  nl = 1
  nr = 1
  do i = 1, irest
    nr = nl + num
    ncou = (nl + nr)/2
    nl = nr + 1
    call next_ele_in_group(elements, eleidx, ncou)
    call get_particle_ele(elements, field_vec, eleidx(ncou), particles(i))
  end do

  do i = irest+1, num_ptcls
    nr = nl + num - 1
    ncou = (nl + nr)/2
    nl = nr + 1
    call next_ele_in_group(elements, eleidx, ncou)
    call get_particle_ele(elements, field_vec, eleidx(ncou), particles(i))
  end do
end subroutine choose_particles_from_eles
!
! -------------------------------------------------------------------
!
subroutine cal_partition_tbl(time_cost, num_domain, partition_tbl)
!
  integer(kind = kint), intent(in) :: num_domain
  !real(kind = kreal), intent(in) :: time_cost(num_domain)
  type(time_esti), intent(in) :: time_cost(num_domain)
  real(kind = kreal), intent(inout) :: partition_tbl(num_domain)
!
  real(kind = kreal) :: M
integer(kind = kint) :: i
!
  M = 0.0
  do i = 1, num_domain
    if(time_cost(i)%ave_time .ne. 0.0) then
      M = M + 1/time_cost(i)%ave_time
    end if
  end do

  if(M .ne. 0.0) then
    M = 1/M
  else
    partition_tbl(1:num_domain) = 1.0 / num_domain
    return
  end if

  do i = 1, num_domain
    if(time_cost(i)%ave_time .ne. 0.0) then
      partition_tbl(i) = M / time_cost(i)%ave_time
    end if
  end do
end subroutine cal_partition_tbl
!
! -------------------------------------------------------------------
!
subroutine cal_part_dim_tbl(num_domain, ndivide_eb, partition_tbl, part_dim_tbl)
!
  integer(kind = kint), intent(in) :: num_domain
  integer(kind = kint), intent(in) :: ndivide_eb(3)
  real(kind = kreal), intent(in) :: partition_tbl(num_domain)
  type(dimension_part_tbl), intent(inout) :: part_dim_tbl
!
  integer(kind = kint) :: i, j, k, division_tmp, idx_tmp, sub_tmp
!
  call allocate_dim_part_tbl(part_dim_tbl, ndivide_eb)
  do i = 1, 3
    write(*,*) 'i', i
    part_dim_tbl%part_dim(i)%ratio(1:ndivide_eb(i)) = 0.0
    division_tmp = 1
    do k = i, 3
      division_tmp = division_tmp * ndivide_eb(k)
    end do
    sub_tmp = division_tmp / ndivide_eb(i)
    do j = 1, num_domain
      idx_tmp = mod(j-1, division_tmp)
      idx_tmp = idx_tmp / sub_tmp + 1
      part_dim_tbl%part_dim(i)%ratio(idx_tmp) = part_dim_tbl%part_dim(i)%ratio(idx_tmp) + partition_tbl(j)
    end do
  end do
! debug output
  do i = 1, 3
    write(*,*) i, ':', part_dim_tbl%part_dim(i)%ratio(1:ndivide_eb(i))
  end do
end subroutine cal_part_dim_tbl
!
! -------------------------------------------------------------------
!
end module intelligent_partition

