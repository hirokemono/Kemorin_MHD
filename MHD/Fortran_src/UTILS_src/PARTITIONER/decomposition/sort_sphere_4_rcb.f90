!sort_sphere_4_rcb.f90
!     module sort_sphere_4_rcb
!
!     written by H. Matsui on Aug., 2007
!
!      subroutine s_sort_sphere_4_rcb(num_CMB, IGROUP_cmb, itheta, iphi,&
!     &          colatitude, longitude, VAL, IS1)
!      subroutine part_sphere_with_radius(iradius, IGROUP_radius,       &
!     &          num_layer, nlayer_ICB, nlayer_CMB)
!      subroutine set_domain_list_by_order(nnod, nlevel_1st, nproc,     &
!     &           num, irest, sort_item, ig_item)
!
!      subroutine set_domain_list_w_rev(nnod, nlevel, nproc, num, irest,&
!     &           sort_item, ig_item)
!
      module sort_sphere_4_rcb
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), allocatable, private :: IS_radial(:)
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_sort_sphere_4_rcb(num_CMB, IGROUP_cmb, itheta, iphi, &
     &          colatitude, longitude, VAL, IS1)
!
      use quicksort
      use sort_by_position_4_rcb
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: num_CMB
      integer(kind = kint), intent(in) :: itheta, iphi
      real(kind = kreal), intent(in) :: colatitude(num_CMB)
      real(kind = kreal), intent(in) :: longitude(num_CMB)
!
      real(kind = kreal), intent(inout) :: VAL(num_CMB)
      integer(kind = kint), intent(inout) :: IS1(num_CMB)
      integer(kind = kint), intent(inout) :: IGROUP_cmb(num_CMB)
!
      integer(kind = kint) :: ip0, ncou, num1, irest1
!
!
      ip0 = 1
      IGROUP_cmb(1:num_CMB) = 1
!
!        write(*,*) 'copy_position_sort_4_rcb'
      call copy_position_sort_4_rcb(num_CMB, ip0, IGROUP_cmb,           &
     &    longitude, ncou, VAL, IS1)
!
!        write(*,*) 'quicksort_real_w_index'
      call quicksort_real_w_index(num_CMB, VAL, ione, num_CMB, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
      call sorting_by_2nd_direction(num_CMB, ncou, longitude,           &
     &    colatitude, VAL, IS1)
!
      call cal_divide_and_rest(num1, irest1, num_CMB, iphi)
!
!        write(*,*) 'set_domain_list_by_order'
      call set_domain_list_by_order(num_CMB, ione, iphi, num1, irest1,  &
     &    IS1, IGROUP_cmb)
!
!
      do ip0 = 1, iphi
!
!        write(*,*) 'copy_position_sort_4_rcb'
        call copy_position_sort_4_rcb(num_CMB, ip0, IGROUP_cmb,         &
     &      colatitude, ncou, VAL, IS1)
!
        call quicksort_real_w_index(num_CMB, VAL, ione, ncou, IS1)
!
!        write(*,*) 'sorting_by_2nd_direction'
        call sorting_by_2nd_direction(num_CMB, ncou, colatitude,        &
     &      longitude, VAL, IS1)
!
        call cal_divide_and_rest(num1, irest1, ncou, itheta)
!
!        write(*,*) 'set_domain_list_by_order'
        call set_domain_list_by_order(num_CMB, iphi, itheta,            &
     &      num1, irest1, IS1, IGROUP_cmb)
!
      end do
!
      end subroutine s_sort_sphere_4_rcb
!
!   --------------------------------------------------------------------
!
      subroutine part_sphere_with_radius(iradius, IGROUP_radius,        &
     &          num_layer, nlayer_ICB, nlayer_CMB)
!
      use cal_minmax_and_stacks
!
      integer(kind= kint), intent(in) :: iradius
      integer(kind= kint), intent(in) :: num_layer
      integer(kind= kint), intent(in) :: nlayer_ICB, nlayer_CMB
!
      integer(kind= kint), intent(inout) :: IGROUP_radius(num_layer)
!
      integer(kind= kint) :: irest1, num0, num1
      integer(kind= kint) :: i, k, kk
!
!
      allocate (IS_radial(num_layer))
!
      do i = 1, num_layer
        IS_radial(i) = i
      end do
      write(*,*) 'IS_radial', IS_radial
!
      IGROUP_radius = 1
      write(*,*) 'IGROUP_radius', IGROUP_radius
!
!   grouping outer core
!
      num0 = nlayer_CMB-nlayer_ICB + 1
!      write(*,*) 'cal_divide_and_rest'
      call cal_divide_and_rest(num1, irest1, num0, iradius)
!
!      write(*,*) 'set_domain_list_by_order'
      call set_domain_list_by_order(num0, ione, iradius, num1, irest1,  &
     &    IS_radial(1), IGROUP_radius(nlayer_ICB) )
!
      do k = nlayer_ICB, nlayer_CMB
        IGROUP_radius(k) = iradius - IGROUP_radius(k) + 1
      end do
!
!   grouping inner core
!
      num0 = nlayer_ICB - 1
!      write(*,*) 'cal_divide_and_rest'
      call cal_divide_and_rest(num1, irest1, num0, iradius)
!
!      write(*,*) 'set_domain_list_by_order'
      call set_domain_list_by_order(num0, ione, iradius, num1, irest1,  &
     &    IS_radial(1), IGROUP_radius(1) )
!
!   grouping mantle
!
!      write(*,*) 'cal_divide_and_rest'
      call cal_divide_and_rest(num1, irest1, num_layer, iradius)
!
!      write(*,*) 'num1, irest1', num1, irest1
      kk = nlayer_CMB
      do i = 1, iradius
        if (i.le.irest1) then
          num0 = num1 + 1
        else
          num0 = num1
        end if
!
        do k = 1, nlayer_CMB
          if ( IGROUP_radius(k) .eq. i) num0 = num0 - 1
        end do
!
        do k = 1, num0
          kk = kk + 1
          IGROUP_radius(kk) = i
        end do
!
      end do
!
      num0 = nlayer_ICB - 1
      write(*,*) 'k, N. layerm IGROUP_radius(k)',                      &
     &           nlayer_ICB, nlayer_CMB, num_layer
      do k = 1, num_layer
        write(*,*) k, (num_layer+1-k), IGROUP_radius(k)
      end do
!
      deallocate (IS_radial)
!
      end subroutine part_sphere_with_radius
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_domain_list_by_order(nnod, nlevel_1st, nproc,      &
     &           num, irest, sort_item, ig_item)
!
      integer(kind = kint), intent(in) :: nnod, nproc, nlevel_1st
      integer(kind = kint), intent(in) :: irest, num
      integer(kind = kint), intent(in) :: sort_item(nnod)
      integer(kind = kint), intent(inout) :: ig_item(nnod)
!
      integer(kind = kint) :: ii, ic, in, icou
!
      icou= 0
      do ii = 1, irest
        do ic= 1, num+1
          icou = icou+1
          in= sort_item(icou)
          ig_item(in) = ig_item(in) + (ii-1)*nlevel_1st
        end do
      end do
!
      do ii = irest+1, nproc
        do ic= 1, num
          icou = icou+1
          in= sort_item(icou)
          ig_item(in)= ig_item(in) + (ii-1)*nlevel_1st
        end do
      end do
!
      end subroutine set_domain_list_by_order
!
!   --------------------------------------------------------------------
!
      subroutine set_domain_list_by_volume(nnod, nlevel_1st, nproc,     &
      &           ncou, grp_volume, sort_item, node_volume, ig_item)
!
      use int_volume_of_single_domain
!
      integer(kind = kint), intent(in) :: nnod, nproc, nlevel_1st, ncou
      integer(kind = kint), intent(in) :: sort_item(nnod)
      real(kind = kreal), intent(in) :: node_volume(nnod)
      real(kind = kreal), intent(in) :: grp_volume
      integer(kind = kint), intent(inout) :: ig_item(nnod)
!
      integer(kind = kint) :: ii, in, icou
      real(kind = kreal) :: total_volume, fnode_volume
!
      ii = 1
      icou= 0
      total_volume = 0.0
      write(*,*) 'divided group volume: ', grp_volume
      do icou = 1, ncou
        in = sort_item(icou)
        fnode_volume = node_volume(in)
        total_volume = total_volume + fnode_volume
        if(total_volume .gt. grp_volume) then
          total_volume = 0.0
          if(ii .lt. nproc) then
            ii = ii + 1
          end if
        end if
        ig_item(in) = ig_item(in) + (ii-1)*nlevel_1st
      end do
!
      end subroutine set_domain_list_by_volume
!
!   --------------------------------------------------------------------
!
      subroutine set_domain_list_with_part_tbl                          &
      &           (nnod, ncou, nlevel, nproc, part_tbl,                 &
      &            sort_item, ig_item, tbl_size, order,                 &
      &            group_id, domain_id)
!
      use m_ctl_param_partitioner
!
      integer(kind = kint), intent(in) :: nnod, nproc, nlevel, ncou
      integer(kind = kint), intent(in) :: tbl_size, order
      integer(kind = kint), intent(in) :: sort_item(nnod)
      real(kind = kreal), intent(inout) :: part_tbl(num_domain)
      integer(kind = kint), intent(inout) :: ig_item(nnod)
      integer(kind = kint), intent(inout) :: group_id(num_domain)
      integer(kind = kint), intent(inout) :: domain_id(num_domain)
!
      integer(kind = kint) :: ii, ic, icou, in
      integer(kind = kint) :: istart, iend, igroup, iproc, ncnt
      integer(kind = kint) :: num(nproc), tmp_i, iflag_debug
      real(kind = kreal) :: ratio(nproc), tmp_r, sum_ratio
!
      ratio(1:nproc) = 0.0
      iflag_debug = 0
!     istart and iend indicate which partition ratio table we will use
!     order is the group we will partition, order also eaqual to the first domain id
!     which is the smallest num in this partition group
      do istart = 1, num_domain
        if(domain_id(istart) .eq. order) then
          exit
        end if
      end do
      iend = istart + tbl_size - 1


      do ii = istart, iend
        igroup = ii - istart
        igroup = mod(igroup, nproc)+1
        ratio(igroup) = ratio(igroup)+part_tbl(ii)
        group_id(ii) = igroup
      end do
      ncnt = tbl_size / nproc
      if(iflag_debug .gt. 0) write(*,*)'istart,iend,tbl_size,nproc,ncnt', istart,iend,tbl_size,nproc,ncnt
      do ii = 1, tbl_size
        do ic = istart, iend-ii
          if(group_id(ic) .gt. group_id(ic+1)) then
            tmp_i = group_id(ic)
            group_id(ic) = group_id(ic+1)
            group_id(ic+1) = tmp_i
            tmp_r = part_tbl(ic)
            part_tbl(ic) = part_tbl(ic+1)
            part_tbl(ic+1) = tmp_r
            tmp_i = domain_id(ic)
            domain_id(ic) = domain_id(ic+1)
            domain_id(ic+1) = tmp_i
          end if
        end do
      end do

      if(iflag_debug .gt. 0) then
        write(*,*) 'group: ', group_id(1:num_domain)
        write(*,*) 'part_tbl ', part_tbl(1:num_domain)
        write(*,*) 'domain id', domain_id(1:num_domain)
        write(*,*) 'ratio ', ratio(:)
      end if
      sum_ratio = 0.0
      do iproc = 1, nproc
        sum_ratio = sum_ratio + ratio(iproc)
      end do
!write(*,*) 'sum ratio', sum_ratio
      ratio(1:nproc) = ratio(1:nproc) / sum_ratio
      num(:) = ratio(:) * ncou
      num(nproc) = ncou
      do ii = 1, nproc-1
        num(nproc) = num(nproc) - num(ii)
      end do
      if(iflag_debug .gt. 0) write(*,*) 'num of node', ncou, 'node num ', num(:)
      icou = 0
      do ii = 1, nproc
        do ic= 1, num(ii)
          icou = icou+1
          in= sort_item(icou)
          ig_item(in)= ig_item(in) + (ii-1)*nlevel
        end do
      end do
!
      end subroutine set_domain_list_with_part_tbl
!
!   --------------------------------------------------------------------
!
subroutine set_domain_list_with_part_volume                          &
&           (nnod, ncou, nlevel, nproc, part_volume, n_volume,       &
&            sort_item, ig_item, tbl_size, order,                    &
&            group_id, domain_id)
!
use m_ctl_param_partitioner
!
integer(kind = kint), intent(in) :: nnod, nproc, nlevel, ncou
integer(kind = kint), intent(in) :: tbl_size, order
integer(kind = kint), intent(in) :: sort_item(nnod)
real(kind = kreal), intent(inout) :: part_volume(num_domain)
real(kind = kreal), intent(in) :: n_volume(nnod)
integer(kind = kint), intent(inout) :: ig_item(nnod)
integer(kind = kint), intent(inout) :: group_id(num_domain)
integer(kind = kint), intent(inout) :: domain_id(num_domain)
!
integer(kind = kint) :: ii, ic, icou, in
integer(kind = kint) :: istart, iend, igroup, ncnt
integer(kind = kint) :: tmp_i, iflag_debug
real(kind = kreal) :: ratio_v(nproc), tmp_r, i_v, t_sum_v
!
ratio_v(1:nproc) = 0.0
iflag_debug = 0
!     istart and iend indicate which partition ratio table we will use
!     order is the group we will partition, order also eaqual to the first domain id
!     which is the smallest num in this partition group
do istart = 1, num_domain
  if(domain_id(istart) .eq. order) then
    exit
  end if
end do
iend = istart + tbl_size - 1


do ii = istart, iend
  igroup = ii - istart
  igroup = mod(igroup, nproc)+1
  ratio_v(igroup) = ratio_v(igroup)+part_volume(ii)
  group_id(ii) = igroup
end do
ncnt = tbl_size / nproc
if(iflag_debug .gt. 0) write(*,*)'istart,iend,tbl_size,nproc,ncnt', istart,iend,tbl_size,nproc,ncnt
do ii = 1, tbl_size
  do ic = istart, iend-ii
    if(group_id(ic) .gt. group_id(ic+1)) then
      tmp_i = group_id(ic)
      group_id(ic) = group_id(ic+1)
      group_id(ic+1) = tmp_i
      tmp_r = part_volume(ic)
      part_volume(ic) = part_volume(ic+1)
      part_volume(ic+1) = tmp_r
      tmp_i = domain_id(ic)
      domain_id(ic) = domain_id(ic+1)
      domain_id(ic+1) = tmp_i
    end if
  end do
end do

if(iflag_debug .gt. 0) then
  write(*,*) 'group: ', group_id(1:num_domain)
  write(*,*) 'part_tbl ', part_volume(1:num_domain)
  write(*,*) 'domain id', domain_id(1:num_domain)
  write(*,*) 'ratio ', ratio_v(:)
end if

t_sum_v = 0.0
ii = 1
do icou = 1, ncou
  in = sort_item(icou)
  i_v = n_volume(in)
  t_sum_v = t_sum_v + i_v
  if(t_sum_v .ge. ratio_v(ii) .and. ii .lt. nproc) then
    t_sum_v = 0.0
    ii = ii + 1
  end if
  ig_item(in)= ig_item(in) + (ii-1)*nlevel
end do
!
end subroutine set_domain_list_with_part_volume
!
!   --------------------------------------------------------------------
!
      subroutine set_domain_list_w_rev(nnod, nlevel, nproc, num, irest, &
     &           sort_item, ig_item)
!
      integer(kind = kint), intent(in) :: nnod, nproc, nlevel
      integer(kind = kint), intent(in) :: irest, num
      integer(kind = kint), intent(in) :: sort_item(nnod)
      integer(kind = kint), intent(inout) :: ig_item(nnod)
!
      integer(kind = kint) :: ii, ic, in, icou
!
      icou= 0
      if (mod(nlevel,2) .eq. 1) then
!
        do ii = 1, irest
          do ic= 1, num+1
            icou = icou+1
            in= sort_item(icou)
            ig_item(in) = ii
          end do
        end do
!
        do ii = irest+1, nproc
          do ic= 1, num
            icou = icou+1
            in= sort_item(icou)
            ig_item(in) = ii
          end do
        end do
!
      else
!
        do ii = 1, irest
          do ic= 1, num+1
            icou = icou+1
            in= sort_item(icou)
            ig_item(in) = nproc - ii + 1
          end do
        end do
!
        do ii = irest+1, nproc
          do ic= 1, num
            icou = icou+1
            in= sort_item(icou)
            ig_item(in) = nproc - ii + 1
          end do
        end do
!
      end if
!
      end subroutine set_domain_list_w_rev
!
!   --------------------------------------------------------------------
!
      end module sort_sphere_4_rcb
