!sort_sphere_4_rcb.f90
!     module sort_sphere_4_rcb
!
      module sort_sphere_4_rcb
!
!     written by H. Matsui on Aug., 2007
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), allocatable, private :: IS_radial(:)
!
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
      integer(kind= kint) :: i, k, kk, n, ist
!
      integer(kind = kint), parameter :: ione = 1
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
