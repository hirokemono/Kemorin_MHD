!
!     module recursive_bisection
!
!     Written by K. Nakajima
!     Modified by H. Matsui
!
!      subroutine rc_bisection(nnod, inter_nod, xx)
!      subroutine rcb_spherical(nnod, inter_nod,                        &
!     &          radius, colatitude, longitude)
!
!       output: nod_d_grp1%IGROUP
!
      module recursive_bisection
!
      use m_precision
!
      use m_ctl_param_partitioner
      use m_domain_group_4_partition
      use sort_by_position_4_rcb
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine rc_bisection(nnod, inter_nod, xx)
!
      integer(kind = kint), intent(in)  :: nnod, inter_nod
      real(kind= kreal), intent(in) :: xx(nnod,3)
!
!
      integer(kind = kint) :: iter, ip0
!C
!C +-----+
!C | RCB |
!C +-----+
!C===
      call allocate_work_4_rcb(nnod)

      nod_d_grp1%IGROUP(1:nnod)= 0
      nod_d_grp1%IGROUP(1:inter_nod)= 1

      do iter= 1, NPOWER_rcb

        if (idir_rcb(iter).lt.1 .or. idir_rcb(iter).gt.3) stop

        if (idir_rcb(iter) .eq. 1) write (*,'(" X-direction")') 
        if (idir_rcb(iter) .eq. 2) write (*,'(" Y-direction")') 
        if (idir_rcb(iter) .eq. 3) write (*,'(" Z-direction")') 

        do ip0= 1, 2**(iter-1)
!
          call sort_4_rcb(inter_nod, iter, ip0, nod_d_grp1%IGROUP(1),   &
     &        idir_rcb(iter), xx(1,1), xx(1,2), xx(1,3),                &
     &        VAL(1), IS1(1) )
        enddo
!
      enddo
!
      call deallocate_rcb_directions
      call deallocate_work_4_rcb
!
      end subroutine rc_bisection
!
!   --------------------------------------------------------------------
!
      subroutine rcb_spherical(nnod, inter_nod,                         &
     &          radius, colatitude, longitude)
!
      integer(kind = kint), intent(in)  :: nnod, inter_nod
      real(kind= kreal), intent(in) :: radius(nnod)
      real(kind= kreal), intent(in) :: colatitude(nnod)
      real(kind= kreal), intent(in) :: longitude(nnod)
!
      integer(kind = kint) :: ip0, iter
!C
!C +------------------------------+
!C | RCB for spherical coordinate |
!C +------------------------------+
!C===
      call allocate_work_4_rcb(nnod)

      nod_d_grp1%IGROUP(1:nnod)= 0
      nod_d_grp1%IGROUP(1:inter_nod)= 1

      do iter= 1, NPOWER_rcb

        if (idir_rcb(iter).lt.1 .or. idir_rcb(iter).gt.3) stop

        if (idir_rcb(iter) .eq. 1) write (*,'(" r-direction")')
        if (idir_rcb(iter) .eq. 2) write (*,'(" theta-direction")')
        if (idir_rcb(iter) .eq. 3) write (*,'(" phi-direction")')

        do ip0= 1, 2**(iter-1)
!
          call sort_4_rcb(inter_nod, iter, ip0, nod_d_grp1%IGROUP(1),   &
     &        idir_rcb(iter), radius(1), colatitude(1), longitude(1),   &
     &        VAL(1), IS1(1) )
!
        enddo
      enddo

      call deallocate_rcb_directions
      call deallocate_work_4_rcb
!
      end subroutine rcb_spherical
!
!   --------------------------------------------------------------------
!
      end module recursive_bisection
