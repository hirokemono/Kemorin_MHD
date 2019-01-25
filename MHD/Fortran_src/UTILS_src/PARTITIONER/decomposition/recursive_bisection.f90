!
!     module recursive_bisection
!
!     Written by K. Nakajima
!     Modified by H. Matsui
!
!!      subroutine rc_bisection(part_p, nnod, inter_nod, xx, nod_d_grp)
!!      subroutine rcb_spherical(part_p, nnod, inter_nod,               &
!!     &          radius, colatitude, longitude, nod_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
!       output: nod_d_grp%IGROUP
!
      module recursive_bisection
!
      use m_precision
!
      use t_ctl_param_partitioner
      use t_domain_group_4_partition
      use sort_by_position_4_rcb
!
      implicit none
!
      type(partitioner_comm_params), private :: part_comm
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine rc_bisection(part_p, nnod, inter_nod, xx, nod_d_grp)
!
      integer(kind = kint), intent(in)  :: nnod, inter_nod
      real(kind= kreal), intent(in) :: xx(nnod,3)
      type(ctl_param_partitioner), intent(in) :: part_p
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
!
      integer(kind = kint) :: iter, ip0
!C
!C +-----+
!C | RCB |
!C +-----+
!C===
      call alloc_work_4_rcb(nnod, part_comm)

      nod_d_grp%IGROUP(1:nnod)= 0
      nod_d_grp%IGROUP(1:inter_nod)= 1

      do iter= 1, part_p%NPOWER_rcb
        if(part_p%idir_rcb(iter) .lt. 1                                 &
     &      .or. part_p%idir_rcb(iter).gt.3) stop

        if(part_p%idir_rcb(iter) .eq. 1) write (*,'(" X-direction")') 
        if(part_p%idir_rcb(iter) .eq. 2) write (*,'(" Y-direction")') 
        if(part_p%idir_rcb(iter) .eq. 3) write (*,'(" Z-direction")') 

        do ip0= 1, 2**(iter-1)
!
          call sort_4_rcb(inter_nod, iter, ip0, nod_d_grp%IGROUP(1),    &
     &        part_p%idir_rcb(iter), xx(1,1), xx(1,2), xx(1,3),         &
     &        part_comm%VAL(1), part_comm%IS1(1))
        end do
!
      end do
!
      call dealloc_work_4_rcb(part_comm)
!
      end subroutine rc_bisection
!
!   --------------------------------------------------------------------
!
      subroutine rcb_spherical(part_p, nnod, inter_nod,                 &
     &          radius, colatitude, longitude, nod_d_grp)
!
      integer(kind = kint), intent(in)  :: nnod, inter_nod
      real(kind= kreal), intent(in) :: radius(nnod)
      real(kind= kreal), intent(in) :: colatitude(nnod)
      real(kind= kreal), intent(in) :: longitude(nnod)
      type(ctl_param_partitioner), intent(in) :: part_p
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      integer(kind = kint) :: ip0, iter
!C
!C +------------------------------+
!C | RCB for spherical coordinate |
!C +------------------------------+
!C===
      call alloc_work_4_rcb(nnod, part_comm)

      nod_d_grp%IGROUP(1:nnod)= 0
      nod_d_grp%IGROUP(1:inter_nod)= 1

      do iter= 1, part_p%NPOWER_rcb
        if (part_p%idir_rcb(iter).lt.1                                  &
     &     .or. part_p%idir_rcb(iter).gt.3) stop

        if(part_p%idir_rcb(iter) .eq. 1) write (*,'(" r-direction")')
        if(part_p%idir_rcb(iter) .eq. 2)                               &
     &                 write (*,'(" theta-direction")')
        if(part_p%idir_rcb(iter) .eq. 3)                               &
     &                 write (*,'(" phi-direction")')

        do ip0= 1, 2**(iter-1)
!
          call sort_4_rcb(inter_nod, iter, ip0, nod_d_grp%IGROUP(1),    &
     &        part_p%idir_rcb(iter), radius(1), colatitude(1),          &
     &        longitude(1), part_comm%VAL(1), part_comm%IS1(1))
        end do
      end do

      call dealloc_work_4_rcb(part_comm)
!
      end subroutine rcb_spherical
!
!   --------------------------------------------------------------------
!
      end module recursive_bisection
