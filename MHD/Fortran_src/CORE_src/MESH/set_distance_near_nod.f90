!set_distance_near_nod.f90
!     module set_distance_near_nod
!
!     Writteg by H.Matsui on Oct., 2006
!
!      subroutine add_distance_flag(np_smp, i_smp_stack, num_grp,       &
!     &          ntot_nod_grp_org, inod_stack_grp_org, idist_grp_org,   &
!     &          ntot_nod_grp, inod_stack_grp, idist_grp)
!
!      subroutine sort_added_nod_4_group(num_grp, inod_stack_grp_org,   &
!     &          iflag_expand, ntot_nod_grp, inod_stack_grp, inod_grp)
!
      module set_distance_near_nod
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_distance_flag(np_smp, i_smp_stack, num_grp,        &
     &          ntot_nod_grp_org, inod_stack_grp_org, idist_grp_org,    &
     &          ntot_nod_grp, inod_stack_grp, idist_grp)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: num_grp
      integer(kind = kint), intent(in) :: i_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ntot_nod_grp_org
      integer(kind = kint), intent(in) :: inod_stack_grp_org(0:num_grp)
      integer(kind = kint), intent(in)                                  &
     &                     :: idist_grp_org(ntot_nod_grp_org)
      integer(kind = kint), intent(in) :: ntot_nod_grp
      integer(kind = kint), intent(in) :: inod_stack_grp(0:num_grp)
!
      integer(kind = kint), intent(inout) :: idist_grp(ntot_nod_grp)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: ist, ied, inum
      integer(kind = kint) :: jst, jed, jnum, jorg, jnew
!
!
!$omp parallel do private(ist,ied,inum,jst,jed,jnum,jorg,jnew)
      do ip = 1, np_smp
        ist = i_smp_stack(ip-1) + 1
        ied = i_smp_stack(ip)
        do inum = ist, ied
!
          jed = inod_stack_grp_org(inum)-inod_stack_grp_org(inum-1)
          do jnum = 1, jed
            jorg = inod_stack_grp_org(inum-1) + jnum
            jnew = inod_stack_grp(inum-1) + jnum
            idist_grp(jnew) = idist_grp_org(jorg)
          end do
!
          jst = inod_stack_grp(inum-1) + 1 +                            &
     &         inod_stack_grp_org(inum)-inod_stack_grp_org(inum-1)
          jed = inod_stack_grp(inum)
          do jnum = jst, jed
            idist_grp(jnum) = idist_grp(jst-1) + 1
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine add_distance_flag
!
!-----------------------------------------------------------------------
!
      subroutine sort_added_nod_4_group(num_grp, inod_stack_grp_org,    &
     &          iflag_expand, ntot_nod_grp, inod_stack_grp,             &
     &          inod_grp, iweight_grp)
!
      use quicksort
!
!
      integer(kind = kint), intent(in) :: num_grp
      integer(kind = kint), intent(in) :: inod_stack_grp_org(0:num_grp)
      integer(kind = kint), intent(in) :: iflag_expand(num_grp)
      integer(kind = kint), intent(in) :: ntot_nod_grp
      integer(kind = kint), intent(in) :: inod_stack_grp(0:num_grp)
!
      integer(kind = kint), intent(inout) :: inod_grp(ntot_nod_grp)
      integer(kind = kint), intent(inout) :: iweight_grp(ntot_nod_grp)
!
      integer(kind = kint) :: ist, ied, inum, jst, jed, jnum
!
!
      iweight_grp(1:ntot_nod_grp) = - iweight_grp(1:ntot_nod_grp)
!
      do inum = 1, num_grp
        if (iflag_expand(inum).eq.1) then
          ist = inod_stack_grp(inum-1) + 1                              &
     &         + inod_stack_grp_org(inum) - inod_stack_grp_org(inum-1)
          ied = inod_stack_grp(inum)
          call quicksort_w_index(ntot_nod_grp, iweight_grp,             &
     &        ist, ied, inod_grp)
        end if
      end do
!
      iweight_grp(1:ntot_nod_grp) = - iweight_grp(1:ntot_nod_grp)
!
      end subroutine sort_added_nod_4_group
!
!-----------------------------------------------------------------------
!
      end module set_distance_near_nod
