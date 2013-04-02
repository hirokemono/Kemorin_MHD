!
!     module add_node_4_group
!
!     Writteg by H.Matsui on Oct., 2006
!
!      subroutine allocate_mark_4_near_node(np_smp, numnod)
!      subroutine deallocate_mark_4_near_node
!
!
!      subroutine add_num_nod_4_group(np_smp, numnod, numele,           &
!     &          nnod_4_ele, ie, i_smp_stack, num_grp, ntot_grp,        &
!     &          istack_grp, iele_grp, ntot_nod_grp_org,                &
!     &          inod_stack_grp_org, inod_grp_org, iflag_expand,        &
!     &          nnod_grp)
!
!      subroutine add_nod_id_4_group(np_smp, numnod, numele,            &
!     &          nnod_4_ele, ie, i_smp_stack, num_grp, ntot_grp,        &
!     &          istack_grp, iele_grp, ntot_nod_grp_org,                &
!     &          inod_stack_grp_org, inod_grp_org, iweight_grp_org,     &
!     &          iflag_expand, ntot_nod_grp, inod_stack_grp, nnod_grp,  &
!     &          inod_grp, iweight_grp)
!
      module add_node_4_group
!
      use m_precision
!
      implicit none
!
      integer (kind=kint), allocatable :: imark_4_node(:,:)
      private :: imark_4_node
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_mark_4_near_node(np_smp, numnod)
!
      integer(kind = kint), intent(in) :: np_smp, numnod
!
      allocate( imark_4_node(numnod,np_smp) )
      imark_4_node = 0
!
      end subroutine allocate_mark_4_near_node
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_mark_4_near_node
!
      deallocate( imark_4_node )
!
      end subroutine deallocate_mark_4_near_node
!
!-----------------------------------------------------------------------
!
      subroutine add_num_nod_4_group(np_smp, numnod, numele,            &
     &          nnod_4_ele, ie, i_smp_stack, num_grp, ntot_grp,         &
     &          istack_grp, iele_grp, ntot_nod_grp_org,                 &
     &          inod_stack_grp_org, inod_grp_org, iflag_expand,         &
     &          nnod_grp)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: num_grp, ntot_grp
      integer(kind = kint), intent(in) :: i_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: istack_grp(0:num_grp)
      integer(kind = kint), intent(in) :: iele_grp(ntot_grp)
      integer(kind = kint), intent(in) :: ntot_nod_grp_org
      integer(kind = kint), intent(in) :: inod_stack_grp_org(0:num_grp)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_grp_org(ntot_nod_grp_org)
      integer(kind = kint), intent(in) :: iflag_expand(num_grp)
!
      integer(kind = kint), intent(inout) :: nnod_grp(num_grp)
!
      integer(kind = kint) :: ip, iele, inod, k
      integer(kind = kint) :: ist, ied, inum
      integer(kind = kint) :: jst, jed, jnum
!
!
      nnod_grp(1:num_grp) = 0
!
!$omp parallel do private(ist,ied,inum,jst,jed,jnum,iele,inod,k)
      do ip = 1, np_smp
        ist = i_smp_stack(ip-1) + 1
        ied = i_smp_stack(ip)
        do inum = ist, ied
!
          nnod_grp(inum) =  inod_stack_grp_org(inum)                    &
     &                    - inod_stack_grp_org(inum-1)
!
          if (iflag_expand(inum) .eq. 1) then
            imark_4_node(1:numnod,ip) = 0
!
            jst = inod_stack_grp_org(inum-1) + 1
            jed = inod_stack_grp_org(inum)
            do jnum = jst, jed
              inod = inod_grp_org(jnum)
              imark_4_node(inod,ip) = imark_4_node(inod,ip) + 1
            end do
!
            jst = istack_grp(inum-1) + 1
            jed = istack_grp(inum)
            do jnum = jst, jed
              iele = abs(iele_grp(jnum))
!
              do k = 1, nnod_4_ele
                inod = abs( ie(iele,k) )
                if (imark_4_node(inod,ip) .eq. 0) then
                  nnod_grp(inum) = nnod_grp(inum) + 1
                end if
                imark_4_node(inod,ip) = imark_4_node(inod,ip) + 1
              end do
!
            end do
!
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine add_num_nod_4_group
!
!-----------------------------------------------------------------------
!
      subroutine add_nod_id_4_group(np_smp, numnod, numele,             &
     &          nnod_4_ele, ie, i_smp_stack, num_grp, ntot_grp,         &
     &          istack_grp, iele_grp, ntot_nod_grp_org,                 &
     &          inod_stack_grp_org, inod_grp_org, iweight_grp_org,      &
     &          iflag_expand, ntot_nod_grp, inod_stack_grp, nnod_grp,   &
     &          inod_grp, iweight_grp)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: num_grp, ntot_grp
      integer(kind = kint), intent(in) :: i_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: istack_grp(0:num_grp)
      integer(kind = kint), intent(in) :: iele_grp(ntot_grp)
      integer(kind = kint), intent(in) :: ntot_nod_grp_org
      integer(kind = kint), intent(in) :: inod_stack_grp_org(0:num_grp)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_grp_org(ntot_nod_grp_org)
      integer(kind = kint), intent(in)                                  &
     &                     :: iweight_grp_org(ntot_nod_grp_org)
      integer(kind = kint), intent(in) :: iflag_expand(num_grp)
      integer(kind = kint), intent(in) :: ntot_nod_grp
      integer(kind = kint), intent(in) :: inod_stack_grp(0:num_grp)
!
      integer(kind = kint), intent(inout) :: nnod_grp(num_grp)
      integer(kind = kint), intent(inout) :: inod_grp(ntot_nod_grp)
      integer(kind = kint), intent(inout) :: iweight_grp(ntot_nod_grp)
!
      integer(kind = kint) :: ip, iele, inod, k, icou
      integer(kind = kint) :: ist, ied, inum
      integer(kind = kint) :: jst, jed, jnum, jorg, jnew
!
!
      nnod_grp(1:num_grp) = 0
!
!$omp parallel do                                                       &
!$omp& private(ist,ied,inum,jst,jed,jnum,iele,inod,k,icou,jorg,jnew)
      do ip = 1, np_smp
        ist = i_smp_stack(ip-1) + 1
        ied = i_smp_stack(ip)
        do inum = ist, ied
          imark_4_node(1:numnod,ip) = 0
!
          jst = inod_stack_grp_org(inum-1) + 1
          jed = inod_stack_grp_org(inum)
          do jnum = jst, jed
            inod = inod_grp_org(jnum)
            imark_4_node(inod,ip) = iweight_grp_org(jnum)
            nnod_grp(inum) = nnod_grp(inum) + 1
            icou = inod_stack_grp(inum-1) + nnod_grp(inum)
            inod_grp(icou) = inod
          end do
!
          if (iflag_expand(inum) .eq. 1) then
!
            jst = istack_grp(inum-1) + 1
            jed = istack_grp(inum)
            do jnum = jst, jed
              iele = abs(iele_grp(jnum))
!
              do k = 1, nnod_4_ele
                inod = ie(iele,k)
                if (imark_4_node(inod,ip) .eq. 0) then
                  nnod_grp(inum) = nnod_grp(inum) + 1
                  icou = inod_stack_grp(inum-1) + nnod_grp(inum)
                  inod_grp(icou) = inod
                end if
                imark_4_node(inod,ip) = imark_4_node(inod,ip) + 1
              end do
!
            end do
!
          end if
!
          jed = inod_stack_grp_org(inum)-inod_stack_grp_org(inum-1)
          do jnum = 1, jed
            jorg = inod_stack_grp_org(inum-1) + jnum
            jnew = inod_stack_grp(inum-1) + jnum
            iweight_grp(jnew) = iweight_grp_org(jorg)
          end do
!
          jst = inod_stack_grp(inum-1) + 1 +                            &
     &         inod_stack_grp_org(inum)-inod_stack_grp_org(inum-1)
          jed = inod_stack_grp(inum)
          do jnum = jst, jed
            inod = inod_grp(jnum)
            iweight_grp(jnum) = imark_4_node(inod,ip)
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine add_nod_id_4_group
!
!-----------------------------------------------------------------------
!
      end module add_node_4_group
