!ordering_ele_4_fix_bd.f90
!     module ordering_ele_4_fix_bd
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!        modified by H.Matsui on Jan., 2009
!
!      subroutine reordering_ele_4_fix_bd(np_smp, nmax_idx_ibc,         &
!     &      num_idx_ibc, ele_bc_id, nod_bc_id, ibc_end, ibc_shape,     &
!     &      ibc_stack, ibc_stack_smp, nnod_4_ele)
!
      module ordering_ele_4_fix_bd
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
      subroutine reordering_ele_4_fix_bd(np_smp, nmax_idx_ibc,          &
     &      num_idx_ibc, ele_bc_id, nod_bc_id, ibc_end, ibc_shape,      &
     &      ibc_stack, ibc_stack_smp, nnod_4_ele)
!
      use cal_minmax_and_stacks
!
      integer (kind= kint), intent(in) :: np_smp
      integer (kind= kint), intent(in) :: nmax_idx_ibc, num_idx_ibc
      integer (kind= kint), intent(in) :: nnod_4_ele
!
      integer (kind= kint), intent(inout) :: ibc_end
      integer (kind= kint), intent(inout) :: ibc_stack(0:nnod_4_ele)
      integer (kind= kint), intent(inout)                               &
     &       :: ibc_stack_smp(0:nnod_4_ele*np_smp)
      integer (kind= kint), intent(inout) :: ibc_shape(nnod_4_ele)
      integer (kind= kint), intent(inout) :: ele_bc_id(nmax_idx_ibc)
      integer (kind= kint), intent(inout) :: nod_bc_id(nmax_idx_ibc)
!
      integer (kind= kint) :: ele_bc_id_wk(nmax_idx_ibc)
      integer (kind= kint) :: nod_bc_id_wk(nmax_idx_ibc)
      integer (kind= kint) :: smp_stack_work(0:np_smp)
!
      integer (kind= kint) :: ii, kk, maxele_smp, iflag
      integer (kind= kint) :: iproc, iproc1, inum
      integer (kind= kint) :: istart, iend
!
!
!   set range of local node ID
!
      if (num_idx_ibc .gt. 0) then
!
        ii = 0
        do kk = 1, nnod_4_ele
          iflag = 0
          do inum = 1, num_idx_ibc
            if ( nod_bc_id(inum) .eq. kk ) iflag = 1
          end do
          if (iflag .eq. 1) then
            ii = ii+1
            ibc_shape(ii) = kk
          end if
        end do
        ibc_end = ii
!
        do inum = 1, num_idx_ibc
          ele_bc_id_wk(inum) = ele_bc_id(inum)
          nod_bc_id_wk(inum) = nod_bc_id(inum)
        end do
!
!  ordering
!
        ii = 0
        do kk = 1, ibc_end
          do inum = 1, num_idx_ibc
!
            if (nod_bc_id_wk(inum) .eq. ibc_shape(kk) ) then
              ii = ii + 1
              ele_bc_id(ii) = ele_bc_id_wk(inum)
              nod_bc_id(ii) = nod_bc_id_wk(inum)
            end if
          end do
!
          ibc_stack(kk) = ii
!
        end do
!
!   divide for smp
!
        do kk = 1, ibc_end
!
          istart = ibc_stack(kk-1)+1
          iend   = ibc_stack(kk) 
          call count_number_4_smp(np_smp, istart, iend,                 &
     &               smp_stack_work, maxele_smp)
!
          do iproc = 0, np_smp
            iproc1 = np_smp*(kk-1)+iproc
            ibc_stack_smp(iproc1) = smp_stack_work(iproc)
          end do
!
        end do
!
      end if
!
      end subroutine reordering_ele_4_fix_bd
!
!-----------------------------------------------------------------------
!
      end module ordering_ele_4_fix_bd
