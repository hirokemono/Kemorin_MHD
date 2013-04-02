!
!      module m_sph_trans_comm_table
!
!     Written by H. Matsui on July, 2007
!
!      subroutine allocate_sph_comm_stack
!      subroutine allocate_sph_comm_item(nnod_rtp, nnod_rtm,            &
!     &          nnod_rlm, nnod_rj)
!
!      subroutine deallocate_sph_comm_item
!
!      subroutine allocate_sph_comm_stack_rtp
!      subroutine allocate_sph_comm_stack_rtm
!      subroutine allocate_sph_comm_stack_rlm
!      subroutine allocate_sph_comm_stack_rj
!
!      subroutine allocate_sph_comm_item_rtp(nnod_rtp)
!      subroutine allocate_sph_comm_item_rtm(nnod_rtm)
!      subroutine allocate_sph_comm_item_rlm(nnod_rlm)
!      subroutine allocate_sph_comm_item_rj(nnod_rj)
!
!      subroutine allocate_idx_gl_rtp_compare
!      subroutine allocate_idx_gl_rtm_out
!      subroutine allocate_idx_gl_rlm_out
!      subroutine allocate_idx_gl_rj_compare
!
!      subroutine deallocate_sph_comm_item_rtp
!      subroutine deallocate_sph_comm_item_rtm
!      subroutine deallocate_sph_comm_item_rlm
!      subroutine deallocate_sph_comm_item_rj
!
!      subroutine deallocate_idx_gl_rtp_compare
!      subroutine deallocate_idx_gl_rtm_out
!      subroutine deallocate_idx_gl_rlm_out
!      subroutine deallocate_idx_gl_rj_compare
!
!      subroutine set_reverse_sph_comm_table(numnod, ntot_item,         &
!     &          item_sr, irev_sr)
!
      module m_sph_trans_comm_table
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: nneib_domain_rtp
      integer(kind = kint) :: ntot_item_sr_rtp
      integer(kind = kint) :: iflag_self_rtp
      integer(kind = kint), allocatable :: id_domain_rtp(:)
      integer(kind = kint), allocatable :: istack_sr_rtp(:)
      integer(kind = kint), allocatable :: item_sr_rtp(:)
      integer(kind = kint), allocatable :: irev_sr_rtp(:)
!
      integer(kind = kint) :: nneib_domain_rtm
      integer(kind = kint) :: ntot_item_sr_rtm
      integer(kind = kint) :: iflag_self_rtm
      integer(kind = kint), allocatable :: id_domain_rtm(:)
      integer(kind = kint), allocatable :: istack_sr_rtm(:)
      integer(kind = kint), allocatable :: item_sr_rtm(:)
      integer(kind = kint), allocatable :: irev_sr_rtm(:)
!
!
      integer(kind = kint) :: nneib_domain_rlm
      integer(kind = kint) :: ntot_item_sr_rlm
      integer(kind = kint) :: iflag_self_rlm
      integer(kind = kint), allocatable :: id_domain_rlm(:)
      integer(kind = kint), allocatable :: istack_sr_rlm(:)
      integer(kind = kint), allocatable :: item_sr_rlm(:)
      integer(kind = kint), allocatable :: irev_sr_rlm(:)
!
      integer(kind = kint) :: nneib_domain_rj
      integer(kind = kint) :: ntot_item_sr_rj
      integer(kind = kint) :: iflag_self_rj
      integer(kind = kint), allocatable :: id_domain_rj(:)
      integer(kind = kint), allocatable :: istack_sr_rj(:)
      integer(kind = kint), allocatable :: item_sr_rj(:)
      integer(kind = kint), allocatable :: irev_sr_rj(:)
!
!
      integer(kind = kint), allocatable :: idx_gl_rtp_compare(:,:)
      integer(kind = kint), allocatable :: idx_gl_rtm_out(:,:)
      integer(kind = kint), allocatable :: idx_gl_rlm_out(:,:)
      integer(kind = kint), allocatable :: idx_gl_rj_compare(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_stack
!
!
      call allocate_sph_comm_stack_rtp
      call allocate_sph_comm_stack_rtm
      call allocate_sph_comm_stack_rlm
      call allocate_sph_comm_stack_rj
!
      end subroutine allocate_sph_comm_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_item(nnod_rtp, nnod_rtm,             &
     &          nnod_rlm, nnod_rj)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rtm
      integer(kind = kint), intent(in) :: nnod_rlm, nnod_rj
!
!
      call allocate_sph_comm_item_rtp(nnod_rtp)
      call allocate_sph_comm_item_rtm(nnod_rtm)
      call allocate_sph_comm_item_rlm(nnod_rlm)
      call allocate_sph_comm_item_rj(nnod_rj)
!
      end subroutine allocate_sph_comm_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_comm_item
!
!
      call deallocate_sph_comm_item_rtp
      call deallocate_sph_comm_item_rtm
      call deallocate_sph_comm_item_rlm
      call deallocate_sph_comm_item_rj
!
      end subroutine deallocate_sph_comm_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_stack_rtp
!
!
      allocate( id_domain_rtp(nneib_domain_rtp) )
      allocate( istack_sr_rtp(0:nneib_domain_rtp) )
      if(nneib_domain_rtp .gt. 0) id_domain_rtp =  0
      istack_sr_rtp =  0
      iflag_self_rtp = 0
!
      end subroutine allocate_sph_comm_stack_rtp
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_stack_rtm
!
!
      allocate( id_domain_rtm(nneib_domain_rtm) )
      allocate( istack_sr_rtm(0:nneib_domain_rtm) )
      if(nneib_domain_rtm .gt. 0) id_domain_rtm =  0
      istack_sr_rtm =  0
      iflag_self_rtm = 0
!
      end subroutine allocate_sph_comm_stack_rtm
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_stack_rlm
!
!
      allocate( id_domain_rlm(nneib_domain_rlm) )
      allocate( istack_sr_rlm(0:nneib_domain_rlm) )
      if(nneib_domain_rlm .gt. 0) id_domain_rlm =  0
      istack_sr_rlm =  0
      iflag_self_rlm = 0
!
      end subroutine allocate_sph_comm_stack_rlm
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_stack_rj
!
!
      allocate( id_domain_rj(nneib_domain_rj) )
      allocate( istack_sr_rj(0:nneib_domain_rj) )
      if(nneib_domain_rj .gt. 0) id_domain_rj =  0
      istack_sr_rj =  0
      iflag_self_rj = 0
!
      end subroutine allocate_sph_comm_stack_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_item_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
!
!
      allocate( item_sr_rtp(ntot_item_sr_rtp) )
      allocate( irev_sr_rtp(nnod_rtp) )
      if(ntot_item_sr_rtp .gt. 0) item_sr_rtp = 0
      if(nnod_rtp .gt. 0) irev_sr_rtp = 0
!
      end subroutine allocate_sph_comm_item_rtp
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_item_rtm(nnod_rtm)
!
      integer(kind = kint), intent(in) :: nnod_rtm
!
!
      allocate( item_sr_rtm(ntot_item_sr_rtm) )
      allocate( irev_sr_rtm(nnod_rtm) )
      if(ntot_item_sr_rtm .gt. 0) item_sr_rtm = 0
      if(nnod_rtm .gt. 0) irev_sr_rtm = 0
!
      end subroutine allocate_sph_comm_item_rtm
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_item_rlm(nnod_rlm)
!
      integer(kind = kint), intent(in) :: nnod_rlm
!
!
      allocate( item_sr_rlm(ntot_item_sr_rlm) )
      allocate( irev_sr_rlm(nnod_rlm) )
      if(ntot_item_sr_rlm .gt. 0) item_sr_rlm = 0
      if(nnod_rlm .gt. 0) irev_sr_rlm = 0
!
      end subroutine allocate_sph_comm_item_rlm
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_item_rj(nnod_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj
!
!
      allocate( item_sr_rj(ntot_item_sr_rj) )
      allocate( irev_sr_rj(nnod_rj) )
      if(ntot_item_sr_rj .gt. 0) item_sr_rj = 0
      if(nnod_rj .gt. 0) irev_sr_rj = 0
!
      end subroutine allocate_sph_comm_item_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_gl_rtp_compare
!
      allocate( idx_gl_rtp_compare(ntot_item_sr_rtp,3) )
      idx_gl_rtp_compare = 0
!
      end subroutine allocate_idx_gl_rtp_compare
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_gl_rtm_out
!
      allocate( idx_gl_rtm_out(ntot_item_sr_rtm,3) )
      idx_gl_rtm_out = 0
!
      end subroutine allocate_idx_gl_rtm_out
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_gl_rlm_out
!
      allocate( idx_gl_rlm_out(ntot_item_sr_rlm,2) )
      idx_gl_rlm_out = 0
!
      end subroutine allocate_idx_gl_rlm_out
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_gl_rj_compare
!
      allocate( idx_gl_rj_compare(nneib_domain_rj,2) )
      idx_gl_rj_compare = 0
!
      end subroutine allocate_idx_gl_rj_compare
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_comm_item_rtp
!
      deallocate( item_sr_rtp, irev_sr_rtp )
      deallocate( id_domain_rtp )
      deallocate( istack_sr_rtp )
!
      end subroutine deallocate_sph_comm_item_rtp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_comm_item_rtm
!
      deallocate( item_sr_rtm, irev_sr_rtm )
      deallocate( id_domain_rtm )
      deallocate( istack_sr_rtm )
!
      end subroutine deallocate_sph_comm_item_rtm
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_comm_item_rlm
!
      deallocate( item_sr_rlm, irev_sr_rlm )
      deallocate( id_domain_rlm )
      deallocate( istack_sr_rlm )
!
      end subroutine deallocate_sph_comm_item_rlm
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_comm_item_rj
!
      deallocate( item_sr_rj, irev_sr_rj )
      deallocate( id_domain_rj )
      deallocate( istack_sr_rj )
!
      end subroutine deallocate_sph_comm_item_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_gl_rtp_compare
!
      deallocate( idx_gl_rtp_compare )
!
      end subroutine deallocate_idx_gl_rtp_compare
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_gl_rtm_out
!
      deallocate( idx_gl_rtm_out )
!
      end subroutine deallocate_idx_gl_rtm_out
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_gl_rlm_out
!
      deallocate( idx_gl_rlm_out )
!
      end subroutine deallocate_idx_gl_rlm_out
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_gl_rj_compare
!
      deallocate( idx_gl_rj_compare )
!
      end subroutine deallocate_idx_gl_rj_compare
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------!
      subroutine set_reverse_sph_comm_table(numnod, ntot_item,          &
     &          item_sr, irev_sr)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: ntot_item
      integer(kind = kint), intent(in) :: item_sr(ntot_item)
!
      integer(kind = kint), intent(inout) :: irev_sr(numnod)
!
      integer(kind = kint) :: i, k
!
!
!$omp parallel do
      do i = 1, numnod
        irev_sr(i) = ntot_item + 1
      end do
!$omp end parallel do
!
!$omp parallel do private(i)
      do k = 1, ntot_item
        i = item_sr(k)
        irev_sr(i) = k
      end do
!$omp end parallel do
!
      end subroutine set_reverse_sph_comm_table
!
! -----------------------------------------------------------------------
!
      end module m_sph_trans_comm_table
