!reconnect_psf_overlap_nod.f90
!      module reconnect_psf_overlap_nod
!
!      Written by H. Matsui on Oct., 2011
!
!      subroutine s_reconnect_psf_overlap_nod(num_psf, ntot_nod_psf,    &
!     &          ntot_ele_psf, istack_nod_out, istack_ele_out,          &
!     &          ihash_out_psf, xx_out_psf, iele_out_psf, ie_out_psf)
!      subroutine set_global_psf_node_id(num_psf, ntot_nod_psf,         &
!     &          istack_nod_out, inod_output_psf)
!
      module reconnect_psf_overlap_nod
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), allocatable :: inod_overlap(:)
      integer(kind = kint), allocatable :: ihash(:)
      integer(kind = kint), allocatable :: inod_org(:)
      private :: inod_overlap, inod_org
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_reconnect_psf_overlap_nod(num_psf, ntot_nod_psf,     &
     &          ntot_ele_psf, istack_nod_out, istack_ele_out,           &
     &          ihash_out_psf, xx_out_psf, iele_out_psf, ie_out_psf)
!
      use m_parallel_var_dof
      use quicksort
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: ntot_nod_psf, ntot_ele_psf
      integer(kind = kint), intent(in) :: istack_nod_out(0:num_psf)
      integer(kind = kint), intent(in) :: istack_ele_out(0:num_psf)
      integer(kind = kint), intent(in) :: ihash_out_psf(ntot_nod_psf)
      real(kind = kreal), intent(in) :: xx_out_psf(ntot_nod_psf,3)
!
      integer(kind = kint), intent(inout) :: iele_out_psf(ntot_ele_psf)
      integer(kind = kint), intent(inout) :: ie_out_psf(ntot_ele_psf,3)
!
      integer(kind = kint) :: iele, inod, jnod, i, j, i1, i2, i3
      integer(kind = kint) :: i_psf, ist, ied, ishift
!
!
      if (my_rank .ne. 0) return
!
      allocate(inod_overlap(ntot_nod_psf))
      allocate(inod_org(ntot_nod_psf))
      allocate(ihash(ntot_nod_psf))
!
!$omp parallel do
      do inod = 1, ntot_nod_psf
        inod_org(inod) = inod
        ihash(inod) = ihash_out_psf(inod)
      end do
!$omp end parallel do
!
      do i_psf = 1, num_psf
        ist = istack_nod_out(i_psf-1) + 1
        ied = istack_nod_out(i_psf)
        call quicksort_w_index(ntot_nod_psf, ihash, ist, ied, inod_org)
      end do
!
!
      inod_overlap(1:ntot_nod_psf) = 0
      do i_psf = 1, num_psf
        ist = istack_nod_out(i_psf-1) + 1
        ied = istack_nod_out(i_psf)
        do i = ist, ied
          inod = inod_org(i)
!
          if(inod_overlap(inod) .eq. 0) then
            inod_overlap(inod) = inod
            do j = i+1, ied
              if(ihash(j) .ne. ihash(i)) exit
!
              jnod = inod_org(j)
              if(inod_overlap(jnod) .eq. 0) then
                if     (xx_out_psf(inod,1).eq.xx_out_psf(jnod,1)        &
     &            .and. xx_out_psf(inod,2).eq.xx_out_psf(jnod,2)        &
     &            .and. xx_out_psf(inod,3).eq.xx_out_psf(jnod,3)        &
     &           ) then
                  inod_overlap(jnod) = inod
                end if
              end if
            end do
          end if
!
        end do
      end do
!
!
      do i_psf = 1, num_psf
        ishift = istack_nod_out(i_psf-1)
        ist = istack_ele_out(i_psf-1) + 1
        ied = istack_ele_out(i_psf)
!$omp parallel do private(iele,i1,i2,i3)
        do iele = ist, ied
          i1 = ie_out_psf(iele,1) + ishift
          i2 = ie_out_psf(iele,2) + ishift
          i3 = ie_out_psf(iele,3) + ishift
          iele_out_psf(iele) = iele - ist + 1
          ie_out_psf(iele,1) = inod_overlap(i1) - ishift
          ie_out_psf(iele,2) = inod_overlap(i2) - ishift
          ie_out_psf(iele,3) = inod_overlap(i3) - ishift
        end do
!$omp end parallel do
      end do
!
      deallocate(inod_overlap, inod_org, ihash)
!
      end subroutine s_reconnect_psf_overlap_nod
!
! ----------------------------------------------------------------------
!
      subroutine set_global_psf_node_id(num_psf, ntot_nod_psf,          &
     &          istack_nod_out, inod_output_psf)
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: ntot_nod_psf
      integer(kind = kint), intent(in) :: istack_nod_out(0:num_psf)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: inod_output_psf(ntot_nod_psf)
!
      integer(kind = kint) :: i_psf, ist, ied, inum
!
!
      do i_psf = 1, num_psf
        ist = istack_nod_out(i_psf-1) + 1
        ied = istack_nod_out(i_psf)
        do inum = ist, ied
          inod_output_psf(inum) = inum - istack_nod_out(i_psf-1)
        end do
      end do
!
      end subroutine set_global_psf_node_id
!
! ----------------------------------------------------------------------
!
      end module reconnect_psf_overlap_nod
