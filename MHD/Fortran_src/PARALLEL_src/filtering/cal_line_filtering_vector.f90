!
!      module cal_line_filtering_vector
!
!     Written by H. Matsui in 2004
!     Modified by H. Matsui on Oct., 2006
!
!!      subroutine cal_l_filtering_scalar                              &
!!     &       (numnod, inod_smp_stack, nmax_l_filter,                 &
!!     &        ntot_l_filter, nsize_lf_smp, inod_l_filter_smp,        &
!!     &        istack_l_filter_smp, item_l_filter_smp, c_l_filter_smp,&
!!     &        ncomp_nod, i_filter, d_nod, ff_lf_smp)
!!      subroutine cal_l_filtering_vector                              &
!!     &       (numnod, inod_smp_stack, nmax_l_filter,                 &
!!     &        ntot_l_filter, nsize_lf_smp, inod_l_filter_smp,        &
!!     &        istack_l_filter_smp, item_l_filter_smp, c_l_filter_smp,&
!!     &        ncomp_nod, i_filter, d_nod, ff_lf_smp)
!!      subroutine cal_l_filtering_tensor                              &
!!     &       (numnod, inod_smp_stack, nmax_l_filter,                 &
!!     &        ntot_l_filter, nsize_lf_smp, inod_l_filter_smp,        &
!!     &        istack_l_filter_smp, item_l_filter_smp, c_l_filter_smp,&
!!     &        ncomp_nod, i_filter, d_nod, ff_lf_smp)
!!        filtering along each directions for scalar field
!!          i_filter(input) :: node data ID for original field
!!          i_filter(output) :: node data ID for filtered field
!!
      module cal_line_filtering_vector
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_l_filtering_scalar                                &
     &       (numnod, inod_smp_stack, nmax_l_filter,                   &
     &        ntot_l_filter, nsize_lf_smp, inod_l_filter_smp,          &
     &        istack_l_filter_smp, item_l_filter_smp, c_l_filter_smp,  &
     &        ncomp_nod, i_filter, d_nod, ff_lf_smp)
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_filter
!
      integer(kind = kint), intent(in) :: nmax_l_filter(3)
      integer(kind = kint), intent(in) :: nsize_lf_smp, ntot_l_filter
      integer (kind = kint), intent(in) :: inod_l_filter_smp(numnod,3)
      integer(kind = kint), intent(in)                                  &
     &                :: istack_l_filter_smp(0:nsize_lf_smp*np_smp,3)
      integer(kind = kint), intent(in)                                  &
     &                :: item_l_filter_smp(ntot_l_filter,3)
      real(kind = kreal), intent(in) :: c_l_filter_smp(ntot_l_filter,3)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
      real(kind = kreal), intent(inout) :: ff_lf_smp(numnod,6)
!
      integer (kind = kint) :: ip, inod, jnod, nd, num
      integer (kind = kint) :: ist, ied, idx, isum, i, ii
!
!
      do nd = 1, 3
!
!$omp parallel do private(inod,ist,ied)
       do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
!cdir nodep
        do inod = ist, ied
          ff_lf_smp(inod,1) = 0.0d0
        end do
       end do
!$omp end parallel do
!
!
!
!$omp parallel do private(inod,jnod,ist,i,idx,isum,ii,num)
       do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        do isum = 1, nmax_l_filter(nd)
          ii = (ip-1)*nsize_lf_smp + isum
          num = istack_l_filter_smp(ii,nd)                              &
     &         - istack_l_filter_smp(ii-1,nd)
!cdir nodep
         do i = 1, num
           inod = inod_smp_stack(ip-1) + i
           idx = istack_l_filter_smp(ii-1,nd) + i
           jnod = item_l_filter_smp(idx,nd)
           ff_lf_smp(inod,1) = ff_lf_smp(inod,1)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter  )
          end do
        end do
       end do
!$omp end parallel do
!
!$omp parallel do private(inod,jnod,ist,ied)
       do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
!cdir nodep
        do jnod = ist, ied
          inod = inod_l_filter_smp(jnod,nd)
          d_nod(inod,i_filter  ) = ff_lf_smp(jnod,1)
        end do
       end do
!$omp end parallel do
!
      end do
!
      end subroutine cal_l_filtering_scalar
!
!-----------------------------------------------------------------------
!
      subroutine cal_l_filtering_vector                                &
     &       (numnod, inod_smp_stack, nmax_l_filter,                   &
     &        ntot_l_filter, nsize_lf_smp, inod_l_filter_smp,          &
     &        istack_l_filter_smp, item_l_filter_smp, c_l_filter_smp,  &
     &        ncomp_nod, i_filter, d_nod, ff_lf_smp)
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_filter
!
      integer(kind = kint), intent(in) :: nmax_l_filter(3)
      integer(kind = kint), intent(in) :: nsize_lf_smp, ntot_l_filter
      integer (kind = kint), intent(in) :: inod_l_filter_smp(numnod,3)
      integer(kind = kint), intent(in)                                  &
     &                :: istack_l_filter_smp(0:nsize_lf_smp*np_smp,3)
      integer(kind = kint), intent(in)                                  &
     &                :: item_l_filter_smp(ntot_l_filter,3)
      real(kind = kreal), intent(in) :: c_l_filter_smp(ntot_l_filter,3)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
      real(kind = kreal), intent(inout) :: ff_lf_smp(numnod,6)
!
      integer (kind = kint) :: ip, inod, jnod, nd, num
      integer (kind = kint) :: ist, ied, idx, isum, i, ii
!
!
      do nd = 1, n_vector
!
!$omp parallel do private(inod,ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           ff_lf_smp(inod,1) = 0.0d0
           ff_lf_smp(inod,2) = 0.0d0
           ff_lf_smp(inod,3) = 0.0d0
         end do
       end do
!$omp end parallel do
!
!$omp parallel do private(inod,jnod,ist,i,idx,isum,ii,num)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         do isum = 1, nmax_l_filter(nd)
          ii = (ip-1)*nsize_lf_smp + isum
          num = istack_l_filter_smp(ii,nd)                              &
     &         - istack_l_filter_smp(ii-1,nd)
           do i = 1, num
             inod = inod_smp_stack(ip-1) + i
             idx = istack_l_filter_smp(ii-1,nd) + i
             jnod = item_l_filter_smp(idx,nd)
             ff_lf_smp(inod,1) = ff_lf_smp(inod,1)                      &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter  )
             ff_lf_smp(inod,2) = ff_lf_smp(inod,2)                      &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+1)
             ff_lf_smp(inod,3) = ff_lf_smp(inod,3)                      &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+2)
           end do
         end do
       end do
!$omp end parallel do
!
!$omp parallel do private(inod,jnod,ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do jnod = ist, ied
           inod = inod_l_filter_smp(jnod,nd)
           d_nod(inod,i_filter  ) = ff_lf_smp(jnod,1)
           d_nod(inod,i_filter+1) = ff_lf_smp(jnod,2)
           d_nod(inod,i_filter+2) = ff_lf_smp(jnod,3)
         end do
       end do
!$omp end parallel do
!
      end do
!
      end subroutine cal_l_filtering_vector
!
!-----------------------------------------------------------------------
!
      subroutine cal_l_filtering_tensor                                &
     &       (numnod, inod_smp_stack, nmax_l_filter,                   &
     &        ntot_l_filter, nsize_lf_smp, inod_l_filter_smp,          &
     &        istack_l_filter_smp, item_l_filter_smp, c_l_filter_smp,  &
     &        ncomp_nod, i_filter, d_nod, ff_lf_smp)
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_filter
!
      integer(kind = kint), intent(in) :: nmax_l_filter(3)
      integer(kind = kint), intent(in) :: nsize_lf_smp, ntot_l_filter
      integer (kind = kint), intent(in) :: inod_l_filter_smp(numnod,3)
      integer(kind = kint), intent(in)                                  &
     &                :: istack_l_filter_smp(0:nsize_lf_smp*np_smp,3)
      integer(kind = kint), intent(in)                                  &
     &                :: item_l_filter_smp(ntot_l_filter,3)
      real(kind = kreal), intent(in) :: c_l_filter_smp(ntot_l_filter,3)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
      real(kind = kreal), intent(inout) :: ff_lf_smp(numnod,6)
!
      integer (kind = kint) :: ip, inod, jnod, nd, num
      integer (kind = kint) :: ist, ied, idx, isum, i, ii
!
!
      do nd = 1, 3
!
!$omp parallel do private(inod,ist,ied)
       do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
        do inod = ist, ied
          ff_lf_smp(inod,1) = 0.0d0
          ff_lf_smp(inod,2) = 0.0d0
          ff_lf_smp(inod,3) = 0.0d0
          ff_lf_smp(inod,4) = 0.0d0
          ff_lf_smp(inod,5) = 0.0d0
          ff_lf_smp(inod,6) = 0.0d0
        end do
       end do
!$omp end parallel do
!
!$omp parallel do private(inod,jnod,ist,i,idx,isum,ii,num)
       do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        do isum = 1, nmax_l_filter(nd)
          ii = (ip-1)*nsize_lf_smp + isum
          num = istack_l_filter_smp(ii,nd)                              &
     &         - istack_l_filter_smp(ii-1,nd)
!cdir nodep
!ocl vector, novrec
!voption, indep, vec
         do i = 1, num
           inod = inod_smp_stack(ip-1) + i
           idx = istack_l_filter_smp(ii-1,nd) + i
           jnod = item_l_filter_smp(idx,nd)
           ff_lf_smp(inod,1) = ff_lf_smp(inod,1)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter  )
           ff_lf_smp(inod,2) = ff_lf_smp(inod,2)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+1)
           ff_lf_smp(inod,3) = ff_lf_smp(inod,3)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+2)
           ff_lf_smp(inod,4) = ff_lf_smp(inod,4)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+3)
           ff_lf_smp(inod,5) = ff_lf_smp(inod,5)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+4)
           ff_lf_smp(inod,6) = ff_lf_smp(inod,6)                        &
     &       + c_l_filter_smp(idx,nd) * d_nod(jnod,i_filter+5)
          end do
        end do
       end do
!$omp end parallel do
!
!$omp parallel do private(inod,jnod,ist,ied)
       do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
!cdir nodep
        do jnod = ist, ied
          inod = inod_l_filter_smp(jnod,nd)
          d_nod(inod,i_filter  ) = ff_lf_smp(jnod,1)
          d_nod(inod,i_filter+1) = ff_lf_smp(jnod,2)
          d_nod(inod,i_filter+2) = ff_lf_smp(jnod,3)
          d_nod(inod,i_filter+3) = ff_lf_smp(jnod,4)
          d_nod(inod,i_filter+4) = ff_lf_smp(jnod,5)
          d_nod(inod,i_filter+5) = ff_lf_smp(jnod,6)
        end do
       end do
!$omp end parallel do
!
      end do
!
      end subroutine cal_l_filtering_tensor
!
!-----------------------------------------------------------------------
!
      end module cal_line_filtering_vector
