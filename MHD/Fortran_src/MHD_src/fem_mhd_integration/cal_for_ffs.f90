!
!     module cal_for_ffs
!
!     Written by H. Matsui on June, 2005
!
!      subroutine cal_ff_2_field (numdir, vector, ff, ml)
!      subroutine cal_ff_2_vector (vector, ff, ml)
!      subroutine cal_ff_2_scalar (scalar, ff, ml)
!      subroutine cal_ff_smp_2_multi_pass (numdir, ff_smp, ff_m_smp)
!      subroutine cal_multi_pass_2_ff_smp (numdir, ff_nl_smp, ff_m_smp)
!
      module cal_for_ffs
!
      use m_precision
!
      use m_geometry_parameter
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_2_field (numdir, vector, ff, ml)
!
      integer(kind=kint), intent(in) :: numdir
!
      real(kind=kreal), intent(in) :: ff(numnod,numdir) 
      real(kind=kreal), intent(in) :: ml(numnod)
      real(kind=kreal), intent(inout) :: vector(numnod,numdir)
!
      integer(kind = kint) :: iproc, inod, nd
      integer(kind = kint) :: istart, iend
!
!
!$omp parallel do private(istart,iend,nd,inod)
      do iproc = 1, np_smp
        istart = inod_smp_stack(iproc-1) + 1
        iend   = inod_smp_stack(iproc)
        do nd = 1, numdir
!cdir nodep
          do inod = istart, iend
            vector(inod,nd) = ff(inod,nd) * ml(inod)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_ff_2_field
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_2_vector (vector, ff, ml)
!
      real(kind=kreal), intent(in) :: ff(numnod,3) 
      real(kind=kreal), intent(in) :: ml(numnod)
      real(kind=kreal), intent(inout) :: vector(numnod,3)
!
      integer(kind = kint) :: iproc, inod
      integer(kind = kint) :: istart, iend
!
!
!$omp parallel do private(istart,iend,inod)
      do iproc = 1, np_smp
       istart = inod_smp_stack(iproc-1) + 1
       iend   = inod_smp_stack(iproc)
!cdir nodep
        do inod = istart, iend
          vector(inod,1) = ff(inod,1) * ml(inod)
          vector(inod,2) = ff(inod,2) * ml(inod)
          vector(inod,3) = ff(inod,3) * ml(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_ff_2_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_2_scalar (scalar, ff, ml)
!
      real(kind=kreal), intent(in) :: ff(numnod,1) 
      real(kind=kreal), intent(in) :: ml(numnod)
      real(kind=kreal), intent(inout) :: scalar(numnod)
!
      integer(kind = kint) :: iproc, inod, nd
      integer(kind = kint) :: istart, iend
!
!
!$omp parallel do private(istart,iend,nd,inod)
      do iproc = 1, np_smp
        istart = inod_smp_stack(iproc-1) + 1
        iend   = inod_smp_stack(iproc)
!cdir nodep
        do inod = istart, iend
          scalar(inod) = ff(inod,1) * ml(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_ff_2_scalar
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_multi_pass (numdir, ff_smp, ff_m_smp)
!
      integer(kind=kint), intent(in) :: numdir
!
      real(kind=kreal), intent(in)    :: ff_smp(maxnod_4_smp,3,np_smp)
      real(kind=kreal), intent(inout) :: ff_m_smp(maxnod_4_smp,3,np_smp)
!
      integer(kind = kint) :: iproc, inod, nd
!
!$omp parallel do private(nd,inod)
       do iproc = 1, np_smp
         do nd = 1, numdir
!
!cdir nodep
           do inod = 1, inod_smp_stack(iproc)-inod_smp_stack(iproc-1)
             ff_m_smp(inod,nd,iproc) = ff_smp(inod,nd,iproc)
           end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_ff_smp_2_multi_pass
!
! ----------------------------------------------------------------------
!
      subroutine cal_multi_pass_2_ff_smp (numdir, ff_nl_smp, ff_m_smp)
!
      integer(kind=kint), intent(in) :: numdir
!
      real(kind=kreal),intent(inout) :: ff_nl_smp(maxnod_4_smp,3,np_smp)
      real(kind=kreal),intent(in) :: ff_m_smp(maxnod_4_smp,3,np_smp)
!
      integer(kind = kint) :: iproc, inod, nd
!
!$omp parallel do private(nd,inod)
       do iproc = 1, np_smp
         do nd = 1, numdir
!
!cdir nodep
          do inod = 1, inod_smp_stack(iproc)-inod_smp_stack(iproc-1)
            ff_nl_smp(inod,nd,iproc) = ff_nl_smp(inod,nd,iproc)         &
     &             + ff_m_smp(inod,nd,iproc)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_multi_pass_2_ff_smp
!
! ----------------------------------------------------------------------
!
      end module cal_for_ffs
