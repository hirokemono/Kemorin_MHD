!
!     module cal_for_ffs
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine cal_ff_2_field(numnod, inod_smp_stack, numdir,       &
!!     &         ff, ml, ncomp_nod, i_fld, d_nod)
!!      subroutine cal_ff_2_vector(numnod, inod_smp_stack, ff, ml,      &
!!     &          ncomp_nod, i_fld, d_nod)
!!      subroutine cal_ff_2_scalar(numnod, inod_smp_stack, ff, ml,      &
!!     &          ncomp_nod, i_fld, d_nod)
!!
!!      subroutine cal_ff_smp_2_multi_pass(maxnod_4_smp, inod_smp_stack,&
!!     &          numdir, ff_smp, ff_m_smp)
!!      subroutine cal_multi_pass_2_ff_smp(maxnod_4_smp, inod_smp_stack,&
!!     &          numdir, ff_nl_smp, ff_m_smp)
!
      module cal_for_ffs
!
      use m_precision
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
      subroutine cal_ff_2_field(numnod, inod_smp_stack, numdir,         &
     &         ff, ml, ncomp_nod, i_fld, d_nod)
!
      integer(kind=kint), intent(in) :: numnod, numdir
      integer(kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: ff(numnod,numdir) 
      real(kind=kreal), intent(in) :: ml(numnod)
!
      integer(kind=kint), intent(in) :: ncomp_nod, i_fld
      real(kind=kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
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
            d_nod(inod,i_fld+nd-1) = ff(inod,nd) * ml(inod)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_ff_2_field
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_2_vector(numnod, inod_smp_stack, ff, ml,        &
     &          ncomp_nod, i_fld, d_nod)
!
      integer(kind=kint), intent(in) :: numnod
      integer(kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: ff(numnod,3) 
      real(kind=kreal), intent(in) :: ml(numnod)
!
      integer(kind=kint), intent(in) :: ncomp_nod, i_fld
      real(kind=kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
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
          d_nod(inod,i_fld  ) = ff(inod,1) * ml(inod)
          d_nod(inod,i_fld+1) = ff(inod,2) * ml(inod)
          d_nod(inod,i_fld+2) = ff(inod,3) * ml(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_ff_2_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_2_scalar(numnod, inod_smp_stack, ff, ml,        &
     &          ncomp_nod, i_fld, d_nod)
!
      integer(kind=kint), intent(in) :: numnod
      integer(kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: ff(numnod,1) 
      real(kind=kreal), intent(in) :: ml(numnod)
!
      integer(kind=kint), intent(in) :: ncomp_nod, i_fld
      real(kind=kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
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
          d_nod(inod,i_fld) = ff(inod,1) * ml(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_ff_2_scalar
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_multi_pass(maxnod_4_smp, inod_smp_stack,  &
     &         numdir, ff_smp, ff_m_smp)
!
      integer(kind=kint), intent(in) :: maxnod_4_smp, numdir
      integer(kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
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
      subroutine cal_multi_pass_2_ff_smp(maxnod_4_smp, inod_smp_stack,  &
     &         numdir, ff_nl_smp, ff_m_smp)
!
      integer(kind=kint), intent(in) :: maxnod_4_smp, numdir
      integer(kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
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
