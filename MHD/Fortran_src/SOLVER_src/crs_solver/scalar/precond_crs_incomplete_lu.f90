!precond_crs_incomplete_lu.f90
!      module precond_crs_incomplete_lu
!
!      Written by K. Nakajima in 2001
!      Modified by H. Matsui on Jan., 2006
!
!      subroutine precond_crs_ilu(N, NP, NPL, NPU, D, AL, INL,IAL,      &
!     &         INU, AU, DD, SIGMA, SIGMA_DIAG)
!      subroutine precond_crs_ssor(N, NP, D, DD, SIGMA_DIAG)
!
!      subroutine mat_scaling_crs_ilu(N, NP, NPL, NPU, D, AL, INL, IAL, &
!     &         INU, IAU, AU, SCALE)
!
!
      module precond_crs_incomplete_lu
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine precond_crs_ilu(N, NP, NPL, NPU, D, AL, INL,IAL,       &
     &         INU, AU, DD, SIGMA, SIGMA_DIAG)
!
      integer(kind=kint), intent(in) :: N, NP
      integer(kind=kint), intent(in) :: NPL, NPU
!
      integer(kind=kint), intent(in) :: IAL(NPL)
      integer(kind=kint), intent(in) :: INL(0:NP)
      integer(kind=kint), intent(in) :: INU(0:NP)
!
      real(kind=kreal), intent(in) :: AL(NPL)
      real(kind=kreal), intent(in) :: AU(NPU)
      real(kind=kreal), intent(in) :: D(NP)
      real(kind=kreal), intent(in) :: SIGMA, SIGMA_DIAG
!
      real(kind=kreal), intent(inout) :: DD(NP)
!
      integer(kind=kint) :: i, k, kk, id
      integer(kind=kint) :: isU, isL, ieU, ieL
      real(kind=kreal) :: SS
!
!
        do i= 1, N
          isL= INL(i-1) + 1
          ieL= INL(i)
          DD(i) = D(i) * SIGMA_DIAG

          do k= isL, ieL
            SS=  AL(k)
            id= IAL(k)

            isU= INU(id-1) + 1
            ieU= INU(id)

            do kk= isU, ieU
              SS = SS + AU(kk) * SIGMA
            enddo
            DD(i) = DD(i) - AL(k)*SS*DD(id)
          enddo
          DD(i) = 1.d0 / DD(i)
        enddo
!
        DD(N+1:NP)= 0.d0
!
      end subroutine precond_crs_ilu
!
! ----------------------------------------------------------------------
!
      subroutine precond_crs_ssor(N, NP, D, DD, SIGMA_DIAG)
!
      integer(kind=kint), intent(in) :: N, NP
!
      real(kind=kreal), intent(in) :: D(NP)
      real(kind=kreal), intent(in) :: SIGMA_DIAG
!
      real(kind=kreal), intent(inout) :: DD(NP)
!
!
        DD(1:N)= 1.d0 / (SIGMA_DIAG*D(1:N))
        DD(N+1:NP)= 0.d0
!
      end subroutine precond_crs_ssor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine mat_scaling_crs_ilu(N, NP, NPL, NPU, D, AL, INL, IAL,  &
     &         INU, IAU, AU, SCALE)
!
      integer(kind=kint), intent(in) :: N, NP
      integer(kind=kint), intent(in) :: NPL, NPU
!
      integer(kind=kint), intent(in) :: IAL(NPL)
      integer(kind=kint), intent(in) :: IAU(NPU)
      integer(kind=kint), intent(in) :: INL(0:NP)
      integer(kind=kint), intent(in) :: INU(0:NP)
!
      real(kind=kreal), intent(inout) :: AL(NPL)
      real(kind=kreal), intent(inout) :: AU(NPU)
      real(kind=kreal), intent(inout) :: D(NP)
      real(kind=kreal), intent(inout) :: SCALE(NP)
!
      integer(kind=kint) :: i, k, kk
      integer(kind=kint) :: isU, isL, ieU, ieL
!
!
      do i= 1, N
        D(i)= dabs(D(i))/D(i)
        isU= INU(i-1) + 1
        ieU= INU(i  ) 
        do k= isU, ieU
          kk= IAU(k)
          AU(k)= AU(k)*SCALE(kk)*SCALE(i)
        enddo
!
        isL= INL(i-1) + 1
        ieL= INL(i  ) 
        do k= isL, ieL
          kk= IAL(k)
          AL(k)= AL(k)*SCALE(kk)*SCALE(i)
        enddo
      enddo
!
      end subroutine mat_scaling_crs_ilu
!
! ----------------------------------------------------------------------
!
      end module precond_crs_incomplete_lu
