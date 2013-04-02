!precond_block_ilu_33.f90
!      module precond_block_ilu_33
!
!      Written by K. Nakajima in 2001
!      Modified by H. Matsui on Jan., 2006
!
!      subroutine precond_bl_ilu_33(N, NP, PEsmpTOT, STACKmcG,          &
!     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, sigma_diag)
!
      module precond_block_ilu_33
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
      subroutine precond_bl_ilu_33(N, NP, PEsmpTOT, STACKmcG,           &
     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, sigma_diag)
!
      integer(kind=kint), intent(in) :: N
!       number of internal nodes (exclude external node)
      integer(kind=kint), intent(in) :: NP
!       number of nodes          (include external node)
      integer(kind=kint), intent(in) :: PEsmpTOT
      integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
      integer(kind=kint), intent(in) :: OtoN_L(NP)
      integer(kind=kint), intent(in) :: OtoN_U(NP)
!
      real(kind=kreal), intent(in) :: sigma_diag
      real(kind=kreal), intent(in) :: D(9*NP)
!
      real(kind=kreal), intent(inout) :: ALU_L(9*N)
      real(kind=kreal), intent(inout) :: ALU_U(9*N)
!
      integer(kind=kint) :: ip, ii, in0L, in0U
      real(kind=kreal) :: ALUG3(9*N)
!
!  preconditiong by iLU
!
!
!C== block LU
!

      ALUG3  = 0.d0
      ALU_U= 0.d0
      ALU_L= 0.d0

!cdir parallel do private(ii,in0L,in0U)
!$omp parallel do private(ii,in0L,in0U)
      do ip = 1, PEsmpTOT
!cdir nodep
       do ii= STACKmcG(ip-1)+1, STACKmcG(ip)
        in0L= OtoN_L(ii)
        in0U= OtoN_U(ii)
!
        ALUG3(9*ii-8)= D(9*ii-8)*SIGMA_DIAG
        ALUG3(9*ii-7)= D(9*ii-7)
        ALUG3(9*ii-6)= D(9*ii-6)
        ALUG3(9*ii-5)= D(9*ii-5)
        ALUG3(9*ii-4)= D(9*ii-4)*SIGMA_DIAG
        ALUG3(9*ii-3)= D(9*ii-3)
        ALUG3(9*ii-2)= D(9*ii-2)
        ALUG3(9*ii-1)= D(9*ii-1)
        ALUG3(9*ii  )= D(9*ii  )*SIGMA_DIAG
!
        ALUG3(9*ii-8)= 1.d0/ALUG3(9*ii-8)
!        ALUG3(9*ii-7)= ALUG3(9*ii-7)
!        ALUG3(9*ii-6)= ALUG3(9*ii-6)
!
        ALUG3(9*ii-5)= ALUG3(9*ii-5) * ALUG3(9*ii-8)
        ALUG3(9*ii-4)= 1.d0                                             &
     &             / ( ALUG3(9*ii-4) - ALUG3(9*ii-5)*ALUG3(9*ii-7) )
        ALUG3(9*ii-3)= ALUG3(9*ii-3) - ALUG3(9*ii-5)*ALUG3(9*ii-6)
!
        ALUG3(9*ii-2)=   ALUG3(9*ii-2) * ALUG3(9*ii-8)
        ALUG3(9*ii-1)= ( ALUG3(9*ii-1) - ALUG3(9*ii-2)*ALUG3(9*ii-7) )  &
     &             * ALUG3(9*ii-4)
        ALUG3(9*ii  )= 1.d0 / ( ALUG3(9*ii  )                           &
     &                        - ALUG3(9*ii-1)*ALUG3(9*ii-3)             &
     &                        - ALUG3(9*ii-2)*ALUG3(9*ii-6) )
!
        ALU_L(9*in0L-8)= ALUG3(9*ii-8)
        ALU_L(9*in0L-7)= ALUG3(9*ii-7)
        ALU_L(9*in0L-6)= ALUG3(9*ii-6)
        ALU_L(9*in0L-5)= ALUG3(9*ii-5)
        ALU_L(9*in0L-4)= ALUG3(9*ii-4)
        ALU_L(9*in0L-3)= ALUG3(9*ii-3)
        ALU_L(9*in0L-2)= ALUG3(9*ii-2)
        ALU_L(9*in0L-1)= ALUG3(9*ii-1)
        ALU_L(9*in0L  )= ALUG3(9*ii  )
!
        ALU_U(9*in0U-8)= ALUG3(9*ii-8)
        ALU_U(9*in0U-7)= ALUG3(9*ii-7)
        ALU_U(9*in0U-6)= ALUG3(9*ii-6)
        ALU_U(9*in0U-5)= ALUG3(9*ii-5)
        ALU_U(9*in0U-4)= ALUG3(9*ii-4)
        ALU_U(9*in0U-3)= ALUG3(9*ii-3)
        ALU_U(9*in0U-2)= ALUG3(9*ii-2)
        ALU_U(9*in0U-1)= ALUG3(9*ii-1)
        ALU_U(9*in0U  )= ALUG3(9*ii  )
!
       end do
      end do
!$omp end parallel do
!
!
      end subroutine precond_bl_ilu_33
!
! ----------------------------------------------------------------------
!
      end module precond_block_ilu_33
