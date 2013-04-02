!write_djds_matrix_data.f90
!      module write_djds_matrix_data
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine write_djds_mat11_comp                                 &
!     &         ( id_file, N, NP, NLmax, NUmax, itotal_l, itotal_u,     &
!     &           npLX1, npUX1, NHYP, PEsmpTOT, NEWtoOLD,               &
!     &           D, indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,  &
!     &           AL, AU, ALUG_L, ALUG_U)
!      subroutine write_djds_mat33_comp                                 &
!     &         ( id_file, N, NP, NLmax, NUmax, itotal_l, itotal_u,     &
!     &           npLX1, npUX1, NHYP, PEsmpTOT, NEWtoOLD,               &
!     &           D, indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,  &
!     &           AL, AU, ALUG_L, ALUG_U)
!      subroutine write_djds_mat_connects                               &
!     &         ( id_file, NP, NHYP, PEsmpTOT, STACKmcG, STACKmc,       &
!     &           NLmaxHYP, NUmaxHYP, IVECT, OLDtoNEW_DJDS_L,           &
!     &           OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U, LtoU,               &
!     &           NEIBPETOT, STACK_EXPORT, NOD_EXPORT_NEW)
!
      module write_djds_matrix_data
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_djds_mat11_comp                                  &
     &         ( id_file, N, NP, NLmax, NUmax, itotal_l, itotal_u,      &
     &           npLX1, npUX1, NHYP, PEsmpTOT, NEWtoOLD,                &
     &           D, indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,   &
     &           AL, AU, ALUG_L, ALUG_U)
!
      integer(kind=kint ), intent(in) :: id_file
!
      integer(kind=kint ), intent(in) :: N, NP, NLmax, NUmax
      integer(kind=kint ), intent(in) ::itotal_l, itotal_u, NHYP
      integer(kind=kint ), intent(in) :: npLX1, npUX1
      integer(kind=kint ), intent(in) :: PEsmpTOT

      integer(kind=kint), intent(in) :: NEWtoOLD(NP)

      integer(kind=kint), intent(in)                                    &
     &                    :: indexDJDS_L(0:NLmax*NHYP*PEsmpTOT)
      integer(kind=kint), intent(in)                                    &
     &                    :: indexDJDS_U(0:NUmax*NHYP*PEsmpTOT)
      integer(kind=kint), intent(in) :: itemDJDS_L(itotal_l)
      integer(kind=kint), intent(in) :: itemDJDS_U(itotal_u)

      real(kind=kreal), intent(in) :: D(NP )
      real(kind=kreal), intent(in) :: AL(itotal_l)
      real(kind=kreal), intent(in) :: AU(itotal_u)

      real(kind=kreal), intent(in) :: ALUG_L(N), ALUG_U(N)
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)') '! i, indexDJDS_L(i)'
      write(id_file,'(3i10)')  NLmax*NHYP*PEsmpTOT, NLmax, npLX1
      do i = 1, NLmax*NHYP*PEsmpTOT
        write(id_file,'(8i10)') i, indexDJDS_L(i)
      end do
!
      write(id_file,'(a)') '! i, indexDJDS_U(i)'
      write(id_file,'(3i10)')  NUmax*NHYP*PEsmpTOT, NUmax, npUX1
      do i = 1, NUmax*NHYP*PEsmpTOT
        write(id_file,'(8i10)') i, indexDJDS_U(i)
      end do
!
      write(id_file,'(a)') '! i, NEWtoOLD(i), D(i)'
      write(id_file,'(i10)') NP
      do i = 1, NP
        write(id_file,'(2i10,1pE25.15e3)') i, NEWtoOLD(i), D(i)
      end do
!
      write(id_file,'(a)') '! i, itemDJDS_L(i), AL(i)'
      write(id_file,'(i10)') itotal_l
      do i = 1, itotal_l
        write(id_file,'(2i10,1pE25.15e3)') i, itemDJDS_L(i), AL(i)
      end do
!
      write(id_file,'(a)') '! i, itemDJDS_U(i), AU(i)'
      write(id_file,'(i10)') itotal_u
      do i = 1, itotal_u
        write(id_file,'(2i10,1pE25.15e3)') i, itemDJDS_U(i), AU(i)
      end do
!
      write(id_file,'(a)') '! i, ALUG_L(i), ALUG_U(i)'
      write(id_file,'(i10)') N
      do i = 1, N
        write(id_file,'(i10,1p2E25.15e3)') i, ALUG_L(i), ALUG_U(i)
      end do
!
      end subroutine write_djds_mat11_comp
!
!  ---------------------------------------------------------------------
!
      subroutine write_djds_mat33_comp                                  &
     &         ( id_file, N, NP, NLmax, NUmax, itotal_l, itotal_u,      &
     &           npLX1, npUX1, NHYP, PEsmpTOT, NEWtoOLD,                &
     &           D, indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,   &
     &           AL, AU, ALUG_L, ALUG_U)
!
!
      integer(kind=kint ), intent(in) :: id_file
!
      integer(kind=kint ), intent(in) :: N, NP, NLmax, NUmax
      integer(kind=kint ), intent(in) :: itotal_l, itotal_u, NHYP
      integer(kind=kint ), intent(in) :: npLX1, npUX1
      integer(kind=kint ), intent(in) :: PEsmpTOT

      integer(kind=kint), intent(in) :: NEWtoOLD(NP)

      integer(kind=kint), intent(in)                                    &
     &                    :: indexDJDS_L(0:NLmax*NHYP*PEsmpTOT)
      integer(kind=kint), intent(in)                                    &
     &                    :: indexDJDS_U(0:NUmax*NHYP*PEsmpTOT)
      integer(kind=kint), intent(in) :: itemDJDS_L(itotal_l)
      integer(kind=kint), intent(in) :: itemDJDS_U(itotal_u)

      real(kind=kreal), intent(in) :: D(9*NP )
      real(kind=kreal), intent(in) :: AL(9*itotal_l)
      real(kind=kreal), intent(in) :: AU(9*itotal_u)

      real(kind=kreal), intent(in) :: ALUG_L(9*N), ALUG_U(9*N)
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)') '! i, indexDJDS_L(i)'
      write(id_file,'(3i10)')  NLmax*NHYP*PEsmpTOT, NLmax, npLX1
      do i = 1, NLmax*NHYP*PEsmpTOT
        write(id_file,'(8i10)') i, indexDJDS_L(i)
      end do
!
      write(id_file,'(a)') '! i, indexDJDS_U(i)'
      write(id_file,'(3i10)')  NUmax*NHYP*PEsmpTOT, NUmax, npUX1
      do i = 1, NUmax*NHYP*PEsmpTOT
        write(id_file,'(8i10)') i, indexDJDS_U(i)
      end do
!
      write(id_file,'(a)') '! i, NEWtoOLD(i), D(i)'
      write(id_file,'(i10)') NP
      do i = 1, NP
        write(id_file,'(2i10,1p9E25.15e3)') i, NEWtoOLD(i), D(9:i-8:9*i)
      end do
!
      write(id_file,'(a)') '! i, itemDJDS_L(i), AL(i)'
      write(id_file,'(i10)') itotal_l
      do i = 1, itotal_l
        write(id_file,'(2i10,1p9E25.15e3)') i, itemDJDS_L(i),           &
     &       AL(9:i-8:9*i)
      end do
!
      write(id_file,'(a)') '! i, itemDJDS_U(i), AU(i)'
      write(id_file,'(i10)') itotal_u
      do i = 1, itotal_u
        write(id_file,'(2i10,1p9E25.15e3)') i, itemDJDS_U(i),           &
     &       AU(9:i-8:9*i)
      end do
!
      write(id_file,'(a)') '! i, ALUG_L(i), ALUG_U(i)'
      write(id_file,'(i10)') N
      do i = 1, N
        write(id_file,'(i10,1p18E25.15e3)') i, ALUG_L(9:i-8:9*i),       &
     &                                      ALUG_U(9:i-8:9*i)
      end do
!
      end subroutine write_djds_mat33_comp
!
!  ---------------------------------------------------------------------
!
      subroutine write_djds_mat_connects                                &
     &         ( id_file, NP, NHYP, PEsmpTOT, STACKmcG, STACKmc,        &
     &           NLmaxHYP, NUmaxHYP, IVECT, OLDtoNEW_DJDS_L,            &
     &           OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U, LtoU,                &
     &           NEIBPETOT, STACK_EXPORT, NOD_EXPORT_NEW)
!
      integer(kind=kint ), intent(in) :: id_file
!
      integer(kind=kint ), intent(in) :: NP, NHYP
      integer(kind=kint ), intent(in) :: PEsmpTOT
      integer(kind=kint ), intent(in) :: NEIBPETOT

      integer(kind=kint), intent(in) :: OLDtoNEW_DJDS_L(NP)
      integer(kind=kint), intent(in) :: NEWtoOLD_DJDS_U(NP)
      integer(kind=kint), intent(in) :: LtoU(NP)
      integer(kind=kint), intent(in) :: OLDtoNEW_DJDS_U(NP)
      integer(kind=kint), intent(in) :: IVECT(0:NHYP)
      integer(kind=kint), intent(in) :: NLmaxHYP(NHYP), NUmaxHYP(NHYP)

      integer(kind=kint), intent(in) :: STACKmc(0:PEsmpTOT*NHYP)
      integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &      :: NOD_EXPORT_NEW(STACK_EXPORT(NEIBPETOT)) 
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)') '! i, STACKmcG(i)'
      write(id_file,'(i10)')  PEsmpTOT
      do i = 1, PEsmpTOT
        write(id_file,'(8i10)') i, STACKmcG(i)
      end do
!
      write(id_file,'(a)') '! i, IVECT(i), NLmaxHYP(i), NUmaxHYP(i)'
      write(id_file,'(i10)') NHYP
      do i = 1, NHYP
        write(id_file,'(8i10)') i, IVECT(i), NLmaxHYP(i), NUmaxHYP(i)
      end do
!
      write(id_file,'(a)') '! i, STACKmc(i)'
      write(id_file,'(i10)')  PEsmpTOT*NHYP
      do i = 1, PEsmpTOT*NHYP
        write(id_file,'(8i10)') i, STACKmc(i)
      end do
!
      write(id_file,'(2a)') '!  i, NEWtoOLD_DJDS_U(i), ',               &
     &    'OLDtoNEW_DJDS_L(i), OLDtoNEW_DJDS_U(i), LtoU(i)'
      write(id_file,'(i10)') NP
      do i = 1, NP
        write(id_file,'(8i10)') i, NEWtoOLD_DJDS_U(i),                  &
     &           OLDtoNEW_DJDS_L(i),  OLDtoNEW_DJDS_U(i), LtoU(i)
      end do
!
      write(id_file,'(a)') '! EXPORT'
      write(id_file,'(8i10)')  STACK_EXPORT(1:NEIBPETOT)
      do i = 1, STACK_EXPORT(NEIBPETOT)
        write(id_file,'(2i10)') i, NOD_EXPORT_NEW(i)
      end do
!
      end subroutine write_djds_mat_connects
!
!  ---------------------------------------------------------------------
!
      end module write_djds_matrix_data
