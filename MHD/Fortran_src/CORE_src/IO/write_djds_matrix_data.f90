!>@file   write_djds_matrix_data.f90
!!@brief  module write_djds_matrix_data
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Routines to output DJDS matrix data
!!
!!@verbatim
!!      subroutine write_djds_mat11_comp                                &
!!     &         ( id_file, N, NP, NLmax, NUmax, itotal_l, itotal_u,    &
!!     &           npLX1, npUX1, NHYP, PEsmpTOT, NEWtoOLD,              &
!!     &           indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,    &
!!     &           DMAT11_IO)
!!      subroutine write_djds_mat33_comp                                &
!!     &         ( id_file, N, NP, NLmax, NUmax, itotal_l, itotal_u,    &
!!     &           npLX1, npUX1, NHYP, PEsmpTOT, NEWtoOLD,              &
!!     &           indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,    &
!!     &           DMAT33_IO)
!!      subroutine write_djds_mat_connects                              &
!!     &         ( id_file, NP, NHYP, PEsmpTOT, STACKmcG, STACKmc,      &
!!     &           NLmaxHYP, NUmaxHYP, IVECT, OLDtoNEW_DJDS_L,          &
!!     &           OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U, LtoU,              &
!!     &           NEIBPETOT, STACK_EXPORT, NOD_EXPORT_NEW)
!!@endverbatim
!
      module write_djds_matrix_data
!
      use m_precision
!
      use m_machine_parameter
      use t_solver_djds
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
     &           indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,      &
     &           DMAT11_IO)
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
!
      type(DJDS_MATRIX), intent(in) :: DMAT11_IO
!
      integer(kind = kint) :: i, j
!
!
      write(id_file,'(a)') '! i, indexDJDS_L(i)'
      write(id_file,'(3i16)')  NLmax*NHYP*PEsmpTOT, NLmax, npLX1
      do i = 1, NLmax*NHYP*PEsmpTOT
        write(id_file,'(8i16)') i, indexDJDS_L(i)
      end do
!
      write(id_file,'(a)') '! i, indexDJDS_U(i)'
      write(id_file,'(3i16)')  NUmax*NHYP*PEsmpTOT, NUmax, npUX1
      do i = 1, NUmax*NHYP*PEsmpTOT
        write(id_file,'(8i16)') i, indexDJDS_U(i)
      end do
!
      write(id_file,'(a)') '! i, NEWtoOLD(i), D(i)'
      write(id_file,'(2i16)') NP, DMAT11_IO%num_non0
      do i = 1, NP
        write(id_file,'(2i16,1pE25.15e3)') i, NEWtoOLD(i),              &
     &                DMAT11_IO%aiccg(i)
      end do
!
      write(id_file,'(a)') '! i, itemDJDS_L(i), AL(i)'
      write(id_file,'(i16)') itotal_l
      do i = 1, itotal_l
        j = i + DMAT11_IO%istart_l - 1
        write(id_file,'(2i16,1pE25.15e3)') i, itemDJDS_L(i),            &
     &                DMAT11_IO%aiccg(j)
      end do
!
      write(id_file,'(a)') '! i, itemDJDS_U(i), AU(i)'
      write(id_file,'(i16)') itotal_u
      do i = 1, itotal_u
        j = i + DMAT11_IO%istart_u - 1
        write(id_file,'(2i16,1pE25.15e3)') i, itemDJDS_U(i),            &
     &                DMAT11_IO%aiccg(j)
      end do
!
      write(id_file,'(a)') '! i, ALUG_L(i), ALUG_U(i)'
      write(id_file,'(i16)') N
      do i = 1, N
        write(id_file,'(i16,1p2E25.15e3)')                              &
     &              i, DMAT11_IO%ALUG_L(i), DMAT11_IO%ALUG_U(i)
      end do
!
      end subroutine write_djds_mat11_comp
!
!  ---------------------------------------------------------------------
!
      subroutine write_djds_mat33_comp                                  &
     &         ( id_file, N, NP, NLmax, NUmax, itotal_l, itotal_u,      &
     &           npLX1, npUX1, NHYP, PEsmpTOT, NEWtoOLD,                &
     &           indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,      &
     &           DMAT33_IO)
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
!
      type(DJDS_MATRIX), intent(in) :: DMAT33_IO
!
      integer(kind = kint) :: i, j
!
!
      write(id_file,'(a)') '! i, indexDJDS_L(i)'
      write(id_file,'(3i16)')  NLmax*NHYP*PEsmpTOT, NLmax, npLX1
      do i = 1, NLmax*NHYP*PEsmpTOT
        write(id_file,'(8i16)') i, indexDJDS_L(i)
      end do
!
      write(id_file,'(a)') '! i, indexDJDS_U(i)'
      write(id_file,'(3i16)')  NUmax*NHYP*PEsmpTOT, NUmax, npUX1
      do i = 1, NUmax*NHYP*PEsmpTOT
        write(id_file,'(8i16)') i, indexDJDS_U(i)
      end do
!
      write(id_file,'(a)') '! i, NEWtoOLD(i), D(i)'
      write(id_file,'(2i16)') NP, DMAT33_IO%num_non0
      do i = 1, NP
        write(id_file,'(2i16,1p9E25.15e3)') i, NEWtoOLD(i),             &
     &        DMAT33_IO%aiccg(9:i-8:9*i)
      end do
!
      write(id_file,'(a)') '! i, itemDJDS_L(i), AL(i)'
      write(id_file,'(i16)') itotal_l
      do i = 1, itotal_l
        j = 9*(i-1) + DMAT33_IO%istart_l - 1
        write(id_file,'(2i16,1p9E25.15e3)') i, itemDJDS_L(i),           &
     &       DMAT33_IO%aiccg(j+1:j+9)
      end do
!
      write(id_file,'(a)') '! i, itemDJDS_U(i), AU(i)'
      write(id_file,'(i16)') itotal_u
      do i = 1, itotal_u
        j = 9*(i-1) + DMAT33_IO%istart_u - 1
        write(id_file,'(2i16,1p9E25.15e3)') i, itemDJDS_U(i),           &
     &       DMAT33_IO%aiccg(j+1:j+9)
      end do
!
      write(id_file,'(a)') '! i, ALUG_L(i), ALUG_U(i)'
      write(id_file,'(i16)') N
      do i = 1, N
        write(id_file,'(i16,1p18E25.15e3)')                             &
     &     i, DMAT33_IO%ALUG_L(9:i-8:9*i), DMAT33_IO%ALUG_U(9:i-8:9*i)
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
      write(id_file,'(i16)')  PEsmpTOT
      do i = 1, PEsmpTOT
        write(id_file,'(8i16)') i, STACKmcG(i)
      end do
!
      write(id_file,'(a)') '! i, IVECT(i), NLmaxHYP(i), NUmaxHYP(i)'
      write(id_file,'(i16)') NHYP
      do i = 1, NHYP
        write(id_file,'(8i16)') i, IVECT(i), NLmaxHYP(i), NUmaxHYP(i)
      end do
!
      write(id_file,'(a)') '! i, STACKmc(i)'
      write(id_file,'(i16)')  PEsmpTOT*NHYP
      do i = 1, PEsmpTOT*NHYP
        write(id_file,'(8i16)') i, STACKmc(i)
      end do
!
      write(id_file,'(2a)') '!  i, NEWtoOLD_DJDS_U(i), ',               &
     &    'OLDtoNEW_DJDS_L(i), OLDtoNEW_DJDS_U(i), LtoU(i)'
      write(id_file,'(i16)') NP
      do i = 1, NP
        write(id_file,'(8i16)') i, NEWtoOLD_DJDS_U(i),                  &
     &           OLDtoNEW_DJDS_L(i),  OLDtoNEW_DJDS_U(i), LtoU(i)
      end do
!
      write(id_file,'(a)') '! EXPORT'
      write(id_file,'(8i16)')  STACK_EXPORT(1:NEIBPETOT)
      do i = 1, STACK_EXPORT(NEIBPETOT)
        write(id_file,'(2i16)') i, NOD_EXPORT_NEW(i)
      end do
!
      end subroutine write_djds_mat_connects
!
!  ---------------------------------------------------------------------
!
      end module write_djds_matrix_data
