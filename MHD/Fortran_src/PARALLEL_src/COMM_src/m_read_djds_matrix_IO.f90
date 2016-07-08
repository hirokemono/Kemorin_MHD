!>@file  m_read_djds_matrix_IO.f90
!!@brief      module m_read_djds_matrix_IO
!!
!!@author  H. Matsui
!!@date Written in Sep., 2006
!
!>@brief IO routines for DJDS matrix
!!
!!@verbatim
!!      subroutine deallocate_djds_mat11_comp_IO
!!      subroutine deallocate_djds_mat33_cmop_IO
!!      subroutine deallocate_djds_mat_connects_IO
!!
!!      subroutine read_djds_mat11_comp(id_file)
!!      subroutine read_djds_mat33_comp(id_file)
!!      subroutine read_djds_mat_connects(id_file)
!!@endverbatim
!
      module m_read_djds_matrix_IO
!
      use m_precision
      use t_comm_table
      use t_solver_djds
!
      use skip_comment_f
!
      implicit none
!
!
      type(DJDS_ordering_table), save :: DJDS_IO
!!DJDS_IO%NLmax
      type(DJDS_MATRIX), save :: DMAT11_IO
!DMAT11_IO%D
      type(DJDS_MATRIX), save :: DMAT33_IO
!
!C-- Ordering arrays
!
      integer(kind=kint) :: N_IO, NP_IO, PEsmpTOT_IO
!
!    coefs for ordering!
      integer(kind=kint) :: NTOT_EXPORT_IO
!
      character(len=255), private :: character_4_read
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_djds_mat11_comp_IO
!
      deallocate(DJDS_IO%indexDJDS_L, DJDS_IO%indexDJDS_U)
!
      deallocate (DMAT11_IO%D, DJDS_IO%NEWtoOLD)
      deallocate (DMAT11_IO%AL, DJDS_IO%itemDJDS_L)
      deallocate (DMAT11_IO%AU, DJDS_IO%itemDJDS_U)
!
      deallocate (DMAT11_IO%ALUG_L, DMAT11_IO%ALUG_U)
!
      end subroutine deallocate_djds_mat11_comp_IO
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_djds_mat33_cmop_IO
!
!
      deallocate(DJDS_IO%indexDJDS_L, DJDS_IO%indexDJDS_U)
!
      deallocate (DMAT33_IO%D, DJDS_IO%NEWtoOLD)
      deallocate (DMAT33_IO%AL, DJDS_IO%itemDJDS_L)
      deallocate (DMAT33_IO%AU, DJDS_IO%itemDJDS_U)
!
      deallocate (DMAT33_IO%ALUG_L, DMAT33_IO%ALUG_U)
!
      end subroutine deallocate_djds_mat33_cmop_IO
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_djds_mat_connects_IO
!
!
      deallocate (DJDS_IO%STACKmcG, DJDS_IO%STACKmc)
      deallocate (DJDS_IO%IVECT, DJDS_IO%NLmaxHYP, DJDS_IO%NUmaxHYP)
!
      deallocate (DJDS_IO%NEWtoOLD_DJDS_U, DJDS_IO%OLDtoNEW_DJDS_L)
      deallocate (DJDS_IO%OLDtoNEW_DJDS_U, DJDS_IO%LtoU)
!
      deallocate (DJDS_IO%NOD_EXPORT_NEW)
!
      end subroutine deallocate_djds_mat_connects_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_djds_mat11_comp(id_file)
!
      integer(kind=kint ), intent(in) :: id_file
      integer(kind = kint) :: i, itmp, num
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itmp,  DJDS_IO%NLmax, DJDS_IO%npLX1
!
      num = PEsmpTOT_IO * DJDS_IO%NLmax * DJDS_IO%NHYP
      allocate(DJDS_IO%indexDJDS_L(0:num))
      DJDS_IO%indexDJDS_L(0) = 0
!
      do i = 1, DJDS_IO%NLmax * DJDS_IO%NHYP * PEsmpTOT_IO
        read(id_file,*) itmp, DJDS_IO%indexDJDS_L(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itmp,  DJDS_IO%NUmax, DJDS_IO%npUX1
!
      num = PEsmpTOT_IO * DJDS_IO%NUmax * DJDS_IO%NHYP
      allocate(DJDS_IO%indexDJDS_U(0:num))
      DJDS_IO%indexDJDS_U(0) = 0
!
      do i = 1, DJDS_IO%NUmax * DJDS_IO%NHYP * PEsmpTOT_IO
        read(id_file,*) itmp, DJDS_IO%indexDJDS_U(i)
      end do
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) NP_IO
!
      allocate (DMAT11_IO%D(NP_IO))
      allocate (DJDS_IO%NEWtoOLD(NP_IO))
      do i = 1, NP_IO
        read(id_file,*) itmp, DJDS_IO%NEWtoOLD(i), DMAT11_IO%D(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) DJDS_IO%itotal_l
!
      allocate (DMAT11_IO%AL(DJDS_IO%itotal_l))
      allocate (DJDS_IO%itemDJDS_L(DJDS_IO%itotal_l))
      do i = 1, DJDS_IO%itotal_l
        read(id_file,*) itmp, DJDS_IO%itemDJDS_L(i), DMAT11_IO%AL(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) DJDS_IO%itotal_u
!
      allocate (DMAT11_IO%AU(DJDS_IO%itotal_u))
      allocate (DJDS_IO%itemDJDS_U(DJDS_IO%itotal_u))
      do i = 1, DJDS_IO%itotal_u
        read(id_file,*) itmp, DJDS_IO%itemDJDS_U(i), DMAT11_IO%AU(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) N_IO
!
      allocate (DMAT11_IO%ALUG_L(N_IO))
      allocate (DMAT11_IO%ALUG_U(N_IO))
      do i = 1, N_IO
        read(id_file,*) itmp, DMAT11_IO%ALUG_L(i), DMAT11_IO%ALUG_U(i)
      end do
!
      end subroutine read_djds_mat11_comp
!
!  ---------------------------------------------------------------------
!
      subroutine read_djds_mat33_comp(id_file)
!
      integer(kind=kint ), intent(in) :: id_file
      integer(kind = kint) :: i, itmp, num
!
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itmp,  DJDS_IO%NLmax, DJDS_IO%npLX1
!
      num = PEsmpTOT_IO*DJDS_IO%NLmax*DJDS_IO%NHYP
      allocate(DJDS_IO%indexDJDS_L(0:num))
      DJDS_IO%indexDJDS_L(0) = 0
!
      do i = 1, DJDS_IO%NLmax*DJDS_IO%NHYP*PEsmpTOT_IO
        read(id_file,*) itmp, DJDS_IO%indexDJDS_L(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itmp,  DJDS_IO%NUmax, DJDS_IO%npUX1
!
      num = PEsmpTOT_IO*DJDS_IO%NUmax*DJDS_IO%NHYP
      allocate(DJDS_IO%indexDJDS_U(0:num))
      DJDS_IO%indexDJDS_U(0) = 0
!
      do i = 1, DJDS_IO%NUmax*DJDS_IO%NHYP*PEsmpTOT_IO
        read(id_file,*) itmp, DJDS_IO%indexDJDS_U(i)
      end do
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) NP_IO
!
      allocate (DMAT33_IO%D(9*NP_IO))
      allocate (DJDS_IO%NEWtoOLD(NP_IO))
      do i = 1, NP_IO
        read(id_file,*) itmp, DJDS_IO%NEWtoOLD(i),                      &
     &                  DMAT33_IO%D(9:i-8:9*i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) DJDS_IO%itotal_l
!
      allocate (DMAT33_IO%AL(9*DJDS_IO%itotal_l))
      allocate (DJDS_IO%itemDJDS_L(DJDS_IO%itotal_l))
      do i = 1, DJDS_IO%itotal_l
        read(id_file,*) itmp, DJDS_IO%itemDJDS_L(i),                    &
     &                        DMAT33_IO%AL(9:i-8:9*i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) DJDS_IO%itotal_u
!
      allocate (DMAT33_IO%AU(9*DJDS_IO%itotal_u))
      allocate (DJDS_IO%itemDJDS_U(DJDS_IO%itotal_u))
      do i = 1, DJDS_IO%itotal_u
        read(id_file,*) itmp, DJDS_IO%itemDJDS_U(i),                    &
     &                        DMAT33_IO%AU(9:i-8:9*i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) N_IO
!
      allocate (DMAT33_IO%ALUG_L(9*N_IO))
      allocate (DMAT33_IO%ALUG_U(9*N_IO))
      do i = 1, N_IO
        read(id_file,*) itmp,  DMAT33_IO%ALUG_L(9:i-8:9*i),             &
     &                         DMAT33_IO%ALUG_U(9:i-8:9*i)
      end do
!
      end subroutine read_djds_mat33_comp
!
!  ---------------------------------------------------------------------
!
      subroutine read_djds_mat_connects(id_file)
!
      integer(kind=kint ), intent(in) :: id_file
      integer(kind = kint) :: i, itmp
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) PEsmpTOT_IO
!
      allocate (DJDS_IO%STACKmcG(0:PEsmpTOT_IO))
      DJDS_IO%STACKmcG(0) = 0
      do i = 1, PEsmpTOT_IO
        read(id_file,*) itmp, DJDS_IO%STACKmcG(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) DJDS_IO%NHYP
!
      allocate (DJDS_IO%IVECT(0:DJDS_IO%NHYP))
      allocate (DJDS_IO%NLmaxHYP(DJDS_IO%NHYP))
      allocate (DJDS_IO%NUmaxHYP(DJDS_IO%NHYP))
      DJDS_IO%IVECT(0) = 0
      do i = 1, DJDS_IO%NHYP
        read(id_file,*) itmp, DJDS_IO%IVECT(i),                         &
     &                  DJDS_IO%NLmaxHYP(i), DJDS_IO%NUmaxHYP(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itmp
!
      allocate (DJDS_IO%STACKmc(0:PEsmpTOT_IO*DJDS_IO%NHYP))
      DJDS_IO%STACKmc(0) = 0
      do i = 1, PEsmpTOT_IO*DJDS_IO%NHYP
        read(id_file,*) itmp, DJDS_IO%STACKmc(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) NP_IO
!
      allocate (DJDS_IO%NEWtoOLD_DJDS_U(NP_IO))
      allocate (DJDS_IO%OLDtoNEW_DJDS_L(NP_IO))
      allocate (DJDS_IO%OLDtoNEW_DJDS_U(NP_IO))
      allocate (DJDS_IO%LtoU(NP_IO))
      do i = 1, NP_IO
        read(id_file,*) itmp, DJDS_IO%NEWtoOLD_DJDS_U(i),               &
     &       DJDS_IO%OLDtoNEW_DJDS_L(i),  DJDS_IO%OLDtoNEW_DJDS_U(i),   &
     &       DJDS_IO%LtoU(i)
      end do
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) NTOT_EXPORT_IO
!
      allocate (DJDS_IO%NOD_EXPORT_NEW(NTOT_EXPORT_IO))
      do i = 1, NTOT_EXPORT_IO
        read(id_file,*) itmp, DJDS_IO%NOD_EXPORT_NEW(i)
      end do
!
      end subroutine read_djds_mat_connects
!
!  ---------------------------------------------------------------------
!
      end module m_read_djds_matrix_IO
