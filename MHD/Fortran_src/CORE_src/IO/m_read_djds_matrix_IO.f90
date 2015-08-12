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
!
      use skip_comment_f
!
      implicit none
!
!
!C-- Ordering arrays
!
      integer(kind=kint) :: N_IO, NP_IO, PEsmpTOT_IO
      integer(kind=kint) :: itotal_l_IO, itotal_u_IO
!
      real(kind=kreal), allocatable :: D_IO(:), AL_IO(:), AU_IO(:)
      real(kind=kreal), allocatable :: ALUG_L_IO(:), ALUG_U_IO(:)
!
      real(kind=kreal), allocatable :: D33_IO(:)
      real(kind=kreal), allocatable :: AL33_IO(:), AU33_IO(:)
      real(kind=kreal), allocatable :: ALUG33_L_IO(:), ALUG33_U_IO(:)
!
!    coefs for ordering
!
      integer(kind=kint) :: NLmax_IO, NUmax_IO
      integer(kind=kint) :: npLX1_IO, npUX1_IO
!
      integer(kind=kint) :: NHYP_IO
      integer(kind=kint), allocatable :: IVECT_IO(:)
!
      integer(kind=kint), allocatable :: NEWtoOLD_IO(:)
!
      integer(kind=kint), allocatable :: NEWtoOLD_DJDS_U_IO(:)
      integer(kind=kint), allocatable :: OLDtoNEW_DJDS_L_IO(:)
      integer(kind=kint), allocatable :: OLDtoNEW_DJDS_U_IO(:)
      integer(kind=kint), allocatable :: LtoU_IO(:)
!
      integer(kind=kint), allocatable :: indexDJDS_L_IO(:)
      integer(kind=kint), allocatable :: indexDJDS_U_IO(:)
      integer(kind=kint), allocatable :: itemDJDS_L_IO(:)
      integer(kind=kint), allocatable :: itemDJDS_U_IO(:)
      integer(kind=kint), allocatable :: NLmaxHYP_IO(:)
      integer(kind=kint), allocatable :: NUmaxHYP_IO(:)
!
      integer(kind=kint), allocatable :: STACKmcG_IO(:)
      integer(kind=kint), allocatable :: STACKmc_IO(:)
!
      integer(kind=kint) :: NTOT_EXPORT_IO
      integer(kind = kint), allocatable :: NOD_EXPORT_NEW_IO(:)
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
      deallocate(indexDJDS_L_IO, indexDJDS_U_IO)
!
      deallocate (D_IO, NEWtoOLD_IO)
      deallocate (AL_IO, itemDJDS_L_IO)
      deallocate (AU_IO, itemDJDS_U_IO)
!
      deallocate (ALUG_L_IO, ALUG_U_IO)
!
      end subroutine deallocate_djds_mat11_comp_IO
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_djds_mat33_cmop_IO
!
!
      deallocate(indexDJDS_L_IO, indexDJDS_U_IO)
!
      deallocate (D33_IO, NEWtoOLD_IO)
      deallocate (AL33_IO, itemDJDS_L_IO)
      deallocate (AU33_IO, itemDJDS_U_IO)
!
      deallocate (ALUG33_L_IO, ALUG33_U_IO)
!
      end subroutine deallocate_djds_mat33_cmop_IO
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_djds_mat_connects_IO
!
!
      deallocate (STACKmcG_IO, STACKmc_IO)
      deallocate (IVECT_IO, NLmaxHYP_IO, NUmaxHYP_IO)
!
      deallocate (NEWtoOLD_DJDS_U_IO, OLDtoNEW_DJDS_L_IO)
      deallocate (OLDtoNEW_DJDS_U_IO, LtoU_IO)
!
      deallocate (NOD_EXPORT_NEW_IO)
!
      end subroutine deallocate_djds_mat_connects_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_djds_mat11_comp(id_file)
!
      integer(kind=kint ), intent(in) :: id_file
      integer(kind = kint) :: i, itmp
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itmp,  NLmax_IO, npLX1_IO
!
      allocate(indexDJDS_L_IO(0:PEsmpTOT_IO*NLmax_IO*NHYP_IO))
      indexDJDS_L_IO(0) = 0
!
      do i = 1, NLmax_IO*NHYP_IO*PEsmpTOT_IO
        read(id_file,*) itmp, indexDJDS_L_IO(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itmp,  NUmax_IO, npUX1_IO
!
      allocate(indexDJDS_U_IO(0:PEsmpTOT_IO*NUmax_IO*NHYP_IO))
      indexDJDS_U_IO(0) = 0
!
      do i = 1, NUmax_IO*NHYP_IO*PEsmpTOT_IO
        read(id_file,*) itmp, indexDJDS_U_IO(i)
      end do
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) NP_IO
!
      allocate (D_IO(NP_IO))
      allocate (NEWtoOLD_IO(NP_IO))
      do i = 1, NP_IO
        read(id_file,*) itmp, NEWtoOLD_IO(i), D_IO(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itotal_l_IO
!
      allocate (AL_IO(itotal_l_IO))
      allocate (itemDJDS_L_IO(itotal_l_IO))
      do i = 1, itotal_l_IO
        read(id_file,*) itmp, itemDJDS_L_IO(i), AL_IO(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itotal_u_IO
!
      allocate (AU_IO(itotal_u_IO))
      allocate (itemDJDS_U_IO(itotal_u_IO))
      do i = 1, itotal_u_IO
        read(id_file,*) itmp, itemDJDS_U_IO(i), AU_IO(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) N_IO
!
      allocate (ALUG_L_IO(N_IO))
      allocate (ALUG_U_IO(N_IO))
      do i = 1, N_IO
        read(id_file,*) itmp, ALUG_L_IO(i), ALUG_U_IO(i)
      end do
!
      end subroutine read_djds_mat11_comp
!
!  ---------------------------------------------------------------------
!
      subroutine read_djds_mat33_comp(id_file)
!
      integer(kind=kint ), intent(in) :: id_file
      integer(kind = kint) :: i, itmp
!
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itmp,  NLmax_IO, npLX1_IO
!
      allocate(indexDJDS_L_IO(0:PEsmpTOT_IO*NLmax_IO*NHYP_IO))
      indexDJDS_L_IO(0) = 0
!
      do i = 1, NLmax_IO*NHYP_IO*PEsmpTOT_IO
        read(id_file,*) itmp, indexDJDS_L_IO(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itmp,  NUmax_IO, npUX1_IO
!
      allocate(indexDJDS_U_IO(0:PEsmpTOT_IO*NUmax_IO*NHYP_IO))
      indexDJDS_U_IO(0) = 0
!
      do i = 1, NUmax_IO*NHYP_IO*PEsmpTOT_IO
        read(id_file,*) itmp, indexDJDS_U_IO(i)
      end do
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) NP_IO
!
      allocate (D33_IO(9*NP_IO))
      allocate (NEWtoOLD_IO(NP_IO))
      do i = 1, NP_IO
        read(id_file,*) itmp, NEWtoOLD_IO(i), D33_IO(9:i-8:9*i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itotal_l_IO
!
      allocate (AL33_IO(9*itotal_l_IO))
      allocate (itemDJDS_L_IO(itotal_l_IO))
      do i = 1, itotal_l_IO
        read(id_file,*) itmp, itemDJDS_L_IO(i), AL33_IO(9:i-8:9*i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itotal_u_IO
!
      allocate (AU33_IO(9*itotal_u_IO))
      allocate (itemDJDS_U_IO(itotal_u_IO))
      do i = 1, itotal_u_IO
        read(id_file,*) itmp, itemDJDS_U_IO(i), AU33_IO(9:i-8:9*i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) N_IO
!
      allocate (ALUG33_L_IO(9*N_IO))
      allocate (ALUG33_U_IO(9*N_IO))
      do i = 1, N_IO
        read(id_file,*) itmp,  ALUG33_L_IO(9:i-8:9*i),                  &
     &                      ALUG33_U_IO(9:i-8:9*i)
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
      allocate (STACKmcG_IO(0:PEsmpTOT_IO))
      STACKmcG_IO(0) = 0
      do i = 1, PEsmpTOT_IO
        read(id_file,*) itmp, STACKmcG_IO(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) NHYP_IO
!
      allocate (IVECT_IO(0:NHYP_IO))
      allocate (NLmaxHYP_IO(NHYP_IO))
      allocate (NUmaxHYP_IO(NHYP_IO))
      IVECT_IO(0) = 0
      do i = 1, NHYP_IO
        read(id_file,*) itmp, IVECT_IO(i),                              &
     &                  NLmaxHYP_IO(i), NUmaxHYP_IO(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) itmp
!
      allocate (STACKmc_IO(0:PEsmpTOT_IO*NHYP_IO))
      STACKmc_IO(0) = 0
      do i = 1, PEsmpTOT_IO*NHYP_IO
        read(id_file,*) itmp, STACKmc_IO(i)
      end do
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) NP_IO
!
      allocate (NEWtoOLD_DJDS_U_IO(NP_IO))
      allocate (OLDtoNEW_DJDS_L_IO(NP_IO))
      allocate (OLDtoNEW_DJDS_U_IO(NP_IO))
      allocate (LtoU_IO(NP_IO))
      do i = 1, NP_IO
        read(id_file,*) itmp, NEWtoOLD_DJDS_U_IO(i),                    &
     &       OLDtoNEW_DJDS_L_IO(i),  OLDtoNEW_DJDS_U_IO(i), LtoU_IO(i)
      end do
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) NTOT_EXPORT_IO
!
      allocate (NOD_EXPORT_NEW_IO(NTOT_EXPORT_IO))
      do i = 1, NTOT_EXPORT_IO
        read(id_file,*) itmp, NOD_EXPORT_NEW_IO(i)
      end do
!
      end subroutine read_djds_mat_connects
!
!  ---------------------------------------------------------------------
!
      end module m_read_djds_matrix_IO
