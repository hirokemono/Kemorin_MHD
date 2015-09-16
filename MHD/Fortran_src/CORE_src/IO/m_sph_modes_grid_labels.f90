!>@file  m_sph_modes_grid_labels.f90
!!       module m_sph_modes_grid_labels
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Routines for speherical grid data IO
!!
!!@verbatim
!!      character(len=ilen_sph_para) function hd_sph_para()
!!      character(len=ilen_rtp_glbl) function hd_rtp_glbl()
!!      character(len=ilen_rtp_comm) function hd_rtp_comm()
!!
!!      character(len=ilen_rj_glbl) function hd_rj_glbl()
!!      character(len=ilen_rj_comm) function hd_rj_comm()
!!
!!      character(len=ilen_rlm_glbl) function hd_rlm_glbl()
!!
!!      character(len=ilen_sgmt) function hd_segment()
!!      character(len=ilen_trnc) function hd_trunc()
!!@endverbatim
!
      module m_sph_modes_grid_labels
!
      use m_precision
!
!
      character(len= 2), parameter                                      &
     &         :: hd_sph11 = '! '
      character(len=24), parameter                                      &
     &         :: hd_sph12 = '! 1.parallel information'
      character(len=14), parameter                                      &
     &         :: hd_sph13 = '!    domain ID'
      character(len=35), parameter                                      &
     &         :: hd_sph14 = '!    number of domain for transfer'
      character(len=28), parameter                                      &
     &         :: hd_sph15 = '!    domain ID for transfer'
      character(len= 2), parameter                                      &
     &         :: hd_sph16 = '! '
!
      character(len= 1), parameter                                      &
     &         :: hd_rtp21 = '!'
      character(len=41), parameter                                      &
     &         :: hd_rtp22 = '! number of stack number for each domain'
      character(len=21), parameter                                      &
     &         :: hd_rtp23 = '! local wavenumber ID'
      character(len=30), parameter                                      &
     &         :: hd_rtp24 = '! global radial ID and grid ID'
      character(len= 1), parameter                                      &
     &         :: hd_rtp25 = '!'
!
      character(len= 1), parameter                                      &
     &         :: hd_rtp31 = '!'
      character(len=39), parameter :: hd_rtp32                          &
     &           = '! communication table between grid data'
      character(len= 1), parameter                                      &
     &         :: hd_rtp33 = '!'
!
!
      character(len= 1), parameter                                      &
     &         :: hd_rj21 =  '!'
      character(len=41), parameter                                      &
     &         :: hd_rj22 =  '! number of stack number for each domain'
      character(len=21), parameter                                      &
     &         :: hd_rj23 =  '! local wavenumber ID'
      character(len=32), parameter                                      &
     &         :: hd_rj24 =  '! global radial ID and spectr ID'
      character(len= 1), parameter                                      &
     &         :: hd_rj25 =  '!'
!
      character(len= 1), parameter                                      &
     &         :: hd_rj31 = '!'
      character(len=41), parameter :: hd_rj32                           &
     &           = '! communication table between spectr data'
      character(len= 1), parameter                                      &
     &         :: hd_rj33 = '!'
!
!
      character(len= 1), parameter                                      &
     &         :: hd_rlm21 = '!'
      character(len=40), parameter                                      &
     &         :: hd_rlm22 = '! number of stack number for each domain'
      character(len=21), parameter                                      &
     &         :: hd_rlm23 = '! local wavenumber ID'
      character(len=36), parameter                                      &
     &         :: hd_rlm24 = '! global radial ID and wavenumber ID'
      character(len= 1), parameter                                      &
     &         :: hd_rlm25 = '!'
!
!
      character(len= 1), parameter                                      &
     &         :: hd_sgmt1 = '!'
      character(len=11), parameter                                      &
     &         :: hd_sgmt2 = '! Domain ID'
      character(len=31), parameter                                      &
     &         :: hd_sgmt3 = '! segment ID for each direction'
      character(len= 1), parameter                                      &
     &         :: hd_sgmt4 = '!'
!
      character(len= 1), parameter                                      &
     &         :: hd_trnc1 = '!'
      character(len=22), parameter                                      &
     &         :: hd_trnc2 = '! num. of global grids'
      character(len=42), parameter :: hd_trnc3                          &
     &           = '! truncation level for spherical harmonics'
      character(len= 1), parameter                                      &
     &         :: hd_trnc4 = '!'
!
!
!
      integer(kind = kint), parameter                                   &
     &         :: ilen_sph_para = 2+24+14+35+28+2+6
      integer(kind = kint), parameter                                   &
     &         :: ilen_rtp_glbl = 1+41+21+30+1+5
      integer(kind = kint), parameter                                   &
     &         :: ilen_rtp_comm = 1+39+1+3
!
      integer(kind = kint), parameter                                   &
     &         :: ilen_rj_glbl = 1+41+21+32+1+5
      integer(kind = kint), parameter                                   &
     &         :: ilen_rj_comm = 1+41+1+3
!
      integer(kind = kint), parameter                                   &
     &         :: ilen_rlm_glbl = 1+40+21+36+1+5
!
      integer(kind = kint), parameter                                   &
     &         :: ilen_sgmt = 1+11+31+1+4
      integer(kind = kint), parameter                                   &
     &         :: ilen_trnc = 1+22+42+1+4
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      character(len=ilen_sph_para) function hd_sph_para()
!
      hd_sph_para = hd_sph11 // char(10)                                &
     &          //  hd_sph12 // char(10)                                &
     &          //  hd_sph13 // char(10)                                &
     &          //  hd_sph14 // char(10)                                &
     &          //  hd_sph15 // char(10)                                &
     &          //  hd_sph16 // char(10)
!
      end function hd_sph_para
!
!------------------------------------------------------------------
!
      character(len=ilen_rtp_glbl) function hd_rtp_glbl()
!
      hd_rtp_glbl = hd_rtp21 // char(10)                                &
     &           // hd_rtp22 // char(10)                                &
     &           // hd_rtp23 // char(10)                                &
     &           // hd_rtp24 // char(10)                                &
     &           // hd_rtp25 // char(10)
!
      end function hd_rtp_glbl
!
!------------------------------------------------------------------
!
      character(len=ilen_rtp_comm) function hd_rtp_comm()
!
      hd_rtp_comm = hd_rtp31 // char(10)                                &
     &           // hd_rtp32 // char(10)                                &
     &           // hd_rtp33 // char(10)
!
      end function hd_rtp_comm
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_rj_glbl) function hd_rj_glbl()
!
      hd_rj_glbl =  hd_rj21 // char(10)                                 &
     &          //  hd_rj22 // char(10)                                 &
     &          //  hd_rj23 // char(10)                                 &
     &          //  hd_rj24 // char(10)                                 &
     &          //  hd_rj25 // char(10)
!
      end function hd_rj_glbl
!
!------------------------------------------------------------------
!
      character(len=ilen_rj_comm) function hd_rj_comm()
!
      hd_rj_comm =  hd_rj31 // char(10)                                 &
     &          //  hd_rj32 // char(10)                                 &
     &          //  hd_rj33 // char(10)
!
      end function hd_rj_comm
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_rlm_glbl) function hd_rlm_glbl()
!
      hd_rlm_glbl = hd_rlm21 // char(10)                                &
     &           // hd_rlm22 // char(10)                                &
     &           // hd_rlm23 // char(10)                                &
     &           // hd_rlm24 // char(10)                                &
     &           // hd_rlm25 // char(10)
!
      end function hd_rlm_glbl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_sgmt) function hd_segment()
!
      hd_segment =  hd_sgmt1 // char(10)                                &
     &           // hd_sgmt2 // char(10)                                &
     &           // hd_sgmt3 // char(10)                                &
     &           // hd_sgmt4 // char(10)
!
      end function hd_segment
!
!------------------------------------------------------------------
!
      character(len=ilen_trnc) function hd_trunc()
!
      hd_trunc =    hd_trnc1 // char(10)                                &
     &           // hd_trnc2 // char(10)                                &
     &           // hd_trnc3 // char(10)                                &
     &           // hd_trnc4 // char(10)
!
      end function hd_trunc
!
!------------------------------------------------------------------
!
      end module m_sph_modes_grid_labels
