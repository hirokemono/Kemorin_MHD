!>@file   ordering_MC_RCM_type.f90
!!@brief  module ordering_MC_RCM_type
!!
!!@author K. Nakajima and H. Matsui
!!@date     Written by K. Nakajima in 2001
!!@n        modified by H. Matsui in May. 2002
!!@n        modified by H. Matsui in June. 2006
!!@n        modified by H. Matsui in Jan., 2009
!!@n        Modified in Nov., 2013
!
!>      RCM ordering from CRS matrix
!!
!!@verbatim
!!      subroutine count_rcm(NP, N, solver_C, tbl_crs, djds_tbl,        &
!!     &          DJDS_param, WK_MC, WK_DJDS)
!!        type(CRS_matrix_connect), intent(in) :: tbl_crs
!!        type(mpi_4_solver), intent(in) :: solver_C
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!        type(work_4_RCM), intent(inout) :: WK_MC
!!        type(work_DJDS_ordering), intent(inout) :: WK_DJDS
!!@endverbatim
!
      module ordering_MC_RCM_type
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_rcm(NP, N, solver_C, tbl_crs, djds_tbl,          &
     &          DJDS_param, WK_MC, WK_DJDS)
!
      use calypso_mpi
      use m_machine_parameter
      use t_crs_connect
      use t_solver_djds
      use t_vector_for_solver
      use t_colored_connect
      use t_work_DJDS_ordering
      use t_iccg_parameter
!
      use ordering_MC_RCM
      use MC_Cuthill_McKee
!
      integer(kind = kint), intent(in) :: NP, N
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(mpi_4_solver), intent(in) :: solver_C
      type(DJDS_poarameter), intent(in) :: DJDS_param
!
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
      type(work_4_RCM), intent(inout) :: WK_MC
      type(work_DJDS_ordering), intent(inout) :: WK_DJDS
!
      integer(kind=kint) :: NHYPmax
      integer(kind=kint) :: IFLAG, IFLAGmax
!
      integer(kind = kint) :: i
!
!
      call alloc_IVECT_rcm(NP, WK_DJDS)
      call alloc_mc_stack(NP, WK_MC)
      call copy_ntot_from_crs_mat(tbl_crs, WK_MC)
!
      call alloc_mc_connect(WK_MC)
      djds_tbl%NLmax = WK_MC%max_mc_l
      djds_tbl%NUmax = WK_MC%max_mc_u
!
!----------------------------------------------------------
!   skip multi colorling (only for diagonal scaling)
!----------------------------------------------------------
!
      if(iflag_debug.eq.1) write(*,*)                                   &
     &             'iflag_ordering', DJDS_param%iflag_ordering
!
      call alloc_iW_ordering(NP, WK_DJDS)
!
      if(DJDS_param%iflag_ordering .eq. iflag_OFF) then
!
        if (iflag_debug.eq.1) write(*,*) 'no_MC'
        call no_MC(NP, tbl_crs%ntot_l, tbl_crs%ntot_u,                  &
     &      tbl_crs%istack_l,   tbl_crs%istack_u,                       &
     &      tbl_crs%item_l,     tbl_crs%item_u,                         &
     &      WK_MC%ntot_mc_l,    WK_MC%ntot_mc_u,                        &
     &      WK_MC%num_mc_l,     WK_MC%num_mc_u,                         &
     &      WK_MC%istack_mc_l,  WK_MC%istack_mc_u,                      &
     &      WK_MC%item_mc_l,    WK_MC%item_mc_u,                        &
     &      djds_tbl%NHYP, WK_DJDS%IVECT_rcm,                           &
     &      djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW)
!
        WK_DJDS%NCOLORtot = 1
!
        call alloc_work_4_rcm(NP, djds_tbl%NHYP, WK_DJDS)
        WK_DJDS%IW(1:NP) = 0
!
!
!CDIR NOVECTOR
        do i= 1, NP
          djds_tbl%OLDtoNEW(i)= i
          djds_tbl%NEWtoOLD(i)= i
          WK_DJDS%OLDtoNEWmc(i)= i
          WK_DJDS%NEWtoOLDmc(i)= i
        enddo
!
        WK_DJDS%IVECmc(0) = 0
        WK_DJDS%IVECmc(1) = N
      else
!C
!C +----------------+
!C | reordering RCM |
!C +----------------+
!C===
!
!  -------  RCM ordering
        if(DJDS_param%iflag_ordering .eq. iflag_MC_RCM) then
          if (iflag_debug.eq.1) write(*,*) 'sRCM'
          call sRCM (NP, N, tbl_crs%ntot_l,   tbl_crs%ntot_u,           &
     &        tbl_crs%istack_l,   tbl_crs%istack_u,                     &
     &        tbl_crs%item_l,     tbl_crs%item_u,                       &
     &        WK_MC%ntot_mc_l,    WK_MC%ntot_mc_u,                      &
     &        WK_MC%num_mc_l,     WK_MC%num_mc_u,                       &
     &        WK_MC%istack_mc_l,  WK_MC%istack_mc_u,                    &
     &        WK_MC%item_mc_l,    WK_MC%item_mc_u,                      &
     &        djds_tbl%NHYP, WK_DJDS%IVECT_rcm,                         &
     &        djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW, WK_DJDS%IW)
!
!  -------  MC ordering
        else if(DJDS_param%iflag_ordering .eq. iflag_MultiColor) then
          if (iflag_debug.eq.1) write(*,*) 'sMC'
          call sMC(DJDS_param%mc_color, NP, N,                          &
     &        tbl_crs%ntot_l, tbl_crs%ntot_u,                           &
     &        tbl_crs%istack_l,   tbl_crs%istack_u,                     &
     &        tbl_crs%item_l,     tbl_crs%item_u,                       &
     &        WK_MC%ntot_mc_l,    WK_MC%ntot_mc_u,                      &
     &        WK_MC%num_mc_l,     WK_MC%num_mc_u,                       &
     &        WK_MC%istack_mc_l,  WK_MC%istack_mc_u,                    &
     &        WK_MC%item_mc_l,    WK_MC%item_mc_u,                      &
     &        djds_tbl%NHYP, WK_DJDS%IVECT_rcm,                         &
     &        djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW, WK_DJDS%IW)
        end if
!
!C===
!C
!C
!C +----------------------+
!C | cyclic MULTIcoloring |
!C +----------------------+
!C===
!C
!C-- ORDERING
        call MPI_allREDUCE (djds_tbl%NHYP, NHYPmax, 1,                  &
     &      CALYPSO_INTEGER, MPI_MAX, solver_C%SOLVER_COMM, ierr_MPI)

        WK_DJDS%NCOLORtot = DJDS_param%min_color
        if (WK_DJDS%NCOLORtot.gt.NHYPmax/itwo) then
          WK_DJDS%NCOLORtot = ( NHYPmax+mod(NHYPmax,itwo) ) / itwo
        endif

  999   continue
!
        if (iflag_debug.eq.1) then
          write (*,'(a,i8,a,i8)')                                       &
     &         'PE:',my_rank,'color number: ', WK_DJDS%NCOLORtot 
        endif
!
        call alloc_work_4_rcm(NP, djds_tbl%NHYP, WK_DJDS)
!
        call set_color_tbl_RCM_MC(NP, djds_tbl%NHYP, WK_DJDS%NCOLORtot, &
     &      WK_DJDS%IVECT_rcm, WK_DJDS%IVECmc, WK_DJDS%ICHK,            &
     &      WK_DJDS%IVnew, WK_DJDS%IW)
!
!C
!C-- CHECK dependency
        call check_dependency_RCM_MC(my_rank, NP,                       &
     &      WK_MC%ntot_mc_l,   WK_MC%ntot_mc_u,                         &
     &      WK_MC%istack_mc_l, WK_MC%istack_mc_u,                       &
     &      WK_MC%item_mc_l,   WK_MC%item_mc_u,                         &
     &      WK_DJDS%NCOLORtot, WK_DJDS%IVECmc, WK_DJDS%IVnew,           &
     &      WK_DJDS%IW, IFLAG)
!
        call MPI_allREDUCE (IFLAG, IFLAGmax, 1, CALYPSO_INTEGER,        &
     &                    MPI_MAX, solver_C%SOLVER_COMM, ierr_MPI)
!
        if (IFLAGmax.eq.1) then
          WK_DJDS%NCOLORtot = WK_DJDS%NCOLORtot + 1
          if(WK_DJDS%NCOLORtot.gt.djds_tbl%NHYP)                        &
     &                           WK_DJDS%NCOLORtot= djds_tbl%NHYP
          call reset_4_new_rcm(WK_DJDS)
          goto 999
        endif
!
        call set_RCM_MC_table(N, NP, WK_DJDS%NCOLORtot, WK_DJDS%IVnew,  &
     &      djds_tbl%NHYP, WK_DJDS%OLDtoNEWmc, WK_DJDS%NEWtoOLDmc)
!
      end if
!
      call dealloc_work_4_RCM(WK_DJDS)
      call dealloc_iW_ordering(WK_DJDS)
      call dealloc_IVECT_rcm(WK_DJDS)
!
      end subroutine  count_rcm
!
! ----------------------------------------------------------------------
!
      end module ordering_MC_RCM_type
