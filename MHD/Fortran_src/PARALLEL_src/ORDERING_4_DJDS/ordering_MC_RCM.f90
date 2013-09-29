!
!     module ordering_MC_RCM
!
!      Written by K. Nakajima in 2001
!        modified by H. Matsui on May. 2002
!        modified by H. Matsui on June. 2006
!        modified by H. Matsui on Jan., 2009
!
!      subroutine count_rcm(NHYP, OLDtoNEW, NEWtoOLD,  NLmax, NUmax)
!
!      subroutine check_dependency_RCM_MC(my_rank, NP,                  &
!     &          NPL_mc, NPU_mc, INL_mc, INU_mc, IAL_mc, IAU_mc,        &
!     &          NCOLORtot, IVECmc, IVnew, IW, IFLAG)
!      subroutine set_color_tbl_RCM_MC(NP, NHYP, NCOLORtot, IVECT_rcm,  &
!     &          IVECmc, ICHK, IVnew, IW)
!      subroutine set_RCM_MC_table(NP, N, NCOLORtot, IVnew,             &
!     &          NHYP, OLDtoNEWmc, NEWtoOLDmc)
!
      module ordering_MC_RCM
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
      subroutine count_rcm(NHYP, OLDtoNEW, NEWtoOLD,  NLmax, NUmax)
!
      use calypso_mpi
      use m_machine_parameter
      use m_parallel_var_dof
      use m_iccg_parameter
      use m_geometry_parameter
      use m_matrix_work
!
      use m_crs_connect
      use m_colored_connect
!
      use MC_Cuthill_McKee
!
      integer(kind=kint), intent(inout) :: NHYP
      integer(kind=kint), intent(inout) :: NLmax, NUmax
      integer(kind=kint), intent(inout) :: OLDtoNEW(numnod)
      integer(kind=kint), intent(inout) :: NEWtoOLD(numnod)
!
      integer(kind=kint) :: NHYPmax
      integer(kind=kint) :: IFLAG, IFLAGmax
!
      integer(kind = kint) :: i
!
!
      ntot_mc_l = ntot_crs_l
      ntot_mc_u = ntot_crs_u
      max_mc_l =  max_crs_l
      min_mc_l =  min_crs_l
      max_mc_u =  max_crs_u
      min_mc_u =  min_crs_u
!
      call allocate_IVECT_rcm(numnod)
      call allocate_mc_stack(numnod)
      call allocate_mc_connect
!
!
      NLmax= max_mc_l
      NUmax= max_mc_u
!
!----------------------------------------------------------
!   skip multi colorling (only for diagonal scaling)
!----------------------------------------------------------
!
      if(iflag_debug.eq.1) write(*,*) 'iflag_ordering', iflag_ordering
!
      call allocate_iW_ordering(numnod)
!
      if (iflag_ordering .eq. 0 ) then
!
        if (iflag_debug.eq.1) write(*,*) 'no_MC'
        call no_MC (numnod,  ntot_crs_l, ntot_crs_u,                   &
     &     istack_crs_l, istack_crs_u, item_crs_l, item_crs_u,         &
     &     ntot_mc_l, ntot_mc_u, num_mc_l, num_mc_u,                   &
     &     istack_mc_l, istack_mc_u, item_mc_l, item_mc_u,             &
     &     NHYP, IVECT_rcm, NEWtoOLD, OLDtoNEW)
!
        NCOLORtot = 1
!
        call allocate_work_4_rcm(numnod, NHYP)
        IW(1:numnod) = 0
!
!
!CDIR NOVECTOR
        do i= 1, numnod
          OLDtoNEW(i)= i
          NEWtoOLD(i)= i
          OLDtoNEWmc(i)= i
          NEWtoOLDmc(i)= i
        enddo
!
        IVECmc(0) = 0
        IVECmc(1) = internal_node
!
      else
!C
!C +----------------+
!C | reordering RCM |
!C +----------------+
!C===
!
!  -------  RCM ordering
        if ( iflag_ordering .eq. 1 ) then
          if (iflag_debug.eq.1) write(*,*) 'sRCM'
          call sRCM (numnod, internal_node, ntot_crs_l, ntot_crs_u,     &
     &        istack_crs_l, istack_crs_u, item_crs_l, item_crs_u,       &
     &        ntot_mc_l, ntot_mc_u, num_mc_l, num_mc_u,                 &
     &        istack_mc_l, istack_mc_u, item_mc_l, item_mc_u,           &
     &        NHYP, IVECT_rcm, NEWtoOLD, OLDtoNEW, IW)
!
!  -------  MC ordering
        else if ( iflag_ordering .eq. 2 ) then
          if (iflag_debug.eq.1) write(*,*) 'sMC', mc_color
          call sMC (numnod, internal_node, ntot_crs_l, ntot_crs_u,      &
     &        istack_crs_l, istack_crs_u, item_crs_l, item_crs_u,       &
     &        ntot_mc_l, ntot_mc_u, num_mc_l, num_mc_u,                 &
     &        istack_mc_l, istack_mc_u, item_mc_l, item_mc_u,           &
     &        NHYP, IVECT_rcm,NEWtoOLD, OLDtoNEW, IW, mc_color)
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
        call MPI_allREDUCE (NHYP, NHYPmax, 1, CALYPSO_INTEGER, MPI_MAX, &
     &                      CALYPSO_COMM, ierr)

        NCOLORtot= min_color
        if (NCOLORtot.gt.NHYPmax/2) then
          NCOLORtot= ( NHYPmax+mod(NHYPmax,2) )/2
        endif

  999   continue
!
        if (iflag_debug.eq.1) then
          write (*,'(a,i8,a,i8)')                                       &
     &         'PE:',my_rank,'color number: ', NCOLORtot 
        endif
!
        call allocate_work_4_rcm(numnod, NHYP)
!
        call set_color_tbl_RCM_MC(numnod, NHYP, NCOLORtot, IVECT_rcm,   &
     &      IVECmc, ICHK, IVnew, IW)
!
!C
!C-- CHECK dependency
        call check_dependency_RCM_MC(my_rank, numnod,                   &
     &      ntot_mc_l, ntot_mc_u, istack_mc_l, istack_mc_u,             &
     &      item_mc_l, item_mc_u, NCOLORtot, IVECmc, IVnew, IW, IFLAG)
!
        call MPI_allREDUCE (IFLAG, IFLAGmax, 1, CALYPSO_INTEGER,        &
     &                    MPI_MAX, CALYPSO_COMM, ierr)
      
        if (IFLAGmax.eq.1) then
          NCOLORtot= NCOLORtot + 1
          if (NCOLORtot.gt.NHYP) NCOLORtot= NHYP
          call reset_4_new_rcm
          goto 999
        endif
!
        call set_RCM_MC_table(numnod, internal_node, NCOLORtot, IVnew,  &
     &          NHYP, OLDtoNEWmc, NEWtoOLDmc)
!
      end if
!
      call deallocate_work_4_RCM
      call deallocate_iW_ordering
      call deallocate_IVECT_rcm
!
!        do i = 1, numnod
!          write(60+my_rank,*) 'istack_mc_l', i, istack_mc_l(i)
!          write(60+my_rank,'(10i8)')                                   &
!     &             item_mc_l(istack_mc_l(i-1)+1:istack_mc_l(i))
!        end do
!        do i = 1, numnod
!          write(60+my_rank,*) 'istack_mc_u', i, istack_mc_u(i)
!          write(60+my_rank,'(10i8)')                                   &
!     &             item_mc_u(istack_mc_u(i-1)+1:istack_mc_u(i))
!        end do
!
      end subroutine  count_rcm
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_color_tbl_RCM_MC(NP, NHYP, NCOLORtot, IVECT_rcm,   &
     &          IVECmc, ICHK, IVnew, IW)
!
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: NHYP
      integer(kind = kint), intent(in) :: NCOLORtot
      integer(kind = kint), intent(in) :: IVECT_rcm(0:NP)
!
      integer(kind = kint), intent(inout) :: IVECmc(0:NCOLORtot)
      integer(kind = kint), intent(inout) :: ICHK(NHYP)
      integer(kind = kint), intent(inout) :: IVnew(NP)
      integer(kind = kint), intent(inout) :: IW(NP)
!
      integer(kind = kint) :: i, k, ic1, ic2
      integer(kind = kint) :: icou, icolt
      integer(kind = kint) :: MCtot, icoug
!
!
      IW(1:NP) = 0
!
      do i= 1, NCOLORtot
        ICHK =  0
        icou =  i - NCOLORtot
        icolt = 0
        do k= 1, NHYP
          icou= icou + NCOLORtot
          if (icou.gt.NHYP) exit
          IW(i)= IW(i) + IVECT_rcm(icou)-IVECT_rcm(icou-1)
          icolt= icolt + 1
          ICHK(icolt)= icou
        enddo

        MCtot= icolt
        icoug= 0
        do ic1= 1, MCtot
          ic2= ICHK(ic1)
          do k= IVECT_rcm(ic2-1)+1, IVECT_rcm(ic2)
            icoug = icoug + 1
            IVnew(icoug+IVECmc(i-1))= k
          enddo
        enddo
        IVECmc(i)= IVECmc(i-1) + icoug
      enddo
!
      end subroutine set_color_tbl_RCM_MC
!
! ----------------------------------------------------------------------
!
      subroutine check_dependency_RCM_MC(my_rank, NP,                   &
     &          NPL_mc, NPU_mc, INL_mc, INU_mc, IAL_mc, IAU_mc,         &
     &          NCOLORtot, IVECmc, IVnew, IW, IFLAG)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: NPL_mc, NPU_mc
      integer(kind = kint), intent(in) :: INL_mc(0:NP)
      integer(kind = kint), intent(in) :: INU_mc(0:NP)
      integer(kind = kint), intent(in) :: IAL_mc(NPL_mc)
      integer(kind = kint), intent(in) :: IAU_mc(NPU_mc)
      integer(kind = kint), intent(in) :: NCOLORtot
      integer(kind = kint), intent(in) :: IVECmc(0:NCOLORtot)
      integer(kind = kint), intent(in) :: IVnew(NP)
!
      integer(kind = kint), intent(inout) :: IFLAG
      integer(kind = kint), intent(inout) :: IW(NP)
!
      integer(kind = kint) :: istart, iend, in
      integer(kind = kint) :: kst, ked, k, j, i, ip
!
!C
!C-- CHECK dependency
      IFLAG= 0
      do i= 1, NCOLORtot
        iStart= IVECmc(i-1) + 1
        iEnd=   IVECmc(i)
        IW= 0
        do j= iStart, iEnd
          in= IVnew(j)
          IW(in)= 1
        end do
        do j= iStart, iEnd
          in= IVnew(j)
!
          kst = INL_mc(in-1) + 1
          ked = INL_mc(in)
          do k = kst, ked
            ip= IAL_mc(k)
            if (IW(ip).eq.1) then
!              write(*,*) 'dep. for lower', my_rank, in, k, ip
              IFLAG= 1
              exit
            end if
          enddo
!
          kst = INU_mc(in-1) + 1
          ked = INU_mc(in)
          do k = kst, ked
            ip= IAU_mc(k)
            if (IW(ip).eq.1) then
!              write(*,*) 'dep. for upper', my_rank, in, k, ip
              IFLAG= 1
              exit
            end if
          enddo
          if ( IFLAG .eq. 1) exit
        enddo
        if ( IFLAG .eq. 1) exit
      enddo
!
      end subroutine check_dependency_RCM_MC
!
! ----------------------------------------------------------------------
!
      subroutine set_RCM_MC_table(NP, N, NCOLORtot, IVnew,              &
     &          NHYP, OLDtoNEWmc, NEWtoOLDmc)
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NCOLORtot
      integer(kind = kint), intent(in) :: IVnew(NP)
!
      integer(kind = kint), intent(inout) :: NHYP
      integer(kind = kint), intent(inout) :: OLDtoNEWmc(NP)
      integer(kind = kint), intent(inout) :: NEWtoOLDmc(NP)
!
      integer(kind = kint) :: i, in
!
!
!CDIR NOVECTOR
        do i= 1, N
          in= IVnew(i)
          OLDtoNEWmc(in)= i
        enddo

!CDIR NOVECTOR
        do i= 1, N
          in= OLDtoNEWmc(i)
          NEWtoOLDmc(in)= i
        enddo

!CDIR NODEP
        do i= N+1, NP
          OLDtoNEWmc(i)= i
          NEWtoOLDmc(i)= i
        enddo
!C
!C-- TRANSFER
        NHYP= NCOLORtot
!
      end subroutine set_RCM_MC_table
!
! ----------------------------------------------------------------------
!
      end module ordering_MC_RCM
