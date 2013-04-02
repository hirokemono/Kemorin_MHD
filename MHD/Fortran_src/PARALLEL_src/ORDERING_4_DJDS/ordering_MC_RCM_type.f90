!
!     module ordering_MC_RCM_type
!
!      Written by K. Nakajima in 2001
!        modified by H. Matsui on May. 2002
!        modified by H. Matsui on June. 2006
!        modified by H. Matsui on Jan., 2009
!
!      subroutine count_rcm_type(nod, tbl_crs, djds_tbl)
!
      module ordering_MC_RCM_type
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
      subroutine count_rcm_type(nod, tbl_crs, djds_tbl)
!
      use m_machine_parameter
      use t_geometry_data
      use t_crs_connect
      use t_solver_djds
!
      use m_parallel_var_dof
      use m_iccg_parameter
      use m_matrix_work
      use m_colored_connect
!
      use ordering_MC_RCM
      use MC_Cuthill_McKee
!
      type(node_data), intent(in) :: nod
      type(CRS_matrix_connect), intent(in) :: tbl_crs
!
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
      integer(kind=kint) :: NHYPmax
      integer(kind=kint) :: IFLAG, IFLAGmax
!
      integer(kind = kint) :: i
!
!
      ntot_mc_l = tbl_crs%ntot_crs_l
      ntot_mc_u = tbl_crs%ntot_crs_u
      max_mc_l =  tbl_crs%max_crs_l
      min_mc_l =  tbl_crs%min_crs_l
      max_mc_u =  tbl_crs%max_crs_u
      min_mc_u =  tbl_crs%min_crs_u
!
      call allocate_IVECT_rcm(nod%numnod)
      call allocate_mc_stack(nod%numnod)
      call allocate_mc_connect
!
!
      djds_tbl%NLmax= max_mc_l
      djds_tbl%NUmax= max_mc_u
!
!----------------------------------------------------------
!   skip multi colorling (only for diagonal scaling)
!----------------------------------------------------------
!
      if(iflag_debug.eq.1) write(*,*) 'iflag_ordering', iflag_ordering
!
      call allocate_iW_ordering(nod%numnod)
!
      if (iflag_ordering .eq. 0 ) then
!
        if (iflag_debug.eq.1) write(*,*) 'no_MC'
        call no_MC (nod%numnod,                                         &
     &      tbl_crs%ntot_crs_l,   tbl_crs%ntot_crs_u,                   &
     &      tbl_crs%istack_crs_l, tbl_crs%istack_crs_u,                 &
     &      tbl_crs%item_crs_l,   tbl_crs%item_crs_u,                   &
     &      ntot_mc_l, ntot_mc_u, num_mc_l, num_mc_u,                   &
     &      istack_mc_l, istack_mc_u, item_mc_l, item_mc_u,             &
     &      djds_tbl%NHYP, IVECT_rcm,                                   &
     &      djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW)
!
        NCOLORtot = 1
!
        call allocate_work_4_rcm(nod%numnod, djds_tbl%NHYP)
        IW(1:nod%numnod) = 0
!
!
!CDIR NOVECTOR
        do i= 1, nod%numnod
          djds_tbl%OLDtoNEW(i)= i
          djds_tbl%NEWtoOLD(i)= i
          OLDtoNEWmc(i)= i
          NEWtoOLDmc(i)= i
        enddo
!
        IVECmc(0) = 0
        IVECmc(1) = nod%internal_node
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
          call sRCM (nod%numnod, nod%internal_node,                     &
     &        tbl_crs%ntot_crs_l,   tbl_crs%ntot_crs_u,                 &
     &        tbl_crs%istack_crs_l, tbl_crs%istack_crs_u,               &
     &        tbl_crs%item_crs_l,   tbl_crs%item_crs_u,                 &
     &        ntot_mc_l, ntot_mc_u, num_mc_l, num_mc_u,                 &
     &        istack_mc_l, istack_mc_u, item_mc_l, item_mc_u,           &
     &        djds_tbl%NHYP, IVECT_rcm,                                 &
     &        djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW, IW)
!
!  -------  MC ordering
        else if ( iflag_ordering .eq. 2 ) then
          if (iflag_debug.eq.1) write(*,*) 'sMC'
          call sMC (nod%numnod, nod%internal_node,                      &
     &        tbl_crs%ntot_crs_l,   tbl_crs%ntot_crs_u,                 &
     &        tbl_crs%istack_crs_l, tbl_crs%istack_crs_u,               &
     &        tbl_crs%item_crs_l,   tbl_crs%item_crs_u,                 &
     &        ntot_mc_l, ntot_mc_u, num_mc_l, num_mc_u,                 &
     &        istack_mc_l, istack_mc_u, item_mc_l, item_mc_u,           &
     &        djds_tbl%NHYP, IVECT_rcm,                                 &
     &        djds_tbl%NEWtoOLD, djds_tbl%OLDtoNEW, IW, mc_color)
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
        call MPI_allREDUCE (djds_tbl%NHYP, NHYPmax, 1, MPI_INTEGER,     &
     &                      MPI_MAX, SOLVER_COMM, ierr)

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
        call allocate_work_4_rcm(nod%numnod, djds_tbl%NHYP)
!
        call set_color_tbl_RCM_MC(nod%numnod, djds_tbl%NHYP, NCOLORtot, &
     &      IVECT_rcm, IVECmc, ICHK, IVnew, IW)
!
!C
!C-- CHECK dependency
        call check_dependency_RCM_MC(my_rank, nod%numnod,               &
     &      ntot_mc_l, ntot_mc_u, istack_mc_l, istack_mc_u,             &
     &      item_mc_l, item_mc_u, NCOLORtot, IVECmc, IVnew, IW, IFLAG)
!
        call MPI_allREDUCE (IFLAG, IFLAGmax, 1, MPI_INTEGER, MPI_MAX,   &
     &                    SOLVER_COMM, ierr)
!
        if (IFLAGmax.eq.1) then
          NCOLORtot= NCOLORtot + 1
          if (NCOLORtot.gt.djds_tbl%NHYP) NCOLORtot= djds_tbl%NHYP
          call reset_4_new_rcm
          goto 999
        endif
!
        call set_RCM_MC_table(nod%numnod, nod%internal_node, NCOLORtot, &
     &      IVnew, djds_tbl%NHYP, OLDtoNEWmc, NEWtoOLDmc)
!
      end if
!
      call deallocate_work_4_RCM
      call deallocate_iW_ordering
      call deallocate_IVECT_rcm
!
!        do i = 1, nod%numnod
!          write(60+my_rank,*) 'istack_mc_l', i, istack_mc_l(i)
!          write(60+my_rank,'(10i8)')                                   &
!     &             item_mc_l(istack_mc_l(i-1)+1:istack_mc_l(i))
!        end do
!        do i = 1, nod%numnod
!          write(60+my_rank,*) 'istack_mc_u', i, istack_mc_u(i)
!          write(60+my_rank,'(10i8)')                                   &
!     &             item_mc_u(istack_mc_u(i-1)+1:istack_mc_u(i))
!        end do
!
      end subroutine  count_rcm_type
!
! ----------------------------------------------------------------------
!
      end module ordering_MC_RCM_type
