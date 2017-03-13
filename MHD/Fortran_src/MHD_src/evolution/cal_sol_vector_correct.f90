!
!      module cal_sol_vector_correct
!
!      Written by H. Matsui on March, 2006
!
!!      subroutine cal_sol_velocity_co(numnod, inter_smp_stack,         &
!!     &          ml_fl, ff, ncomp_nod, i_velo, i_p_phi, d_nod)
!!      subroutine cal_sol_vector_co(numnod, inter_smp_stack, ml, ff,   &
!!     &          ncomp_nod, i_field, d_nod)
!!
!!      subroutine cal_sol_velo_co_crank_lump                           &
!!     &         (dt, numnod, inter_smp_stack,                          &
!!     &          ml_o_fl, ncomp_nod, i_velo, d_nod, ff_nl, ff)
!!      subroutine cal_sol_magne_insulate                               &
!!     &         (numnod, inter_smp_stack, nnod_ins, inod_insulate, ff, &
!!     &          ncomp_nod, i_magne, d_nod)
!!      subroutine cal_sol_vect_co_crank(dt, numnod, inter_smp_stack,   &
!!     &          ml_o, ncomp_nod, i_field, d_nod, ff_nl, ff)
!!      subroutine cal_sol_vect_co_crank_consist                        &
!!     &         (dt, numnod, inter_smp_stack, ff_nl, ff)
!
      module cal_sol_vector_correct
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------!
      subroutine cal_sol_velocity_co(numnod, inter_smp_stack,           &
     &          ml_fl, ff, ncomp_nod, i_velo, i_p_phi, d_nod)
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: i_velo, i_p_phi
      real(kind = kreal), intent(in) :: ml_fl(numnod)
      real(kind = kreal), intent(in) :: ff(numnod,n_vector)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: iproc, inod, nd, icomp, ist, ied
!
!
!$omp parallel do private(nd,inod,icomp,ist,ied)
      do iproc = 1, np_smp
        do nd = 1, n_vector
          icomp = i_velo + nd - 1
          ist = inter_smp_stack(iproc-1)+1
          ied = inter_smp_stack(iproc)
          do inod = ist, ied
            d_nod(inod,icomp)  = d_nod(inod,icomp)                      &
     &                          + ml_fl(inod)*ff(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1)+1
        ied = inter_smp_stack(iproc)
        do inod = ist, ied
          d_nod(inod,i_p_phi) = 0.0d0
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_velocity_co
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vector_co(numnod, inter_smp_stack, ml, ff,     &
     &          ncomp_nod, i_field, d_nod)
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_field
      real(kind = kreal), intent(in) :: ml(numnod)
      real(kind = kreal), intent(in) :: ff(numnod,n_vector)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: iproc, inod, nd, icomp, ist, ied
!
!
!$omp parallel do private(nd,inod,icomp,ist,ied)
      do iproc = 1, np_smp
        do nd = 1, n_vector
          icomp = i_field + nd - 1
          ist = inter_smp_stack(iproc-1)+1
          ied = inter_smp_stack(iproc)
          do inod = ist, ied
            d_nod(inod,icomp) = d_nod(inod,icomp)                       &
     &                         + ml(inod)*ff(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vector_co
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_co_crank_lump                             &
     &         (dt, numnod, inter_smp_stack,                            &
     &          ml_o_fl, ncomp_nod, i_velo, d_nod, ff_nl, ff)
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_velo
      real(kind = kreal), intent(in) :: ml_o_fl(numnod)
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
      real(kind = kreal), intent(in) :: dt
!
      real(kind = kreal), intent(in) :: ff_nl(numnod,n_vector)
      real(kind = kreal), intent(inout) :: ff(numnod,n_vector)
!
      integer (kind = kint) :: iproc, ist, ied, inod, nd, icomp
!
!
!$omp parallel do private(ist,ied,nd,inod,icomp)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd = 1, n_vector
          icomp = i_velo + nd - 1
!cdir nodep
          do inod = ist, ied
            ff(inod,nd) = d_nod(inod,icomp) * ml_o_fl(inod)             &
     &                   + (-ff(inod,nd) * dt + ff_nl(inod,nd) )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_velo_co_crank_lump
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_insulate                                 &
     &         (numnod, inter_smp_stack, nnod_ins, inod_insulate, ff,   &
     &          ncomp_nod, i_magne, d_nod)
!
      integer (kind = kint), intent(in) :: nnod_ins
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: inod_insulate(nnod_ins)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_magne
!
      real(kind = kreal), intent(in) :: ff(numnod,n_vector)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: nd
      integer (kind = kint) :: iproc, inod, inum
      integer (kind = kint) :: ist, ied
!
!
!$omp parallel do private(nd,ist,ied,inum,inod)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1)+1
        ied = inter_smp_stack(iproc)
        do nd=1, n_vector
!cdir nodep
          do inum = ist, ied
            inod = inod_insulate(inum)
!
            d_nod(inod,i_magne+nd-1) = - ff(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_magne_insulate
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_co_crank(dt, numnod, inter_smp_stack,     &
     &          ml_o, ncomp_nod, i_field, d_nod, ff_nl, ff)
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_field
      real(kind = kreal), intent(in) :: ml_o(numnod)
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
      real(kind = kreal), intent(in) :: ff_nl(numnod,n_vector)
      real(kind = kreal), intent(in) :: dt
!
      real(kind = kreal), intent(inout) :: ff(numnod,n_vector)
!
!
      integer (kind = kint) :: iproc, inod, nd, icomp
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,nd,inod,icomp)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd = 1, n_vector
          icomp = i_field + nd - 1
!cdir nodep
          do inod = ist, ied
!
            ff(inod,nd) = d_nod(inod,icomp) * ml_o(inod)                &
     &                   + (-ff(inod,nd) * dt + ff_nl(inod,nd) )
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vect_co_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_sol_vect_co_crank_consist                          &
     &         (dt, numnod, inter_smp_stack, ff_nl, ff)
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: ff_nl(numnod,n_vector)
      real(kind = kreal), intent(inout) :: ff(numnod,n_vector)
      real(kind = kreal), intent(in) :: dt
!
       integer (kind=kint) :: iproc, inod, nd
       integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(nd,ist,ied,inod)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd=1, n_vector
!cdir nodep
          do inod = ist, ied
            ff(inod,nd) = -ff(inod,nd) * dt + ff_nl(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vect_co_crank_consist
!
! -----------------------------------------------------------------------!
      end module cal_sol_vector_correct
