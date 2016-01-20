!
!      module cal_sol_field_explicit
!
!      Written by H. Matsui on March, 2006
!
!!      subroutine cal_sol_vect_pre_fluid_euler(numnod, inter_smp_stack,&
!!     &          ml_fl, ff, ff_nl, ncomp_nod, numdir, i_field, d_nod)
!!      subroutine cal_sol_vect_pre_conduct_euler                       &
!!     &         (numnod, inter_smp_stack, nnod_cd, inod_conduct,       &
!!     &          ml_cd, ff, ff_nl, ncomp_nod, numdir, i_field, d_nod)
!!
!!      subroutine cal_sol_vect_pre_fluid_adams                         &
!!     &         (numnod, inter_smp_stack, ml_fl, ff, ff_nl,            &
!!     &          ncomp_nod, numdir, i_field, if_pre, d_nod)
!!      subroutine cal_sol_vect_pre_conduct_adams                       &
!!     &         (numnod, inter_smp_stack, nnod_cd, inod_conduct,       &
!!     &          ml_cd, ff, ff_nl, ncomp_nod, numdir,                  &
!!     &          i_field, if_pre, d_nod)
!!
!!      subroutine cal_sol_vec_fluid_linear                             &
!!     &         (numnod, inod_smp_stack, ml_o_fl, ff_nl,               &
!!     &          ncomp_nod, numdir, i_field, if_pre, d_nod, ff)
!!      subroutine cal_sol_vec_conduct_linear(numnod, inter_smp_stack,  &
!!     &          inter_cd_smp_stack, nnod_cd, inod_conduct, ml_o_cd,   &
!!     &          ff_nl, ncomp_nod, numdir, i_field, if_pre, d_nod, ff)
!!
!!      subroutine cal_sol_vec_pre_consist                              &
!!     &          (numnod, inter_smp_stack, ff_nl,                      &
!!     &           ncomp_nod, numdir, if_pre, d_nod, ff)
!
      module cal_sol_field_explicit
!
      use m_precision
!
      use m_machine_parameter
      use m_t_int_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_pre_fluid_euler(numnod, inter_smp_stack,  &
     &          ml_fl, ff, ff_nl, ncomp_nod, numdir, i_field, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: ml_fl(numnod)
      real(kind = kreal), intent(in) :: ff(numnod,3)
      real(kind = kreal), intent(in) :: ff_nl(numnod,3)
!
      integer (kind = kint), intent(in) :: numdir, i_field
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: iproc, inod, nd, icomp, ist, ied
!
!
!$omp parallel do private(nd,icomp,inod,ist,ied)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd=1, numdir
          icomp = i_field + nd - 1
          do inod = ist, ied
             d_nod(inod,icomp) = d_nod(inod,icomp)                      &
     &            + ml_fl(inod)*(ff(inod,nd) + ff_nl(inod,nd) )*dt
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vect_pre_fluid_euler
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_pre_conduct_euler                         &
     &         (numnod, inter_smp_stack, nnod_cd, inod_conduct,         &
     &          ml_cd, ff, ff_nl, ncomp_nod, numdir, i_field, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, nnod_cd, ncomp_nod
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: inod_conduct(nnod_cd)
      real(kind = kreal), intent(in) :: ml_cd(numnod)
      real(kind = kreal), intent(in) :: ff(numnod,3)
      real(kind = kreal), intent(in) :: ff_nl(numnod,3)
!
      integer (kind = kint), intent(in) :: numdir, i_field
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: iproc, inum, inod, nd, icomp
      integer (kind = kint) :: ist, ied
!
!
!$omp parallel do private(nd,icomp,inum,inod)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd=1, numdir
          icomp = i_field + nd - 1
!cdir nodep
          do inum = ist, ied
            inod = inod_conduct(inum)
            d_nod(inod,icomp) = d_nod(inod,icomp)                       &
     &           + ml_cd(inod) * ( ff(inod,nd) + ff_nl(inod,nd) ) * dt
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vect_pre_conduct_euler
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_pre_fluid_adams                           &
     &         (numnod, inter_smp_stack, ml_fl, ff, ff_nl,              &
     &          ncomp_nod, numdir, i_field, if_pre, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: ml_fl(numnod)
      real(kind = kreal), intent(in) :: ff(numnod,3)
      real(kind = kreal), intent(in) :: ff_nl(numnod,3)
!
      integer (kind = kint), intent(in) :: numdir, i_field, if_pre
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: iproc, inod, nd, icomp, if_comp
      integer (kind = kint) :: ist, ied
!
!
!$omp parallel do private(nd,icomp,if_comp,inod,ist,ied)
      do iproc = 1, np_smp
        do nd=1, numdir
          icomp = i_field + nd - 1
          if_comp = if_pre + nd - 1
          ist = inter_smp_stack(iproc-1)+1
          ied = inter_smp_stack(iproc)
          do inod = ist, ied
            d_nod(inod,icomp) = d_nod(inod,icomp)                       &
     &       + ml_fl(inod)*adam_0 * ( ff(inod,nd)+ff_nl(inod,nd) ) * dt &
     &       + ml_fl(inod)*adam_1 * d_nod(inod,if_comp) * dt
            d_nod(inod,if_comp) = ff(inod,nd) + ff_nl(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vect_pre_fluid_adams
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_pre_conduct_adams                         &
     &         (numnod, inter_smp_stack, nnod_cd, inod_conduct,         &
     &          ml_cd, ff, ff_nl, ncomp_nod, numdir,                    &
     &          i_field, if_pre, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, nnod_cd, ncomp_nod
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: inod_conduct(nnod_cd)
      real(kind = kreal), intent(in) :: ml_cd(numnod)
      real(kind = kreal), intent(in) :: ff(numnod,3)
      real(kind = kreal), intent(in) :: ff_nl(numnod,3)
!
      integer (kind = kint), intent(in) :: numdir, i_field, if_pre
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: nd, icomp, if_comp
      integer (kind = kint) :: ist, ied
      integer (kind = kint) :: iproc, inod, inum
!
!
!$omp parallel do private(nd,ist,ied,icomp,if_comp,inum,inod)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd = 1, numdir
          icomp = i_field + nd - 1
          if_comp = if_pre + nd - 1
!cdir nodep
          do inum = ist, ied
            inod = inod_conduct(inum)
!
            d_nod(inod,icomp) = d_nod(inod,icomp)                       &
     &       + adam_0 * ml_cd(inod) * (ff(inod,nd)+ff_nl(inod,nd)) * dt &
     &       + adam_1 * ml_cd(inod) * d_nod(inod,if_comp) * dt
            d_nod(inod,if_comp) = ff(inod,nd) + ff_nl(inod,nd)
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vect_pre_conduct_adams
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------
!
      subroutine cal_sol_vec_fluid_linear                               &
     &         (numnod, inod_smp_stack, ml_o_fl, ff_nl,                 &
     &          ncomp_nod, numdir, i_field, if_pre, d_nod, ff)
!
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: ml_o_fl(numnod)
      real(kind = kreal), intent(in) :: ff_nl(numnod,3)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: numdir, i_field, if_pre
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
      real(kind = kreal), intent(inout) :: ff(numnod,3)
!
!
      integer (kind = kint) :: iproc, inod, nd
      integer (kind = kint) :: icomp, if_comp
      integer (kind = kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,nd,icomp,if_comp,inod)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
        do nd = 1, numdir
          icomp = i_field + nd - 1
          if_comp = if_pre + nd - 1
!cdir nodep
          do inod = ist, ied
            ff(inod,nd) = d_nod(inod,icomp) * ml_o_fl(inod)             &
     &                   + ( ff(inod,nd)                                &
     &                   + adam_0*ff_nl(inod,nd)                        &
     &                   + adam_1* d_nod(inod,if_comp) ) * dt
            d_nod(inod,if_comp) = ff_nl(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
!
      end subroutine cal_sol_vec_fluid_linear
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vec_conduct_linear(numnod, inter_smp_stack,    &
     &          inter_cd_smp_stack, nnod_cd, inod_conduct, ml_o_cd,     &
     &          ff_nl, ncomp_nod, numdir, i_field, if_pre, d_nod, ff)
!
      integer (kind = kint), intent(in) :: numnod, nnod_cd, ncomp_nod
      integer (kind = kint), intent(in) :: inod_conduct(nnod_cd)
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: inter_cd_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: ml_o_cd(numnod)
      real(kind = kreal), intent(in) :: ff_nl(numnod,3)
!
      integer (kind = kint), intent(in) :: numdir, i_field, if_pre
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
      real(kind = kreal), intent(inout) :: ff(numnod,3)
!
      integer (kind = kint) :: iproc, inum, inod, nd
      integer (kind = kint) :: icomp, if_comp
      integer (kind = kint) :: ist, ied
!
!
!$omp parallel do private(nd,ist,ied,inod)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd=1, numdir
!cdir nodep
          do inod = ist, ied
            ff(inod,nd) = ff(inod,nd) * dt
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(nd,ist,ied,inum,inod,icomp,if_comp)
      do iproc = 1, np_smp
        ist = inter_cd_smp_stack(iproc-1) + 1
        ied = inter_cd_smp_stack(iproc)
        do nd=1, numdir
          icomp = i_field + nd - 1
          if_comp = if_pre + nd - 1
!cdir nodep
          do inum = ist, ied
            inod = inod_conduct(inum)
!
            ff(inod,nd) = d_nod(inod,icomp) * ml_o_cd(inod)             &
     &                   + ff(inod,nd)                                  &
     &                   + ( adam_0 * ff_nl(inod,nd)                    &
     &                    + adam_1 * d_nod(inod,if_comp) ) * dt
            d_nod(inod,if_comp) = ff_nl(inod,nd) 
          end do
        end do
      end do
!$omp end parallel do
!
!
      end subroutine cal_sol_vec_conduct_linear
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vec_pre_consist                                &
     &          (numnod, inter_smp_stack, ff_nl,                        &
     &           ncomp_nod, numdir, if_pre, d_nod, ff)
!
      integer(kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: numdir, if_pre
      real(kind = kreal), intent(in) :: ff_nl(numnod,3)
      real(kind = kreal), intent(inout) :: ff(numnod,3)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: iproc, inod, nd
      integer (kind = kint) :: if_comp
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,nd,inod,if_comp)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd=1, numdir
          if_comp = if_pre + nd - 1
!cdir nodep
          do inod = ist, ied
            ff(inod,nd) = ( ff(inod,nd)                                 &
     &                   + adam_0 * ff_nl(inod,nd)                      &
     &                   + adam_1 * d_nod(inod,if_comp) ) * dt
            d_nod(inod,if_comp) = ff_nl(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vec_pre_consist
!
! -----------------------------------------------------------------------
!
      end module cal_sol_field_explicit
