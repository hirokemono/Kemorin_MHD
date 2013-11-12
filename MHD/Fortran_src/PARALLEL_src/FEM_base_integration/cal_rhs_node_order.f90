!
!     module cal_rhs_node_order
!
!     Written by H. Matsui on June, 2005
!     Modified by H. Matsui on March, 2009
!
!      subroutine s_set_ff_nl_smp_2_ff(numnod, np_smp, maxnod_4_smp,    &
!     &          inod_smp_stack, node_sort_list_smp, numdir,            &
!     &          ff_smp, ff_nl_smp, ff, ff_nl)
!      subroutine s_cal_ff_smp_2_ff (numnod, np_smp, maxnod_4_smp,      &
!     &          inod_smp_stack, node_sort_list_smp, numdir, ff_smp, ff)
!      subroutine s_cal_ff_smp_2_ml (numnod, np_smp, maxnod_4_smp,      &
!     &          inod_smp_stack, node_sort_list_smp, ml, ml_o, ff_smp)
!
!      subroutine s_cal_ff_smp_2_scalar (numnod, np_smp, maxnod_4_smp,  &
!     &          inod_smp_stack, node_sort_list_smp, scalar, ff_smp, ml)
!      subroutine s_cal_ff_smp_2_vector (numnod, np_smp, maxnod_4_smp,  &
!     &          inod_smp_stack, node_sort_list_smp, vector, ff_smp, ml)
!      subroutine s_cal_ff_smp_2_tensor (numnod, np_smp, maxnod_4_smp,  &
!     &          inod_smp_stack, node_sort_list_smp, tensor,            &
!     &          ff_t_smp, ml)
!
      module cal_rhs_node_order
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
      subroutine s_set_ff_nl_smp_2_ff(numnod, np_smp, maxnod_4_smp,     &
     &          inod_smp_stack, node_sort_list_smp, numdir,             &
     &          ff_smp, ff_nl_smp, ff, ff_nl)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: maxnod_4_smp
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: node_sort_list_smp(numnod,2)
      real (kind=kreal), intent(in) :: ff_smp(maxnod_4_smp,3,np_smp)
      real (kind=kreal), intent(in) :: ff_nl_smp(maxnod_4_smp,3,np_smp)
!
      real(kind = kreal), intent(inout) :: ff(numnod,3)
      real(kind = kreal), intent(inout) :: ff_nl(numnod,3)
!
      integer(kind = kint) :: iproc, nd, inod
      integer(kind = kint) :: inod1, iproc1
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,nd,inod,inod1,iproc1)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1) + 1
        ied   = inod_smp_stack(iproc)
        do nd = 1, numdir
!cdir nodep
          do inod = ist, ied
            inod1 = node_sort_list_smp(inod,1)
            iproc1 = node_sort_list_smp(inod,2)
            ff(inod,nd) = ff_smp(inod1,nd,iproc1)
            ff_nl(inod,nd) = ff_nl_smp(inod1,nd,iproc1)
          end do
        end do
      end do
!$omp end parallel do
!
!      write(50,*) 'inod, iproc1_smp, inod_smp'
!      do iproc = 1, np_smp
!        ist = inod_smp_stack(iproc-1) + 1
!        ied   = inod_smp_stack(iproc)
!        do nd = 1, numdir
!          do inod = ist, ied
!            inod1 = node_sort_list_smp(inod,1)
!            iproc1 = node_sort_list_smp(inod,2)
!            write(50,*) inod, iproc1, inod1
!          end do
!        end do
!      end do
!
      end subroutine s_set_ff_nl_smp_2_ff
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_ff_smp_2_ff (numnod, np_smp, maxnod_4_smp,       &
     &          inod_smp_stack, node_sort_list_smp, numdir, ff_smp, ff)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: maxnod_4_smp
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: node_sort_list_smp(numnod,2)
!
      real(kind = kreal), intent(inout) :: ff(numnod,3)
      real(kind = kreal), intent(in) :: ff_smp(maxnod_4_smp,3,np_smp)
!
      integer(kind = kint) :: nd, iproc, inod, inod1, iproc1
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,nd,inod,inod1,iproc1)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1) + 1
        ied   = inod_smp_stack(iproc)
        do nd = 1, numdir
!cdir nodep
          do inod = ist, ied
            inod1 = node_sort_list_smp(inod,1)
            iproc1 = node_sort_list_smp(inod,2)
            ff(inod,nd) = ff(inod,nd) + ff_smp(inod1,nd,iproc1)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine s_cal_ff_smp_2_ff
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_ff_smp_2_ml (numnod, np_smp, maxnod_4_smp,       &
     &          inod_smp_stack, node_sort_list_smp, ml, ml_o, ff_smp)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: maxnod_4_smp
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: node_sort_list_smp(numnod,2)
!
      real(kind=kreal), intent(in) :: ff_smp(maxnod_4_smp,3,np_smp)
      real(kind=kreal), intent(inout) :: ml(numnod)
      real(kind=kreal), intent(inout) :: ml_o(numnod)
!
!
      integer(kind = kint) :: iproc, iproc1, inod, inod1
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,inod,inod1,iproc1)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1) + 1
        ied   = inod_smp_stack(iproc)
!cdir nodep
        do inod = ist, ied
          inod1 = node_sort_list_smp(inod,1)
          iproc1 = node_sort_list_smp(inod,2)
          ml_o(inod) = ff_smp(inod1,1,iproc1)
          if ( ml_o(inod) .eq. 0.0d0 ) then
            ml(inod) = 0.0d0
          else
            ml(inod) = 1.0d0 / ff_smp(inod1,1,iproc1)
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine s_cal_ff_smp_2_ml
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_ff_smp_2_scalar (numnod, np_smp, maxnod_4_smp,   &
     &          inod_smp_stack, node_sort_list_smp, scalar, ff_smp, ml)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: maxnod_4_smp
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: node_sort_list_smp(numnod,2)
!
      real(kind=kreal), intent(inout) :: scalar(numnod)
      real(kind=kreal), intent(in) :: ml(numnod)
      real(kind=kreal), intent(in) :: ff_smp(maxnod_4_smp,3,np_smp)
!
      integer(kind = kint) :: inod, inod1
      integer(kind = kint) :: iproc, iproc1
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,inod,inod1,iproc1)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1) + 1
        ied   = inod_smp_stack(iproc)
!cdir nodep
        do inod = ist, ied
          inod1 = node_sort_list_smp(inod,1)
          iproc1 = node_sort_list_smp(inod,2)
          scalar(inod) = ff_smp(inod1,1,iproc1) * ml(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine s_cal_ff_smp_2_scalar
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_ff_smp_2_vector (numnod, np_smp, maxnod_4_smp,   &
     &          inod_smp_stack, node_sort_list_smp, vector, ff_smp, ml)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: maxnod_4_smp
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: node_sort_list_smp(numnod,2)
!
      real(kind=kreal), intent(inout) :: vector(numnod,3)
      real(kind=kreal), intent(in) :: ml(numnod)
      real(kind=kreal), intent(in) :: ff_smp(maxnod_4_smp,3,np_smp)
!
      integer(kind = kint) :: inod, inod1
      integer(kind = kint) :: iproc, iproc1
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,inod,inod1,iproc1)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1) + 1
        ied   = inod_smp_stack(iproc)
!cdir nodep
        do inod = ist, ied
          inod1 = node_sort_list_smp(inod,1)
          iproc1 = node_sort_list_smp(inod,2)
          vector(inod,1) = ff_smp(inod1,1,iproc1) * ml(inod)
          vector(inod,2) = ff_smp(inod1,2,iproc1) * ml(inod)
          vector(inod,3) = ff_smp(inod1,3,iproc1) * ml(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine s_cal_ff_smp_2_vector
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_ff_smp_2_tensor (numnod, np_smp, maxnod_4_smp,   &
     &          inod_smp_stack, node_sort_list_smp, tensor,             &
     &          ff_t_smp, ml)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: maxnod_4_smp
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: node_sort_list_smp(numnod,2)
      real(kind=kreal), intent(in) :: ml(numnod)
      real(kind=kreal), intent(in) :: ff_t_smp(maxnod_4_smp,6,np_smp)
!
      real(kind=kreal), intent(inout) :: tensor(numnod,6)
!
      integer(kind = kint) :: inod, inod1
      integer(kind = kint) :: iproc, iproc1
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,inod,inod1,iproc1)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1) + 1
        ied   = inod_smp_stack(iproc)
!cdir nodep
        do inod = ist, ied
          inod1 = node_sort_list_smp(inod,1)
          iproc1 = node_sort_list_smp(inod,2)
          tensor(inod,1) = ff_t_smp(inod1,1,iproc1) * ml(inod)
          tensor(inod,2) = ff_t_smp(inod1,2,iproc1) * ml(inod)
          tensor(inod,3) = ff_t_smp(inod1,3,iproc1) * ml(inod)
          tensor(inod,4) = ff_t_smp(inod1,4,iproc1) * ml(inod)
          tensor(inod,5) = ff_t_smp(inod1,5,iproc1) * ml(inod)
          tensor(inod,6) = ff_t_smp(inod1,6,iproc1) * ml(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine s_cal_ff_smp_2_tensor
!
! ----------------------------------------------------------------------
!
      end module cal_rhs_node_order
