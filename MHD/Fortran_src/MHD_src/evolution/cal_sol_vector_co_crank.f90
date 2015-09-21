!
!      module cal_sol_vector_co_crank
!
!      Written by H. Matsui on March, 2006
!
!      subroutine cal_sol_velo_co_crank(inter_smp_stack)
!      subroutine cal_sol_vect_p_co_crank(inter_smp_stack)
!      subroutine cal_sol_magne_co_crank(inter_smp_stack)
!      subroutine cal_sol_velo_co_crank_consist(inter_smp_stack)
!      subroutine cal_sol_vect_p_co_crank_consist(inter_smp_stack)
!      subroutine cal_sol_magne_co_crank_consist(inter_smp_stack)
!
!      subroutine cal_sol_magne_insulator(inter_smp_stack)
!
      module cal_sol_vector_co_crank
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
      private :: cal_sol_velo_co_crank_lump, cal_sol_magne_insulate
      private :: cal_sol_vect_co_crank, cal_sol_vect_co_crank_consist
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_co_crank(inter_smp_stack)
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
!
      call cal_sol_velo_co_crank_lump(node1%numnod, inter_smp_stack,    &
     &    nod_fld1%ntot_phys, iphys%i_velo, d_nod)
!
      end subroutine cal_sol_velo_co_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_co_crank(inter_smp_stack)
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vect_co_crank(node1%numnod, inter_smp_stack,         &
     &    nod_fld1%ntot_phys, iphys%i_vecp, d_nod)
!
      end subroutine cal_sol_vect_p_co_crank
!
! ----------------------------------------------------------------------
!
      subroutine cal_sol_magne_co_crank(inter_smp_stack)
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vect_co_crank(node1%numnod, inter_smp_stack,         &
     &    nod_fld1%ntot_phys, iphys%i_magne, d_nod)
!
      end subroutine cal_sol_magne_co_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_co_crank_consist(inter_smp_stack)
!
      use m_physical_property
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vect_co_crank_consist(inter_smp_stack, coef_velo)
!
      end subroutine cal_sol_velo_co_crank_consist
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_co_crank_consist(inter_smp_stack)
!
      use m_physical_property
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vect_co_crank_consist(inter_smp_stack, coef_magne)
!
      end subroutine cal_sol_vect_p_co_crank_consist
!
! ----------------------------------------------------------------------
!
      subroutine cal_sol_magne_co_crank_consist(inter_smp_stack)
!
      use m_physical_property
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      call cal_sol_vect_co_crank_consist(inter_smp_stack, coef_magne)
!
      end subroutine cal_sol_magne_co_crank_consist
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_insulator
!
      use m_geometry_data_MHD
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
!
!
      call cal_sol_magne_insulate(node1%numnod, inter_ins_smp_stack,    &
     &    numnod_insulate, inod_insulate, nod_fld1%ntot_phys,           &
     &    iphys%i_magne, d_nod)
!
      end subroutine cal_sol_magne_insulator
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_co_crank_lump(numnod, inter_smp_stack,    &
     &          ncomp_nod, i_velo, d_nod)
!
      use m_phys_constants
      use m_finite_element_matrix
      use m_t_int_parameter
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_velo
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: iproc, ist, ied, inod, nd, icomp
!
!
!$omp parallel do private(ist,ied,nd,inod,icomp)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd=1, n_vector
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
      subroutine cal_sol_magne_insulate(numnod, inter_smp_stack,        &
     &          nnod_ins, inod_insulate, ncomp_nod, i_magne, d_nod)
!
      use m_phys_constants
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: nnod_ins
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: inod_insulate(nnod_ins)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_magne
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
      subroutine cal_sol_vect_co_crank(numnod, inter_smp_stack,         &
     &          ncomp_nod, i_field, d_nod)
!
      use m_machine_parameter
      use m_finite_element_matrix
      use m_phys_constants
      use m_t_int_parameter
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_field
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: iproc, inod, nd, icomp
       integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(ist,ied,nd,inod,icomp)
      do iproc = 1, np_smp
        ist = inter_smp_stack(iproc-1) + 1
        ied = inter_smp_stack(iproc)
        do nd=1, n_vector
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
     &        (inter_smp_stack, coef_field)
!
      use m_machine_parameter
      use m_finite_element_matrix
      use m_phys_constants
      use m_t_int_parameter
!
      use cal_ff_smp_to_ffs
!
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef_field
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
      if (coef_field .gt. 0.0d0) then
        call cal_ff_smp_2_ff(n_vector, ff_m_smp, ff)
      end if
!
      end subroutine cal_sol_vect_co_crank_consist
!
! -----------------------------------------------------------------------!
      end module cal_sol_vector_co_crank
